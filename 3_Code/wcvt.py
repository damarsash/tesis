import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.spatial import Voronoi
from shapely.geometry import Polygon
from shapely.ops import transform
from pyproj import Transformer
from collections import defaultdict, deque

# =====================================================
# 1. LOAD DATA
# =====================================================
df = pd.read_excel(
    "D:/IPB/TESIS/PENELITIAN/CODE/output/hasil_kmeans_2025-08-20.xlsx"
)

points = df[['longitude', 'latitude']].values
clusters_init = df['cluster_id'].values
cluster_ids = sorted(df['cluster_id'].unique())
NUM_REGIONS = len(cluster_ids)

# =====================================================
# 2. PROJECTION (WGS84 → UTM)
# =====================================================
to_utm = Transformer.from_crs("EPSG:4326", "EPSG:32748", always_xy=True)
to_lonlat = Transformer.from_crs("EPSG:32748", "EPSG:4326", always_xy=True)

def project_xy(xy):
    return np.array([to_utm.transform(x, y) for x, y in xy])

points_utm = project_xy(points)

# =====================================================
# 3. INIT CLUSTERS & CENTROIDS
# =====================================================
clusters = {}
generators = []

for cid in cluster_ids:
    pts = points_utm[clusters_init == cid]
    clusters[cid] = pts.tolist()
    generators.append(pts.mean(axis=0))

generators = np.array(generators)
cid_to_idx = {cid: i for i, cid in enumerate(cluster_ids)}

# =====================================================
# 4. FIXED VORONOI ADJACENCY
# =====================================================
vor_fixed = Voronoi(generators)

neighbors = defaultdict(set)
for a, b in vor_fixed.ridge_points:
    neighbors[cluster_ids[a]].add(cluster_ids[b])
    neighbors[cluster_ids[b]].add(cluster_ids[a])

# =====================================================
# 5. GLOBAL DISCRETE LOAD BALANCING
# =====================================================
total_pkg = len(points)
target = total_pkg // NUM_REGIONS
upper = target + 1
MAX_ITER = 20000

def find_donor(start, sizes):
    visited = {start}
    q = deque([start])
    while q:
        u = q.popleft()
        for v in neighbors[u]:
            if v in visited:
                continue
            if sizes[v] > upper:
                return v
            visited.add(v)
            q.append(v)
    return None

for _ in range(MAX_ITER):
    sizes = {c: len(clusters[c]) for c in cluster_ids}
    moved = False

    for area in cluster_ids:
        if sizes[area] >= target:
            continue

        donor = find_donor(area, sizes)
        if donor is None:
            continue

        pts_d = np.array(clusters[donor])
        if len(pts_d) == 0:
            continue

        gen = generators[cid_to_idx[area]]
        idx = np.argmin(np.linalg.norm(pts_d - gen, axis=1))
        p = clusters[donor][idx]

        clusters[donor].remove(p)
        clusters[area].append(p)
        sizes[donor] -= 1
        sizes[area] += 1
        moved = True

    for cid in cluster_ids:
        if clusters[cid]:
            generators[cid_to_idx[cid]] = np.mean(clusters[cid], axis=0)

    if not moved:
        break

# =====================================================
# 6. VORONOI AREA COMPUTATION (km²)
# =====================================================
bbox_lonlat = Polygon([
    (points[:,0].min()-0.01, points[:,1].min()-0.01),
    (points[:,0].max()+0.01, points[:,1].min()-0.01),
    (points[:,0].max()+0.01, points[:,1].max()+0.01),
    (points[:,0].min()-0.01, points[:,1].max()+0.01),
])

bbox_utm = transform(lambda x,y: to_utm.transform(x,y), bbox_lonlat)

voronoi_areas = {}

for i, cid in enumerate(cluster_ids):
    region = vor_fixed.regions[vor_fixed.point_region[i]]
    if -1 in region or not region:
        voronoi_areas[cid] = 0.0
        continue

    poly = Polygon([vor_fixed.vertices[j] for j in region])
    poly = poly.intersection(bbox_utm)
    voronoi_areas[cid] = poly.area / 1_000_000 if not poly.is_empty else 0.0

# =====================================================
# 7. DISTANCE METRICS (km)
# =====================================================
distance_stats = {}
all_dist = []

for cid in cluster_ids:
    pts = np.array(clusters[cid])
    if len(pts) == 0:
        continue
    gen = generators[cid_to_idx[cid]]
    d = np.linalg.norm(pts - gen, axis=1) / 1000
    distance_stats[cid] = d
    all_dist.extend(d)

all_dist = np.array(all_dist)

# =====================================================
# 8. NORMALIZATION & FAIRNESS
# =====================================================
resi = np.array([len(clusters[c]) for c in cluster_ids])
luas = np.array([voronoi_areas[c] for c in cluster_ids])

resi_norm = (resi - resi.min()) / (resi.max() - resi.min())
luas_norm = (luas - luas.min()) / (luas.max() - luas.min())

fairness = np.abs(resi_norm - luas_norm)
density = np.divide(resi, luas, out=np.zeros_like(resi, dtype=float), where=luas>0)

df_result = pd.DataFrame({
    "cluster_id": cluster_ids,
    "resi": resi,
    "luas_km2": luas,
    "resi_norm": resi_norm,
    "luas_norm": luas_norm,
    "fairness_index": fairness,
    "density_resi_per_km2": density
}).sort_values("fairness_index", ascending=False)

# =====================================================
# 9. OUTPUT SUMMARY
# =====================================================
print("\n=== RINGKASAN AKHIR ===\n")
print(f"Jumlah Area                  : {NUM_REGIONS}")
print(f"Total Resi                   : {total_pkg}")
print(f"Min / Max Resi               : {resi.min()} / {resi.max()}")
print(f"Total Jarak (km)             : {all_dist.sum():.2f}")
print(f"Rata-rata Jarak / Area (km)  : {np.mean([v.mean() for v in distance_stats.values()]):.2f}")
print(f"Std Dev Jarak (km)           : {all_dist.std():.2f}")
print(f"Min / Max Jarak (km)         : {all_dist.min():.2f} / {all_dist.max():.2f}")

print("\n=== FAIRNESS ===")
print(f"Rata-rata Fairness Index     : {fairness.mean():.3f}")
print(f"Fairness Terburuk            : {fairness.max():.3f}")

print("\nTop 5 Area Paling Tidak Seimbang:")
print(df_result.head(5).to_string(index=False))

# =====================================================
# 10. VISUALIZATION (MAP + VORONOI)
# =====================================================
plt.figure(figsize=(10,10))

for i, cid in enumerate(cluster_ids):
    region = vor_fixed.regions[vor_fixed.point_region[i]]
    if -1 in region or not region:
        continue
    poly = Polygon([vor_fixed.vertices[j] for j in region])
    poly = poly.intersection(bbox_utm)
    if poly.is_empty:
        continue
    poly_ll = transform(lambda x,y: to_lonlat.transform(x,y), poly)
    x,y = poly_ll.exterior.xy
    plt.fill(x,y,alpha=0.3,edgecolor='black')

plt.scatter(points[:,0], points[:,1], s=6, c='red', label='Paket')
gens_ll = np.array([to_lonlat.transform(g[0], g[1]) for g in generators])
plt.scatter(gens_ll[:,0], gens_ll[:,1], s=120, c='blue', marker='X', label='Centroid')

plt.title("Voronoi Area Seimbang + Distribusi Paket")
plt.xlabel("Longitude")
plt.ylabel("Latitude")
plt.legend()
plt.axis("equal")
plt.grid(True)
plt.tight_layout()
plt.show()
