import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.spatial import Voronoi
from shapely.geometry import Polygon, Point
from pyproj import Transformer
from collections import defaultdict

# ===============================
# 1. LOAD DATA
# ===============================
df = pd.read_excel("D:/IPB/TESIS/PENELITIAN/CODE/output/hasil_kmeans_2025-08-20.xlsx")

points = df[['longitude', 'latitude']].values
clusters_init = df['cluster_id'].values
NUM_REGIONS = df['cluster_id'].nunique()
MAX_ITER = 50
TOL = 0.05   # 5% toleransi ketidakrataan

# ===============================
# 2. PROJECTION (WGS84 → UTM)
# ===============================
transformer = Transformer.from_crs(
    "EPSG:4326",
    "EPSG:32748",  # Indonesia Barat
    always_xy=True
)

def project_xy(xy):
    return np.array([transformer.transform(x, y) for x, y in xy])

points_utm = project_xy(points)

# ===============================
# 3. INIT GENERATORS (KMEANS CENTROID)
# ===============================
generators = []
clusters = {}

for k in sorted(df['cluster_id'].unique()):
    pts = points_utm[clusters_init == k]
    clusters[k] = pts.tolist()
    generators.append(pts.mean(axis=0))

generators = np.array(generators)

# ===============================
# 4. DISCRETE NEIGHBOR-CONSTRAINED BALANCING (GABUNGAN)
# ===============================
from collections import deque

total_pkg = len(points)
target = total_pkg // NUM_REGIONS   # diskrit
remainder = total_pkg % NUM_REGIONS

cluster_ids = list(clusters.keys())
cid_to_idx = {cid: i for i, cid in enumerate(cluster_ids)}

for iteration in range(MAX_ITER):

    vor = Voronoi(generators)

    # -----------------------------------
    # Bangun adjacency Voronoi
    # -----------------------------------
    neighbors = defaultdict(set)
    for a, b in vor.ridge_points:
        ca = cluster_ids[a]
        cb = cluster_ids[b]
        neighbors[ca].add(cb)
        neighbors[cb].add(ca)

    sizes = {cid: len(clusters[cid]) for cid in cluster_ids}

    moved = False

    # -----------------------------------
    # Area yang kekurangan paket
    # -----------------------------------
    deficit_areas = [cid for cid in cluster_ids if sizes[cid] < target]

    for area in deficit_areas:

        for nb in neighbors[area]:

            if sizes[nb] > sizes[area]:

                # ===============================
                # Transfer 1 paket terdekat
                # ===============================
                pts_nb = np.array(clusters[nb])
                if len(pts_nb) == 0:
                    continue

                gen_area = generators[cid_to_idx[area]]

                dists = np.linalg.norm(pts_nb - gen_area, axis=1)
                idx = np.argmin(dists)

                p = clusters[nb][idx]

                clusters[nb].remove(p)
                clusters[area].append(p)

                sizes[nb] -= 1
                sizes[area] += 1

                moved = True
                break   # 1 paket per area per iterasi

    # -----------------------------------
    # Update generator
    # -----------------------------------
    for cid in cluster_ids:
        if len(clusters[cid]) > 0:
            generators[cid_to_idx[cid]] = np.mean(clusters[cid], axis=0)

    # -----------------------------------
    # Kriteria berhenti
    # -----------------------------------
    if not moved:
        break

# ===============================
# 5. AREA COMPUTATION (KM²)
# ===============================
bbox = Polygon([
    (points[:,0].min()-0.01, points[:,1].min()-0.01),
    (points[:,0].max()+0.01, points[:,1].min()-0.01),
    (points[:,0].max()+0.01, points[:,1].max()+0.01),
    (points[:,0].min()-0.01, points[:,1].max()+0.01),
])

areas = []

for idx in range(NUM_REGIONS):
    region = vor.regions[vor.point_region[idx]]
    if -1 in region or len(region) == 0:
        areas.append(0)
        continue

    poly = Polygon([vor.vertices[i] for i in region])
    clipped = poly.intersection(Polygon(project_xy(np.array(bbox.exterior.coords))))
    areas.append(clipped.area / 1_000_000)

# ===============================
# 6. DISTANCE METRICS (KM)
# ===============================
distance_stats = []

for cid in clusters:
    pts = np.array(clusters[cid])
    if len(pts) == 0:
        continue

    gen = generators[cid_to_idx[cid]]
    d = np.linalg.norm(pts - gen, axis=1) / 1000
    distance_stats.append(d)
# ===============================
# 7. OUTPUT SUMMARY
# ===============================
pkg_counts = np.array([len(clusters[i]) for i in clusters])

print("\n=== HASIL AKHIR STABIL ===\n")
print(f"Jumlah Area              : {NUM_REGIONS}")
print(f"Total Resi               : {total_pkg}")
print(f"Rata-rata Resi / Area    : {pkg_counts.mean():.2f}")
print(f"Std Dev Resi             : {pkg_counts.std():.2f}")
print(f"Min Resi                 : {pkg_counts.min()}")
print(f"Max Resi                 : {pkg_counts.max()}")

dist_all = np.concatenate(distance_stats)

print(f"\nTotal Jarak (km)         : {dist_all.sum():.2f}")
print(f"Rata-rata Jarak / Area   : {np.mean([d.mean() for d in distance_stats]):.2f}")
print(f"Std Dev Jarak            : {dist_all.std():.2f}")
print(f"Min Jarak                : {dist_all.min():.2f}")
print(f"Max Jarak                : {dist_all.max():.2f}")

# ===============================
# 8. VISUALIZATION
# ===============================
plt.figure(figsize=(8,8))
plt.scatter(points[:,0], points[:,1], c='red', s=20, label='Paket')
plt.scatter(generators[:,0], generators[:,1], c='blue', s=150, marker='X', label='Generator')

for simplex in vor.ridge_vertices:
    if -1 not in simplex:
        v = vor.vertices[simplex]
        plt.plot(v[:,0], v[:,1], 'k-', lw=1)

plt.title("Neighbor-Constrained Voronoi Balancing (Stabil)")
plt.xlabel("Longitude")
plt.ylabel("Latitude")
plt.legend()
plt.grid(True)
plt.show()
