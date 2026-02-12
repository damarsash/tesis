import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.spatial import Voronoi
from pyproj import Transformer
from shapely.geometry import Polygon, Point
from shapely.ops import unary_union
from shapely import wkt
from math import radians, sin, cos, sqrt, atan2

# =====================================================
# PARAMETER WCVT SHIFT
# =====================================================
MAX_ITER = 15
ALPHA_OVER = 0.08
ALPHA_UNDER = 0.08
THETA_PLUS  = 1.01
THETA_MINUS = 0.98

# =====================================================
# 1. LOAD DATA
# =====================================================
df = pd.read_excel("D:/IPB/TESIS/PENELITIAN/CODE/output/hasil_kmeans_2025-08-20.xlsx")
df["205_tm"] = pd.to_datetime(df["205_tm"])
points_lonlat = df[['longitude', 'latitude']].values

centroids_df = (
    df[['cluster_id', 'centroid_longitude', 'centroid_latitude']]
    .drop_duplicates()
    .sort_values('cluster_id')
)

# =====================================================
# 2. LOAD POLYGON
# =====================================================
with open("D:/IPB/TESIS/PENELITIAN/CODE/source/polygon.txt") as f:
    polygons = [wkt.loads(line.strip()) for line in f if line.strip()]

boundary_polygon = unary_union(polygons).convex_hull

# =====================================================
# 3. PROJECTION
# =====================================================
to_utm = Transformer.from_crs("EPSG:4326", "EPSG:32748", always_xy=True)
to_lonlat = Transformer.from_crs("EPSG:32748", "EPSG:4326", always_xy=True)

points_utm = np.array([to_utm.transform(x, y) for x, y in points_lonlat])
centroids_utm = np.array([
    to_utm.transform(x, y)
    for x, y in centroids_df[['centroid_longitude','centroid_latitude']].values
])

boundary_utm = Polygon([to_utm.transform(x, y) for x, y in boundary_polygon.exterior.coords])

# =====================================================
# VORONOI FINITE
# =====================================================
def voronoi_finite_polygons_2d(vor, radius=1e6):
    new_regions, new_vertices = [], vor.vertices.tolist()
    center = vor.points.mean(axis=0)
    all_ridges = {}

    for (p1,p2),(v1,v2) in zip(vor.ridge_points, vor.ridge_vertices):
        all_ridges.setdefault(p1, []).append((p2,v1,v2))
        all_ridges.setdefault(p2, []).append((p1,v1,v2))

    for p1, region_idx in enumerate(vor.point_region):
        region = vor.regions[region_idx]
        if all(v>=0 for v in region):
            new_regions.append(region)
            continue

        new_region = [v for v in region if v>=0]
        for p2,v1,v2 in all_ridges[p1]:
            if v2<0: v1,v2=v2,v1
            if v1>=0: continue

            t = vor.points[p2] - vor.points[p1]
            t/=np.linalg.norm(t)
            n = np.array([-t[1],t[0]])
            midpoint = vor.points[[p1,p2]].mean(axis=0)
            direction = np.sign(np.dot(midpoint-center,n))*n
            far = vor.vertices[v2] + direction*radius

            new_vertices.append(far.tolist())
            new_region.append(len(new_vertices)-1)

        vs = np.array([new_vertices[v] for v in new_region])
        c = vs.mean(axis=0)
        angles = np.arctan2(vs[:,1]-c[1], vs[:,0]-c[0])
        new_regions.append(np.array(new_region)[np.argsort(angles)].tolist())

    return new_regions, np.array(new_vertices)

# =====================================================
# ASSIGN POINT
# =====================================================
def assign_points(vor_polygons):
    df["voronoi_id"] = -1
    pts_geom = [Point(p) for p in points_utm]

    for i, poly in enumerate(vor_polygons, start=1):
        for idx, p in enumerate(pts_geom):
            if poly.contains(p):
                df.at[idx, "voronoi_id"] = i

    return df[df["voronoi_id"]>=0]

# =====================================================
# HITUNG CLF
# =====================================================
def compute_clf(df_local):
    count = df_local.groupby("voronoi_id").size().reset_index(name="n_point")
    total = count["n_point"].sum()
    k = count.shape[0]
    cap = total/k

    count["kapasitas_kurir"] = cap
    count["CLF"] = (count["n_point"]/cap).round(2)

    def status(v):
        if v>THETA_PLUS: return "Overload"
        elif v<THETA_MINUS: return "Underload"
        else: return "Normal"

    count["status_CLF"] = count["CLF"].apply(status)
    return count

# =====================================================
# UPDATE CENTROID BERDASARKAN CLF
# =====================================================
def shift_centroids(vor_polygons, clf_table, centroids_utm):
    new_centroids = centroids_utm.copy()

    for i, poly in enumerate(vor_polygons, start=1):
        row = clf_table[clf_table["voronoi_id"]==i]
        if row.empty:
            continue

        status = row.iloc[0]["status_CLF"]
        centroid_area = np.array(poly.centroid.coords[0])
        c = new_centroids[i-1]

        direction = c - centroid_area

        if status=="Overload":
            new_centroids[i-1] = c + ALPHA_OVER * direction

        elif status=="Underload":
            new_centroids[i-1] = c - ALPHA_UNDER * direction

    return new_centroids

# =====================================================
# HARD CAPACITY ENFORCEMENT
# =====================================================
def hard_capacity_balancing(df_local, vor_polygons, centroids_utm):

    print("\n=== HARD CAPACITY BALANCING ===")

    counts = df_local.groupby("voronoi_id").size().to_dict()

    N = len(df_local)
    K = len(vor_polygons)

    base_capacity = N // K
    remainder = N % K

    print(f"Total titik = {N}")
    print(f"Jumlah Voronoi = {K}")
    print(f"Kapasitas dasar = {base_capacity}")
    print(f"Sisa paket = {remainder}")

    # target tiap cell
    target = {i+1: base_capacity for i in range(K)}

    # pilih area yang boleh +1 (yang paling underload)
    sorted_cells = sorted(counts.items(), key=lambda x: x[1])
    for i in range(remainder):
        target[sorted_cells[i][0]] += 1

    # -------------------------------------------------
    # transfer titik sampai sesuai target
    # -------------------------------------------------
    points_geom = [Point(p) for p in points_utm]

    changed = True
    while changed:
        changed = False

        for vid in range(1, K+1):

            while counts.get(vid, 0) > target[vid]:

                # ambil titik terjauh dari centroid (paling logis dipindah)
                subset = df_local[df_local["voronoi_id"] == vid]

                c = centroids_utm[vid-1]

                distances = subset.apply(
                    lambda r: np.linalg.norm(
                        np.array(to_utm.transform(r["longitude"], r["latitude"])) - c
                    ),
                    axis=1
                )

                idx_move = distances.idxmax()
                point_geom = points_geom[idx_move]

                # cari tetangga yang masih butuh titik
                best_target = None
                best_dist = np.inf

                for j in range(1, K+1):

                    if counts.get(j, 0) >= target[j]:
                        continue

                    if vor_polygons[j-1].distance(point_geom) < best_dist:
                        best_dist = vor_polygons[j-1].distance(point_geom)
                        best_target = j

                if best_target is None:
                    break

                df_local.at[idx_move, "voronoi_id"] = best_target

                counts[vid] -= 1
                counts[best_target] = counts.get(best_target, 0) + 1

                changed = True

    print("Balancing selesai ✔")
    return df_local



# =====================================================
# ITERASI WCVT
# =====================================================
for it in range(MAX_ITER):

    vor = Voronoi(centroids_utm)
    regions, vertices = voronoi_finite_polygons_2d(vor)

    vor_polygons = [
        Polygon(vertices[r]).intersection(boundary_utm)
        for r in regions
        if not Polygon(vertices[r]).intersection(boundary_utm).is_empty
    ]

    df_assigned = assign_points(vor_polygons)
    df_assigned = hard_capacity_balancing(df_assigned, vor_polygons, centroids_utm)
    clf_table = compute_clf(df_assigned)

    print(f"\nITERASI {it+1}")
    print(clf_table)

    if all(clf_table["status_CLF"]=="Normal"):
        print("Konvergen ✔")
        break

    centroids_utm = shift_centroids(vor_polygons, clf_table, centroids_utm)

# =====================================================
# OUTPUT LAMA TETAP ADA
# =====================================================
print("\nJUMLAH POINT & CLF SETIAP AREA VORONOI\n")
print(clf_table)

# =====================================================
# VISUALISASI FINAL
# =====================================================
plt.figure(figsize=(10,10))

bx, by = boundary_polygon.exterior.xy
plt.plot(bx, by, 'k-', lw=2)

for poly in vor_polygons:
    x, y = poly.exterior.xy
    lon, lat = to_lonlat.transform(x, y)
    plt.fill(lon, lat, alpha=0.4, edgecolor='black')

lon_c, lat_c = to_lonlat.transform(centroids_utm[:,0], centroids_utm[:,1])

plt.scatter(points_lonlat[:,0], points_lonlat[:,1], c='red', s=8)
plt.scatter(lon_c, lat_c, c='blue', s=140, marker='X')

plt.title("Voronoi Final — Weighted Centroid Shift")
plt.gca().set_aspect('equal', adjustable='box')
plt.grid(True)
plt.tight_layout()
plt.show()
