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
# 1. LOAD DATA
# =====================================================
df = pd.read_excel(
    "D:/IPB/TESIS/PENELITIAN/CODE/output/hasil_kmeans_2025-08-06.xlsx"
)

df["205_tm"] = pd.to_datetime(df["205_tm"])

points_lonlat = df[['longitude', 'latitude']].values

centroids = (
    df[['cluster_id', 'centroid_longitude', 'centroid_latitude']]
    .drop_duplicates()
    .sort_values('cluster_id')
)

# =====================================================
# 2. LOAD POLYGON + CONVEX HULL
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
    for x, y in centroids[['centroid_longitude', 'centroid_latitude']].values
])

boundary_utm = Polygon([
    to_utm.transform(x, y)
    for x, y in boundary_polygon.exterior.coords
])

# =====================================================
# 4. VORONOI FINITE
# =====================================================
def voronoi_finite_polygons_2d(vor, radius=1e6):
    new_regions, new_vertices = [], vor.vertices.tolist()
    center = vor.points.mean(axis=0)
    all_ridges = {}

    for (p1, p2), (v1, v2) in zip(vor.ridge_points, vor.ridge_vertices):
        all_ridges.setdefault(p1, []).append((p2, v1, v2))
        all_ridges.setdefault(p2, []).append((p1, v1, v2))

    for p1, region_idx in enumerate(vor.point_region):
        region = vor.regions[region_idx]
        if all(v >= 0 for v in region):
            new_regions.append(region)
            continue

        new_region = [v for v in region if v >= 0]
        for p2, v1, v2 in all_ridges[p1]:
            if v2 < 0:
                v1, v2 = v2, v1
            if v1 >= 0:
                continue

            t = vor.points[p2] - vor.points[p1]
            t /= np.linalg.norm(t)
            n = np.array([-t[1], t[0]])
            midpoint = vor.points[[p1, p2]].mean(axis=0)
            direction = np.sign(np.dot(midpoint - center, n)) * n
            far = vor.vertices[v2] + direction * radius

            new_vertices.append(far.tolist())
            new_region.append(len(new_vertices) - 1)

        vs = np.array([new_vertices[v] for v in new_region])
        c = vs.mean(axis=0)
        angles = np.arctan2(vs[:,1]-c[1], vs[:,0]-c[0])
        new_regions.append(np.array(new_region)[np.argsort(angles)].tolist())

    return new_regions, np.array(new_vertices)

vor = Voronoi(centroids_utm)
regions, vertices = voronoi_finite_polygons_2d(vor)

vor_polygons = [
    Polygon(vertices[r]).intersection(boundary_utm)
    for r in regions
    if not Polygon(vertices[r]).intersection(boundary_utm).is_empty
]

# =====================================================
# 5. HAVERSINE
# =====================================================
def haversine_km(lon1, lat1, lon2, lat2):
    R = 6371
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    dlon, dlat = lon2-lon1, lat2-lat1
    a = sin(dlat/2)**2 + cos(lat1)*cos(lat2)*sin(dlon/2)**2
    return R * 2 * atan2(sqrt(a), sqrt(1-a))

# =====================================================
# 6. ASSIGN POINT → VORONOI CELL
# =====================================================
df["voronoi_id"] = -1
points_geom_utm = [Point(p) for p in points_utm]

for i, poly in enumerate(vor_polygons, start=1):
    for idx, p in enumerate(points_geom_utm):
        if poly.contains(p):
            df.at[idx, "voronoi_id"] = i

df = df[df["voronoi_id"] >= 0]

# =====================================================
# 7. JARAK BERDASARKAN URUTAN WAKTU (SETARA K-MEANS)
# =====================================================
df = df.sort_values(["voronoi_id", "205_tm"])

df["lon_prev"] = df.groupby("voronoi_id")["longitude"].shift(1)
df["lat_prev"] = df.groupby("voronoi_id")["latitude"].shift(1)

df["seg_km"] = df.apply(
    lambda r: haversine_km(
        r["lon_prev"], r["lat_prev"],
        r["longitude"], r["latitude"]
    ) if pd.notna(r["lon_prev"]) else 0,
    axis=1
)

# =====================================================
# 8. STATISTIK JARAK PER VORONOI
# =====================================================
df_stat_jarak = df.groupby("voronoi_id").agg(
    n_points=("seg_km", "count"),
    total_jarak_km=("seg_km", "sum"),
    rata2_jarak_km=("seg_km", "mean"),
    stddev_jarak_km=("seg_km", "std"),
    min_jarak_km=("seg_km", "min"),
    max_jarak_km=("seg_km", "max")
).reset_index()

print("\nSTATISTIK JARAK PER VORONOI (TIME ORDERED)\n")
print(df_stat_jarak.round(3))

# =====================================================
# 9. SUMMARY GLOBAL – JARAK
# =====================================================
df_summary_global = pd.DataFrame([{
    "jumlah_area_voronoi": len(df_stat_jarak),
    "total_point": df_stat_jarak["n_points"].sum(),
    "total_jarak_global_km": df_stat_jarak["total_jarak_km"].sum(),
    "mean_jarak_km": df_stat_jarak["total_jarak_km"].mean(),
    "std_jarak_km": df_stat_jarak["total_jarak_km"].std(),
    "min_jarak_km": df_stat_jarak["total_jarak_km"].min(),
    "max_jarak_km": df_stat_jarak["total_jarak_km"].max()
}])

print("\nSUMMARY GLOBAL – JARAK\n")
print(df_summary_global.round(3))

# =====================================================
# 10. SUMMARY GLOBAL – JUMLAH POINT
# =====================================================
df_summary_point_global = pd.DataFrame([{
    "total_point_global": df_stat_jarak["n_points"].sum(),
    "mean_point_per_cell": df_stat_jarak["n_points"].mean(),
    "std_point_per_cell": df_stat_jarak["n_points"].std(),
    "min_point_per_cell": df_stat_jarak["n_points"].min(),
    "max_point_per_cell": df_stat_jarak["n_points"].max()
}])

print("\nSUMMARY GLOBAL – JUMLAH POINT PER CELL\n")
print(df_summary_point_global.round(2))

# =====================================================
# 11. VISUALISASI (TETAP ADA)
# =====================================================
plt.figure(figsize=(10, 10))

bx, by = boundary_polygon.exterior.xy
plt.plot(bx, by, 'k-', lw=2)

for poly in vor_polygons:
    x, y = poly.exterior.xy
    lon, lat = to_lonlat.transform(x, y)
    plt.fill(lon, lat, alpha=0.4, edgecolor='black')

plt.scatter(
    points_lonlat[:, 0],
    points_lonlat[:, 1],
    c='red', s=8, label='Points'
)

plt.scatter(
    centroids['centroid_longitude'],
    centroids['centroid_latitude'],
    c='blue', s=140, marker='X', label='Centroid'
)

plt.title("Voronoi Clipped by Convex Hull\n(Time-Ordered Haversine Distance)")
plt.xlabel("Longitude")
plt.ylabel("Latitude")
plt.legend()
plt.grid(True)
plt.gca().set_aspect('equal', adjustable='box')
plt.tight_layout()
plt.show()


# =====================================================
# 12. MATRIX KETETANGGAAN (ADJACENCY MATRIX)
# =====================================================
n_cells = len(vor_polygons)

adj_matrix = np.zeros((n_cells, n_cells), dtype=int)

for i in range(n_cells):
    for j in range(i + 1, n_cells):

        # Tetangga jika berbagi edge (intersection length > 0)
        inter = vor_polygons[i].boundary.intersection(
            vor_polygons[j].boundary
        )

        if not inter.is_empty and inter.length > 0:
            adj_matrix[i, j] = 1
            adj_matrix[j, i] = 1

# Convert ke DataFrame agar mudah dibaca
df_adjacency = pd.DataFrame(
    adj_matrix,
    columns=[f"V{j}" for j in range(1, n_cells + 1)],
    index=[f"V{i}" for i in range(1, n_cells + 1)]
)

print("\nMATRIX KETETANGGAAN CELL VORONOI (1 = Bertetangga)\n")
print(df_adjacency)

# =====================================================
# 13. DAFTAR TETANGGA PER VORONOI CELL
# =====================================================
neighbor_list = {}

for i in range(n_cells):
    neighbors = [j + 1 for j in np.where(adj_matrix[i] == 1)[0]]
    neighbor_list[i+1] = neighbors

print("\nDAFTAR TETANGGA SETIAP CELL VORONOI\n")
for k, v in neighbor_list.items():
    print(f"Voronoi {k} bertetangga dengan: {v}")

# =====================================================
# 14. JUMLAH POINT PER AREA VORONOI
# =====================================================
df_n_point_per_voronoi = (
    df.groupby("voronoi_id")
    .size()
    .reset_index(name="n_point")
)

print("\nJUMLAH POINT DI SETIAP AREA VORONOI\n")
print(df_n_point_per_voronoi)

# =====================================================
# 14. JUMLAH POINT + CLF PER AREA VORONOI
# =====================================================
THETA_PLUS  = 1.00   # Θ⁺ (ambang overload)
THETA_MINUS = 0.98   # Θ⁻ (ambang underload)

df_n_point_per_voronoi = (
    df.groupby("voronoi_id")
    .size()
    .reset_index(name="n_point")
    .sort_values("voronoi_id")
)

# -----------------------------------------------------
# Hitung kapasitas maksimum per kurir (A(ci))
# -----------------------------------------------------
total_point = df_n_point_per_voronoi["n_point"].sum()
jumlah_voronoi = df_n_point_per_voronoi.shape[0]

kapasitas_kurir = total_point / jumlah_voronoi

# -----------------------------------------------------
# Hitung CLF
# -----------------------------------------------------
df_n_point_per_voronoi["kapasitas_kurir"] = kapasitas_kurir
df_n_point_per_voronoi["CLF"] = (
    df_n_point_per_voronoi["n_point"] / kapasitas_kurir
)

# -----------------------------------------------------
# Kategori CLF (Underload / Normal / Overload)
# -----------------------------------------------------
def clf_status(clf):
    if clf > THETA_PLUS:
        return "Overload"
    elif clf < THETA_MINUS:
        return "Underload"
    else:
        return "Normal"

df_n_point_per_voronoi["status_CLF"] = (
    df_n_point_per_voronoi["CLF"].apply(clf_status)
)

print("\nJUMLAH POINT & CLF SETIAP AREA VORONOI\n")
print(df_n_point_per_voronoi.round(3))
