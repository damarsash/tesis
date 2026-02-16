import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.spatial import Voronoi
from pyproj import Transformer
from shapely.geometry import Polygon, Point
from shapely.ops import unary_union
from shapely import wkt
from math import radians, sin, cos, sqrt, atan2
import imageio.v2 as imageio
import os
from collections import deque

# =====================================================
# PARAMETER
# =====================================================
THETA_PLUS  = 1.01
THETA_MINUS = 0.98
MAX_ITER = 30

# =====================================================
# 1. LOAD DATA
# =====================================================
df = pd.read_excel(
    "D:/IPB/TESIS/PENELITIAN/CODE/output/hasil_kmeans_2025-07-28.xlsx"
)
df["205_tm"] = pd.to_datetime(df["205_tm"])
points_lonlat = df[['longitude', 'latitude']].values

centroids = (
    df[['cluster_id', 'centroid_longitude', 'centroid_latitude']]
    .drop_duplicates()
    .sort_values('cluster_id')
)

# =====================================================
# 2. LOAD BOUNDARY
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

boundary_utm = Polygon([
    to_utm.transform(x, y)
    for x, y in boundary_polygon.exterior.coords
])

# =====================================================
# VORONOI HELPER
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

# =====================================================
# HAVERSINE
# =====================================================
def haversine_km(lon1, lat1, lon2, lat2):
    R = 6371
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    dlon, dlat = lon2-lon1, lat2-lat1
    a = sin(dlat/2)**2 + cos(lat1)*cos(lat2)*sin(dlon/2)**2
    return R * 2 * atan2(sqrt(a), sqrt(1-a))

# =====================================================
# ASSIGN POINT → CELL
# =====================================================
def assign_points(df, vor_polygons, points_utm):
    df["voronoi_id"] = -1
    pts = [Point(p) for p in points_utm]

    for i, poly in enumerate(vor_polygons, start=1):
        for idx, p in enumerate(pts):
            if poly.contains(p):
                df.at[idx, "voronoi_id"] = i

    return df[df["voronoi_id"] >= 0]

# =====================================================
# UPDATE CENTROID
# =====================================================
def recompute_centroids(df):
    return np.array([
        [g["longitude"].mean(), g["latitude"].mean()]
        for _, g in df.groupby("voronoi_id")
    ])

# =====================================================
# HITUNG TETANGGA VORONOI
# =====================================================
def compute_voronoi_neighbors(vor_polygons):
    neighbors = {i: set() for i in range(1, len(vor_polygons)+1)}

    for i, poly_i in enumerate(vor_polygons, start=1):
        for j, poly_j in enumerate(vor_polygons, start=1):
            if i == j:
                continue

            # cek adjacency spasial
            if poly_i.touches(poly_j) or poly_i.intersects(poly_j):
                inter = poly_i.intersection(poly_j)
                if not inter.is_empty:
                    neighbors[i].add(j)
                    neighbors[j].add(i)

    # convert set → list
    neighbors = {k: list(v) for k, v in neighbors.items()}
    return neighbors

# =====================================================
# CAPACITY BALANCING
# =====================================================
# =====================================================
# CAPACITY BALANCING BERBASIS TETANGGA VORONOI
# =====================================================
def capacity_balance_neighbors(df, centroids_lonlat, neighbors_dict,
                               max_iter=20, tolerance=0):

    stat = df.groupby("voronoi_id").size().reset_index(name="n_point")
    load = dict(zip(stat["voronoi_id"], stat["n_point"]))

    total = sum(load.values())
    K = len(load)
    target = total / K

    moved_total = 0

    def find_transfer_path(start_cell):
        """
        Cari jalur dari overload ke underload melalui graph tetangga.
        BFS pada graph Voronoi.
        """
        visited = set()
        queue = deque([(start_cell, [start_cell])])

        while queue:
            current, path = queue.popleft()
            visited.add(current)

            # jika menemukan underload → return path
            if load[current] < target - tolerance:
                return path

            for nb in neighbors_dict.get(current, []):
                if nb not in visited:
                    queue.append((nb, path + [nb]))

        return None

    for iteration in range(max_iter):

        moved_iter = 0

        # urutkan dari paling overload
        cells_sorted = sorted(load.keys(), key=lambda c: load[c], reverse=True)

        for cell in cells_sorted:

            if load[cell] <= target + tolerance:
                continue

            df_cell = df[df["voronoi_id"] == cell]

            for idx, row in df_cell.iterrows():

                if load[cell] <= target + tolerance:
                    break

                path = find_transfer_path(cell)

                if path is None or len(path) < 2:
                    continue

                # tujuan akhir adalah underload
                final_target = path[-1]

                # pilih titik paling dekat centroid tujuan akhir
                lon, lat = row["longitude"], row["latitude"]
                c_lon, c_lat = centroids_lonlat[final_target-1]
                dist = haversine_km(lon, lat, c_lon, c_lat)

                # pindahkan langsung ke tujuan akhir
                df.at[idx, "voronoi_id"] = final_target

                load[cell] -= 1
                load[final_target] += 1
                moved_iter += 1

        moved_total += moved_iter

        max_dev = max(abs(load[c] - target) for c in load)

        if moved_iter == 0 or max_dev <= tolerance:
            break

    return df, moved_total, load


# =====================================================
# ITERATIVE VORONOI BALANCING
# =====================================================
centroids_utm = np.array([
    to_utm.transform(x, y)
    for x, y in centroids[['centroid_longitude','centroid_latitude']].values
])

# =====================================================
# SETUP GIF OUTPUT
# =====================================================
frames = []
gif_path = "D:/IPB/TESIS/PENELITIAN/CODE/output/voronoi_balancing_discreate.gif"

# folder sementara untuk frame
temp_dir = "D:/IPB/TESIS/PENELITIAN/CODE/output/voronoi_frames_balance_discreate"
os.makedirs(temp_dir, exist_ok=True)
# =====================================================

for iteration in range(MAX_ITER):

    vor = Voronoi(centroids_utm)
    regions, vertices = voronoi_finite_polygons_2d(vor)

    vor_polygons = [
        Polygon(vertices[r]).intersection(boundary_utm)
        for r in regions
        if not Polygon(vertices[r]).intersection(boundary_utm).is_empty
    ]
    if iteration == 0:
        df = assign_points(df, vor_polygons, points_utm)

    # hitung tetangga voronoi
    neighbors_dict = compute_voronoi_neighbors(vor_polygons)

    centroids_lonlat = recompute_centroids(df)

    df, moved, load_state = capacity_balance_neighbors(
        df,
        centroids_lonlat,
        neighbors_dict,
        max_iter=10,
        tolerance=0
    )
    print(f"Iter {iteration} | moved = {moved} | max_load = {max(load_state.values())} | min_load = {min(load_state.values())}")

    # =====================================================
    # SIMPAN FRAME VISUAL ITERASI
    # =====================================================
    plt.figure(figsize=(8,8))

    bx, by = boundary_polygon.exterior.xy
    plt.plot(bx, by, 'k-', lw=2)

    for i, poly in enumerate(vor_polygons, start=1):
        x, y = poly.exterior.xy
        lon, lat = to_lonlat.transform(x, y)
        plt.plot(lon, lat, color='black')

        c = poly.centroid
        c_lon, c_lat = to_lonlat.transform(c.x, c.y)

        plt.scatter(c_lon, c_lat, s=200, facecolor='white', edgecolor='black')
        plt.text(c_lon, c_lat, str(i), ha='center', va='center', fontsize=8)

    plt.scatter(points_lonlat[:,0], points_lonlat[:,1], c='red', s=5)
    plt.title(f"Iterasi {iteration} | moved = {moved}")
    plt.gca().set_aspect('equal')
    plt.grid(True)

    frame_path = os.path.join(temp_dir, f"frame_{iteration:03d}.png")
    plt.savefig(frame_path, dpi=150)
    plt.close()

    frames.append(imageio.imread(frame_path))
    # =========================================================

    if moved == 0:
        print(f"Konvergen pada iterasi {iteration}")
        break

    centroids_utm = np.array([
        to_utm.transform(lon, lat)
        for lon, lat in centroids_lonlat
    ])

# =====================================================
# EXPORT GIF
# =====================================================
if len(frames) > 0:
    imageio.mimsave(gif_path, frames, duration=1.0)
    print(f"\nGIF tersimpan di: {gif_path}")

# =====================================================
# HITUNG JARAK FINAL
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
# STATISTIK JARAK
# =====================================================
df_stat_jarak = df.groupby("voronoi_id").agg(
    n_points=("seg_km", "count"),
    total_jarak_km=("seg_km", "sum"),
    rata2_jarak_km=("seg_km", "mean"),
    stddev_jarak_km=("seg_km", "std"),
    min_jarak_km=("seg_km", "min"),
    max_jarak_km=("seg_km", "max")
).reset_index()

print("\nSTATISTIK JARAK PER VORONOI\n")
print(df_stat_jarak.round(3))

# =====================================================
# SUMMARY GLOBAL – JARAK
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
# SUMMARY GLOBAL – JUMLAH POINT
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
# CLF FINAL
# =====================================================
df_n_point_per_voronoi = df.groupby("voronoi_id").size().reset_index(name="n_point")
avg = df_n_point_per_voronoi["n_point"].mean()
df_n_point_per_voronoi["CLF"] = (df_n_point_per_voronoi["n_point"] / avg).round(2)

def clf_status(clf):
    if clf > THETA_PLUS:
        return "Overload"
    elif clf < THETA_MINUS:
        return "Underload"
    else:
        return "Balance"

df_n_point_per_voronoi["status_CLF"] = df_n_point_per_voronoi["CLF"].apply(clf_status)

print("\nJUMLAH POINT & CLF SETIAP AREA VORONOI\n")
print(df_n_point_per_voronoi)

# =====================================================
# VISUAL FINAL
# =====================================================
plt.figure(figsize=(10,10))

bx, by = boundary_polygon.exterior.xy
plt.plot(bx, by, 'k-', lw=2)

for i, poly in enumerate(vor_polygons, start=1):
    x, y = poly.exterior.xy
    lon, lat = to_lonlat.transform(x, y)
    plt.plot(lon, lat, color='black')

    c = poly.centroid
    c_lon, c_lat = to_lonlat.transform(c.x, c.y)

    plt.scatter(c_lon, c_lat, s=300, facecolor='white', edgecolor='black')
    plt.text(c_lon, c_lat, str(i),
        fontsize=10,
        fontweight="bold",
        color="black",
        ha="center",
        va="center",
        bbox=dict(
            facecolor="white",
            edgecolor="black",
            boxstyle="circle,pad=0.3",
            alpha=0.8
        ))

plt.scatter(points_lonlat[:,0], points_lonlat[:,1], c='red', s=8)
plt.title("Voronoi Balanced by Capacity")
plt.gca().set_aspect('equal')
plt.grid(True)
plt.show()
