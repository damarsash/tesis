import numpy as np
import pandas as pd
from scipy.spatial import Voronoi
from pyproj import Transformer
from shapely.geometry import Polygon, Point
from shapely.ops import unary_union
from shapely import wkt
from math import radians, sin, cos, sqrt, atan2
from collections import deque
import time

# =====================================================
# PARAMETER
# =====================================================
THETA_PLUS  = 1.01
THETA_MINUS = 0.98
MAX_ITER = 30

start_time = time.time()
# =====================================================
# 1. LOAD DATA
# =====================================================
running_date = "2025-07-28";
df = pd.read_excel(
    f"D:/IPB/TESIS/PENELITIAN/CODE/output/kmeans/hasil_kmeans_{running_date}.xlsx"
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

def choose_boundary_point(
        df,
        source_cell,
        target_cell,
        vor_polygons):

    source_poly = vor_polygons[source_cell - 1]
    target_poly = vor_polygons[target_cell - 1]

    shared_boundary = source_poly.boundary.intersection(
        target_poly.boundary
    )

    if shared_boundary.is_empty:
        return None

    candidates = []

    df_source = df[df["voronoi_id"] == source_cell]

    for idx, row in df_source.iterrows():

        p = Point(
            to_utm.transform(
                row["longitude"],
                row["latitude"]
            )
        )

        dist = p.distance(shared_boundary)

        candidates.append((idx, dist))

    if len(candidates) == 0:
        return None

    candidates.sort(key=lambda x: x[1])

    return candidates[0][0]

def transfer_along_bfs_path(
        df,
        path,
        load,
        vor_polygons):

    moved = 0

    for i in range(len(path) - 1):

        source = path[i]
        target = path[i + 1]

        idx_move = choose_boundary_point(
            df,
            source,
            target,
            vor_polygons
        )

        if idx_move is None:
            continue

        df.at[idx_move, "voronoi_id"] = target

        load[source] -= 1
        load[target] += 1

        moved += 1

    return moved

# =====================================================
# CAPACITY BALANCING BERBASIS TETANGGA VORONOI
# =====================================================

def capacity_balance_neighbors(
        df,
        neighbors_dict,
        vor_polygons,
        max_iter=20,
        tolerance=0):

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
        iter_start = time.time()
        moved_iter = 0

        # urutkan dari paling overload
        cells_sorted = sorted(load.keys(), key=lambda c: load[c], reverse=True)

        for cell in cells_sorted:

            if load[cell] <= target + tolerance:
                continue

            df_cell = df[df["voronoi_id"] == cell]

            while load[cell] > target + tolerance:

                path = find_transfer_path(cell)

                if path is None:
                    break

                moved_path = transfer_along_bfs_path(
                    df,
                    path,
                    load,
                    vor_polygons
                )

                if moved_path == 0:
                    break

                moved_iter += moved_path

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

    df, moved, load_state = capacity_balance_neighbors(
        df,
        neighbors_dict,
        vor_polygons,
        max_iter=10,
        tolerance=0
    )
    print(f"Iter {iteration} | moved = {moved} | max_load = {max(load_state.values())} | min_load = {min(load_state.values())}")
    

# =====================================================
# VISUAL FINAL
# =====================================================
voronoi_centroids_lonlat = {}

for i, poly in enumerate(vor_polygons, start=1):
    x, y = poly.exterior.xy
    lon, lat = to_lonlat.transform(x, y)

    c = poly.centroid
    c_lon, c_lat = to_lonlat.transform(c.x, c.y)
    voronoi_centroids_lonlat[i] = (c_lon, c_lat)


    df["voronoi_centroid_longitude"] = df["voronoi_id"].map(lambda vid: voronoi_centroids_lonlat.get(vid, (np.nan, np.nan))[0])
    df["voronoi_centroid_latitude"] = df["voronoi_id"].map(lambda vid: voronoi_centroids_lonlat.get(vid, (np.nan, np.nan))[1])

# =====================================================
# JARAK TITIK -> CENTROID VORONOI FINAL
# =====================================================

df["dist_to_centroid_km"] = df.apply(
    lambda r: haversine_km(
        r["longitude"],
        r["latitude"],
        r["voronoi_centroid_longitude"],
        r["voronoi_centroid_latitude"]
    ),
    axis=1
)


# =====================================================
# STATISTIK JARAK KE CENTROID
# =====================================================

df_stat_jarak = (
    df.groupby("voronoi_id")
      .agg(
          n_points=("dist_to_centroid_km", "count"),
          total_jarak_km=("dist_to_centroid_km", "sum"),
          rata2_jarak_km=("dist_to_centroid_km", "mean"),
          stddev_jarak_km=("dist_to_centroid_km", "std"),
          min_jarak_km=("dist_to_centroid_km", "min"),
          max_jarak_km=("dist_to_centroid_km", "max")
      )
      .reset_index()
)

print("\nSTATISTIK JARAK TITIK KE CENTROID VORONOI\n")
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
# EXPORT DATA TITIK KE EXCEL
# =====================================================
export_path = f"D:/IPB/TESIS/PENELITIAN/CODE/output/ccvd/ccvd_voronoi_assignment_final_{running_date}.xlsx"
df_export = df[[
    "resi",                       # pastikan kolom ini memang ada di dataset
    "latitude",
    "longitude",
    "voronoi_id",
    "voronoi_centroid_latitude",
    "voronoi_centroid_longitude"
]].copy()
df_export.to_excel(export_path, index=False)
#print(f"\nData assignment Voronoi berhasil diexport ke:\n{export_path}")

# =====================================================
# EXECUTION TIME
# =====================================================
end_time = time.time()
execution_time = end_time - start_time

print("\n=====================================================")
print(f"TOTAL EXECUTION TIME : {execution_time:.2f} detik")
print("=====================================================")