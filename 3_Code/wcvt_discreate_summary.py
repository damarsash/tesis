import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.spatial import Voronoi
from pyproj import Transformer
from shapely.geometry import Polygon, Point
from shapely.ops import unary_union
from shapely import wkt
from math import radians, sin, cos, sqrt, atan2
import imageio
import os

# =====================================================
# PARAMETER WCVT
# =====================================================
THETA_PLUS  = 1.01
THETA_MINUS = 0.98
MAX_ITER = 100

# folder gif
frames_dir = "wcvt_frames"
os.makedirs(frames_dir, exist_ok=True)
frame_paths = []

# =====================================================
# LOAD DATA
# =====================================================
df = pd.read_excel("D:/IPB/TESIS/PENELITIAN/CODE/output/hasil_kmeans_2025-08-20.xlsx")
df["205_tm"] = pd.to_datetime(df["205_tm"])
points_lonlat = df[['longitude', 'latitude']].values

centroids = (
    df[['cluster_id', 'centroid_longitude', 'centroid_latitude']]
    .drop_duplicates()
    .sort_values('cluster_id')
)

# =====================================================
# LOAD POLYGON
# =====================================================
with open("D:/IPB/TESIS/PENELITIAN/CODE/source/polygon.txt") as f:
    polygons = [wkt.loads(line.strip()) for line in f if line.strip()]

boundary_polygon = unary_union(polygons).convex_hull

# =====================================================
# PROJECTION
# =====================================================
to_utm = Transformer.from_crs("EPSG:4326", "EPSG:32748", always_xy=True)
to_lonlat = Transformer.from_crs("EPSG:32748", "EPSG:4326", always_xy=True)

points_utm = np.array([to_utm.transform(x, y) for x, y in points_lonlat])
points_geom_utm = [Point(p) for p in points_utm]

centroids_utm = np.array([
    to_utm.transform(x, y)
    for x, y in centroids[['centroid_longitude', 'centroid_latitude']].values
])

boundary_utm = Polygon([
    to_utm.transform(x, y)
    for x, y in boundary_polygon.exterior.coords
])

# =====================================================
# VORONOI FINITE
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
# HITUNG CLF
# =====================================================
def hitung_clf(df_assign):
    count = df_assign.groupby("voronoi_id").size().sort_index()
    kapasitas = count.mean()
    clf = count / kapasitas
    return clf, kapasitas

# =====================================================
# ADJACENCY
# =====================================================
def get_voronoi_neighbors(vor):
    neighbors = {i: set() for i in range(len(vor.points))}
    for (p1, p2) in vor.ridge_points:
        neighbors[p1].add(p2)
        neighbors[p2].add(p1)
    return neighbors

# =====================================================
# NEIGHBOR TRANSFER — LOOP SAMPAI CLF VALID
# =====================================================
def neighbor_transfer(df_assign, neighbors, kapasitas):

    counts = df_assign.groupby("voronoi_id").size().to_dict()

    def clf(v):
        return counts.get(v, 0) / kapasitas

    progress = True
    guard = 0

    # loop global sampai semua zona valid atau mentok
    while progress and guard < 10000:
        progress = False
        guard += 1

        # cek semua zona overload
        for vid in sorted(counts.keys()):

            while clf(vid) > THETA_PLUS:

                # cari tetangga yang masih bisa menerima
                kandidat = [
                    n+1 for n in neighbors[vid-1]
                    if clf(n+1) < THETA_MINUS
                ]

                if not kandidat:
                    break  # tidak ada tetangga feasible

                # pilih target paling underload
                target = min(kandidat, key=lambda x: clf(x))

                # ambil 1 titik dari zona overload
                pts = df_assign[df_assign["voronoi_id"] == vid]
                if len(pts) == 0:
                    break

                idx_pindah = pts.index[0]

                # transfer titik
                df_assign.at[idx_pindah, "voronoi_id"] = target

                counts[vid] -= 1
                counts[target] = counts.get(target, 0) + 1

                progress = True

    return df_assign


# =====================================================
# ITERASI WCVT
# =====================================================
centroids_iter = centroids_utm.copy()

for iteration in range(MAX_ITER):

    vor = Voronoi(centroids_iter)
    regions, vertices = voronoi_finite_polygons_2d(vor)

    vor_polygons = [
        Polygon(vertices[r]).intersection(boundary_utm)
        for r in regions
        if not Polygon(vertices[r]).intersection(boundary_utm).is_empty
    ]

    df["voronoi_id"] = -1
    for i, poly in enumerate(vor_polygons, start=1):
        for idx, p in enumerate(points_geom_utm):
            if poly.contains(p):
                df.at[idx, "voronoi_id"] = i

    df_iter = df[df["voronoi_id"] >= 0].copy()

    clf, kapasitas = hitung_clf(df_iter)
    neighbors = get_voronoi_neighbors(vor)

    df_iter = neighbor_transfer(df_iter, neighbors, kapasitas)
    clf, kapasitas = hitung_clf(df_iter)

    print(f"Iterasi {iteration+1} | Range CLF = {clf.max()-clf.min():.4f}")

    new_centroids = []
    for i in range(len(vor_polygons)):
        pts = df_iter[df_iter["voronoi_id"] == i+1]
        if len(pts)==0:
            new_centroids.append(centroids_iter[i])
            continue

        pts_utm = np.array([
            to_utm.transform(x,y)
            for x,y in pts[["longitude","latitude"]].values
        ])
        new_centroids.append(pts_utm.mean(axis=0))

    centroids_iter = np.array(new_centroids)

    # simpan frame
    fig, ax = plt.subplots(figsize=(8,8))
    x_b, y_b = boundary_utm.exterior.xy
    ax.plot(x_b, y_b)

    for poly in vor_polygons:
        x, y = poly.exterior.xy
        ax.plot(x, y)

    pts = np.array(points_utm)
    ax.scatter(pts[:,0], pts[:,1], s=5)
    ax.scatter(centroids_iter[:,0], centroids_iter[:,1], marker="x", s=80)
    ax.set_aspect("equal")

    frame_path = f"{frames_dir}/frame_{iteration:03d}.png"
    plt.savefig(frame_path, dpi=120)
    plt.close()
    frame_paths.append(frame_path)

    if (clf>=THETA_MINUS).all() and (clf<=THETA_PLUS).all():
        print("Semua area dalam ambang CLF")
        break

centroids_utm = centroids_iter

# =====================================================
# GIF
# =====================================================
gif_path = "D:/IPB/TESIS/PENELITIAN/CODE/output/WCVT_discreate_process.gif"
with imageio.get_writer(gif_path, mode="I", duration=1) as writer:
    for fp in frame_paths:
        writer.append_data(imageio.imread(fp))

print("GIF tersimpan:", gif_path)

# =====================================================
# STATISTIK JARAK (ASLI)
# =====================================================
df = df.sort_values(["voronoi_id", "205_tm"])

def haversine_km(lon1, lat1, lon2, lat2):
    R=6371
    lon1,lat1,lon2,lat2=map(radians,[lon1,lat1,lon2,lat2])
    dlon, dlat = lon2-lon1, lat2-lat1
    a=sin(dlat/2)**2+cos(lat1)*cos(lat2)*sin(dlon/2)**2
    return R*2*atan2(sqrt(a),sqrt(1-a))

df["lon_prev"] = df.groupby("voronoi_id")["longitude"].shift(1)
df["lat_prev"] = df.groupby("voronoi_id")["latitude"].shift(1)

df["seg_km"] = df.apply(
    lambda r: haversine_km(r["lon_prev"],r["lat_prev"],r["longitude"],r["latitude"])
    if pd.notna(r["lon_prev"]) else 0, axis=1
)

df_stat_jarak = df.groupby("voronoi_id").agg(
    n_points=("seg_km","count"),
    total_jarak_km=("seg_km","sum"),
    rata2_jarak_km=("seg_km","mean")
).reset_index()

print("\nSTATISTIK JARAK PER VORONOI\n")
print(df_stat_jarak.round(3))

# =====================================================
# CLF FINAL
# =====================================================
df_n_point_per_voronoi = (
    df.groupby("voronoi_id")
    .size()
    .reset_index(name="n_point")
    .sort_values("voronoi_id")
)

kapasitas = df_n_point_per_voronoi["n_point"].mean()

df_n_point_per_voronoi["kapasitas_kurir"] = kapasitas
df_n_point_per_voronoi["CLF"] = (
    df_n_point_per_voronoi["n_point"] / kapasitas
).round(2)

def clf_status(clf):
    if clf > THETA_PLUS:
        return "Overload"
    elif clf < THETA_MINUS:
        return "Underload"
    return "Normal"

df_n_point_per_voronoi["status_CLF"] = (
    df_n_point_per_voronoi["CLF"].apply(clf_status)
)

print("\nJUMLAH POINT & CLF SETIAP AREA VORONOI\n")
print(df_n_point_per_voronoi)
