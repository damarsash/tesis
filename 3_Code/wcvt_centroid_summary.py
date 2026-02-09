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

MAX_ITER = 20
LEARNING_RATE_UP = 0.05
LEARNING_RATE_DOWN = 0.05
CONVERGENCE_TOL = 0.02

# =====================================================
# LOAD DATA
# =====================================================
df = pd.read_excel(
    "D:/IPB/TESIS/PENELITIAN/CODE/output/hasil_kmeans_2025-08-20.xlsx"
)

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
# OUTPUT GIF SETUP
# =====================================================
frames_dir = "D:/IPB/TESIS/PENELITIAN/CODE/output/wcvt_frames"
os.makedirs(frames_dir, exist_ok=True)
frame_paths = []

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
# CLF
# =====================================================
def hitung_clf(df_assign):
    count = df_assign.groupby("voronoi_id").size().sort_index()
    kapasitas = count.mean()
    clf = count / kapasitas
    return clf, kapasitas

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

    new_centroids = []

    for i in range(len(vor_polygons)):
        pts = df_iter[df_iter["voronoi_id"] == i+1]

        if len(pts) == 0:
            new_centroids.append(centroids_iter[i])
            continue

        pts_utm = np.array([
            to_utm.transform(x, y)
            for x, y in pts[["longitude", "latitude"]].values
        ])

        centroid = pts_utm.mean(axis=0)

        if clf.iloc[i] > THETA_PLUS:
            target_idx = np.argmin(clf.values)
            direction = centroids_iter[target_idx] - centroid
            centroid = centroid + 0.2 * direction

        new_centroids.append(centroid)

    new_centroids = np.array(new_centroids)

    clf_range = clf.max() - clf.min()
    print(f"Iterasi {iteration+1} | Range CLF = {clf_range:.4f}")

    # ===== SAVE FRAME =====
    fig, ax = plt.subplots(figsize=(8, 8))

    x_b, y_b = boundary_utm.exterior.xy
    ax.plot(x_b, y_b)

    for poly in vor_polygons:
        if not poly.is_empty:
            x, y = poly.exterior.xy
            ax.plot(x, y)

    pts = np.array(points_utm)
    ax.scatter(pts[:, 0], pts[:, 1], s=5)
    ax.scatter(centroids_iter[:, 0], centroids_iter[:, 1], marker="x", s=80)

    ax.set_title(f"WCVT Iteration {iteration+1}")
    ax.set_aspect("equal")

    frame_path = f"{frames_dir}/frame_{iteration:03d}.png"
    plt.savefig(frame_path, dpi=120)
    plt.close()

    frame_paths.append(frame_path)

    if clf_range < CONVERGENCE_TOL:
        print("Konvergen")
        centroids_iter = new_centroids
        break

    centroids_iter = new_centroids

# =====================================================
# BUAT GIF
# =====================================================
gif_path = "D:/IPB/TESIS/PENELITIAN/CODE/output/wcvt_centroid_voronoi.gif"

with imageio.get_writer(gif_path, mode="I", duration=0.8) as writer:
    for path in frame_paths:
        writer.append_data(imageio.imread(path))

print("\nGIF berhasil dibuat:")
print(gif_path)
