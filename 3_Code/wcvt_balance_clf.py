import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.spatial import Voronoi
from pyproj import Transformer
from shapely.geometry import Polygon, Point
from shapely.ops import unary_union
from shapely import wkt
from math import radians, sin, cos, sqrt, atan2
import os
import imageio.v2 as imageio


# =====================================================
# PARAMETER WCVT SHIFT
# =====================================================
MAX_ITER = 50
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
# ITERASI WCVT
# =====================================================
frame_dir = "D:/IPB/TESIS/PENELITIAN/CODE/output/voronoi_frames_balance_clf"
os.makedirs(frame_dir, exist_ok=True)
frames = []

for it in range(MAX_ITER):

    vor = Voronoi(centroids_utm)
    regions, vertices = voronoi_finite_polygons_2d(vor)

    vor_polygons = [
        Polygon(vertices[r]).intersection(boundary_utm)
        for r in regions
        if not Polygon(vertices[r]).intersection(boundary_utm).is_empty
    ]

    df_assigned = assign_points(vor_polygons)
    clf_table = compute_clf(df_assigned)

    print(f"\nITERASI {it+1}")
    print(clf_table)
        # ==========================================
    # SIMPAN FRAME ITERASI
    # ==========================================
    plt.figure(figsize=(8,8))

    bx, by = boundary_polygon.exterior.xy
    plt.plot(bx, by, 'k-', lw=2)

    for poly in vor_polygons:
        x, y = poly.exterior.xy
        lon, lat = to_lonlat.transform(x, y)
        plt.fill(lon, lat, alpha=0.35, edgecolor='black')

    lon_c, lat_c = to_lonlat.transform(
        centroids_utm[:,0], centroids_utm[:,1]
    )

    plt.scatter(points_lonlat[:,0], points_lonlat[:,1], s=6)
    plt.scatter(lon_c, lat_c, s=120, marker='X')

    plt.title(f"Iterasi {it+1}")
    plt.gca().set_aspect('equal', adjustable='box')
    plt.tight_layout()

    frame_path = os.path.join(frame_dir, f"frame_{it:03d}.png")
    plt.savefig(frame_path, dpi=120)
    plt.close()

    frames.append(imageio.imread(frame_path))


    if all(clf_table["status_CLF"]=="Normal"):
        print("Konvergen ✔")
        break

    centroids_utm = shift_centroids(vor_polygons, clf_table, centroids_utm)

# =====================================================
# SIMPAN GIF
# =====================================================
gif_path = "D:/IPB/TESIS/PENELITIAN/CODE/output/voronoi_balance_clf.gif"
imageio.mimsave(gif_path, frames, duration=0.8)

print(f"\nGIF tersimpan di: {gif_path}")

# =====================================================
# OUTPUT LAMA TETAP ADA
# =====================================================
print("\nJUMLAH POINT & CLF SETIAP AREA VORONOI\n")
print(clf_table)

# =====================================================
# HITUNG JARAK PER SEGMEN DALAM SETIAP VORONOI
# =====================================================
df_final = df_assigned.copy()
df_final = df_final.sort_values(["voronoi_id", "205_tm"])

df_final["lon_prev"] = df_final.groupby("voronoi_id")["longitude"].shift(1)
df_final["lat_prev"] = df_final.groupby("voronoi_id")["latitude"].shift(1)

def haversine_km(lon1, lat1, lon2, lat2):
    R = 6371
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    dlon, dlat = lon2-lon1, lat2-lat1
    a = sin(dlat/2)**2 + cos(lat1)*cos(lat2)*sin(dlon/2)**2
    return R * 2 * atan2(sqrt(a), sqrt(1-a))

df_final["seg_km"] = df_final.apply(
    lambda r: haversine_km(
        r["lon_prev"], r["lat_prev"],
        r["longitude"], r["latitude"]
    ) if pd.notna(r["lon_prev"]) else 0,
    axis=1
)

# =====================================================
# STATISTIK JARAK
# =====================================================
df_stat_jarak = df_final.groupby("voronoi_id").agg(
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
