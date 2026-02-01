import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.spatial import Voronoi
from pyproj import Transformer

# =====================================================
# 1. LOAD DATA
# =====================================================
df = pd.read_excel("D:/IPB/TESIS/PENELITIAN/CODE/output/hasil_kmeans_2025-08-07.xlsx")

points_lonlat = df[['longitude', 'latitude']].values

centroids = (
    df[['cluster_id', 'centroid_longitude', 'centroid_latitude']]
    .drop_duplicates()
    .sort_values('cluster_id')
)

# =====================================================
# 2. PROJECTION
# =====================================================
to_utm = Transformer.from_crs("EPSG:4326", "EPSG:32748", always_xy=True)
to_lonlat = Transformer.from_crs("EPSG:32748", "EPSG:4326", always_xy=True)

def project(xy):
    return np.array([to_utm.transform(x, y) for x, y in xy])

points_utm = project(points_lonlat)
centroids_utm = project(
    centroids[['centroid_longitude', 'centroid_latitude']].values
)

# =====================================================
# 3. VORONOI
# =====================================================
vor = Voronoi(centroids_utm)

# =====================================================
# 4. VISUALISASI RIDGE FINITE + INFINITE (FIXED)
# =====================================================
plt.figure(figsize=(10, 10))

CENTER = centroids_utm.mean(axis=0)
RAY_LENGTH = 60000  # meter

for (p1, p2), ridge in zip(vor.ridge_points, vor.ridge_vertices):

    # ===============================
    # RIDGE FINITE
    # ===============================
    if -1 not in ridge:
        v0 = vor.vertices[ridge[0]]
        v1 = vor.vertices[ridge[1]]

        lon0, lat0 = to_lonlat.transform(v0[0], v0[1])
        lon1, lat1 = to_lonlat.transform(v1[0], v1[1])

        plt.plot([lon0, lon1], [lat0, lat1], 'k-', lw=1)

    # ===============================
    # RIDGE INFINITE (FIXED)
    # ===============================
    else:
        finite_vertex_idx = ridge[0] if ridge[1] == -1 else ridge[1]
        finite_vertex = vor.vertices[finite_vertex_idx]

        pA = centroids_utm[p1]
        pB = centroids_utm[p2]

        # direction normal
        tangent = pB - pA
        normal = np.array([-tangent[1], tangent[0]])
        normal /= np.linalg.norm(normal)

        # pastikan arah keluar
        direction = finite_vertex - CENTER
        if np.dot(direction, normal) < 0:
            normal = -normal

        far_point = finite_vertex + normal * RAY_LENGTH

        lon0, lat0 = to_lonlat.transform(finite_vertex[0], finite_vertex[1])
        lon1, lat1 = to_lonlat.transform(far_point[0], far_point[1])

        plt.plot([lon0, lon1], [lat0, lat1], 'k--', lw=1)


# =====================================================
# 5. PLOT POINT & CENTROID
# =====================================================
plt.scatter(
    points_lonlat[:, 0],
    points_lonlat[:, 1],
    c='red',
    s=8,

)

plt.scatter(
    centroids['centroid_longitude'],
    centroids['centroid_latitude'],
    c='blue',
    s=140,
    marker='X',

)

# =====================================================
# 6. FINAL STYLING
# =====================================================
plt.title("Voronoi Awal")
plt.xlabel("Longitude")
plt.ylabel("Latitude")
plt.legend()

# =====================================================
# HARD ZOOM – CILEBUT BOGOR 
# =====================================================
pad = 0.01  # derajat (~1.1 km)
plt.xlim(
    centroids['centroid_longitude'].min() - pad,
    centroids['centroid_longitude'].max() + pad
)
plt.ylim(
    centroids['centroid_latitude'].min() - pad,
    centroids['centroid_latitude'].max() + pad
)

plt.gca().set_aspect('equal', adjustable='box')
plt.grid(True)
plt.tight_layout()
plt.show()
