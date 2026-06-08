import pandas as pd
import numpy as np
import folium
from folium.features import DivIcon
import os
import pickle
import hashlib
from math import radians, sin, cos, sqrt, atan2

# =========================
# PARAMETER GLOBAL
# =========================
running_date = "2025-08-20"
FILE_PATH = f"D:/IPB/TESIS/PENELITIAN/CODE/output/ccvd/ccvd_voronoi_assignment_final_{running_date}.xlsx"

START_LAT = -6.535158
START_LON = 106.799133

AVG_SPEED_KMH = 30
SERVICE_TIME_MIN = 2

N_ANTS = 37
N_ITER = 100
ALPHA = 1
BETA = 9
EVAPORATION = 0.65
INITIAL_PHEROMONE = 0.2

Q = 100
MAP_VORONOI_ID = 5  # pilih area yang ingin divisualisasikan

results = []
route_rows = []   # urutan kirim per resi
summary_rows = [] # ringkasan per kurir
all_distances = []
all_points = []
all_times = []
all_vids = []
# =========================
# CACHE CONFIG
# =========================
CACHE_DIR = "D:/IPB/TESIS/PENELITIAN/CODE/output/aco/cache"
os.makedirs(CACHE_DIR, exist_ok=True)

def hash_coords(coords):
    key = str(coords).encode()
    return hashlib.md5(key).hexdigest()

def cache_path(name, key):
    return os.path.join(CACHE_DIR, f"{name}_{key}.pkl")

def load_cache(name, key):
    path = cache_path(name, key)
    if os.path.exists(path):
        with open(path, "rb") as f:
            return pickle.load(f)
    return None

def save_cache(name, key, data):
    path = cache_path(name, key)
    with open(path, "wb") as f:
        pickle.dump(data, f)

# =========================
# HAVERSINE DISTANCE
# =========================
def haversine(lat1, lon1, lat2, lon2):
    R = 6371
    dlat = radians(lat2 - lat1)
    dlon = radians(lon2 - lon1)

    a = sin(dlat/2)**2 + cos(radians(lat1)) * cos(radians(lat2)) * sin(dlon/2)**2
    c = 2 * atan2(sqrt(a), sqrt(1-a))
    return R * c  # km

# =========================
# DISTANCE MATRIX
# =========================
def build_distance_matrix(coords):
    key = hash_coords(coords)
    cached = load_cache("dist_matrix", key)
    if cached is not None:
        return cached

    n = len(coords)
    dist = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            dist[i][j] = haversine(coords[i][0], coords[i][1],
                                   coords[j][0], coords[j][1])

    save_cache("dist_matrix", key, dist)
    return dist

# =========================
# ACO TSP (RETURN TO DEPOT)
# =========================
def aco_route_fast(dist_matrix, coords):
    key = hash_coords(coords) + f"_{N_ANTS}_{N_ITER}_{ALPHA}_{BETA}_cycle"
    cached = load_cache("aco_fast", key)
    if cached is not None:
        print("⚡ FAST ACO loaded from cache")
        return cached

    n = len(dist_matrix)

    time_matrix = dist_matrix / AVG_SPEED_KMH
    heuristic = 1 / (time_matrix + 1e-9)

    CANDIDATE_SIZE = min(15, n-1)
    candidate_list = np.argsort(dist_matrix, axis=1)[:, 1:CANDIDATE_SIZE+1]

    pheromone = np.full((n, n), INITIAL_PHEROMONE)
    best_route = None
    best_length = float("inf")
    stagnation = 0

    for iteration in range(N_ITER):
        all_routes = []
        all_lengths = []

        for ant in range(N_ANTS):
            visited = np.zeros(n, dtype=bool)
            visited[0] = True
            route = [0]

            for step in range(n-1):
                i = route[-1]
                candidates = [j for j in candidate_list[i] if not visited[j]]
                if not candidates:
                    candidates = np.where(~visited)[0]

                tau = pheromone[i, candidates] ** ALPHA
                eta = heuristic[i, candidates] ** BETA
                prob = tau * eta
                prob /= prob.sum()

                next_node = np.random.choice(candidates, p=prob)
                route.append(next_node)
                visited[next_node] = True

            # =========================
            # PANJANG RUTE SIKLIK
            # =========================
            length = np.sum(dist_matrix[route[:-1], route[1:]])
            length += dist_matrix[route[-1], route[0]]  # kembali ke depot

            all_routes.append(route)
            all_lengths.append(length)

            if length < best_length:
                best_length = length
                best_route = route
                stagnation = 0

        stagnation += 1
        if stagnation > 15:
            print(f"🛑 Early convergence at iter {iteration}")
            break

        pheromone *= (1 - EVAPORATION)

        elite_count = max(1, int(0.2 * N_ANTS))
        elite_idx = np.argsort(all_lengths)[:elite_count]

        for idx in elite_idx:
            route = all_routes[idx]
            length = all_lengths[idx]

            for i in range(len(route)-1):
                pheromone[route[i], route[i+1]] += Q / length

            pheromone[route[-1], route[0]] += Q / length  # edge kembali

        for i in range(len(best_route)-1):
            pheromone[best_route[i], best_route[i+1]] += 2 * Q / best_length

        pheromone[best_route[-1], best_route[0]] += 2 * Q / best_length

    result = (best_route, best_length)
    save_cache("aco_fast", key, result)
    return result

# =========================
# LOAD DATA
# =========================
df = pd.read_excel(FILE_PATH)

for vid in sorted(df["voronoi_id"].unique()):
    print(f"\n=== Optimasi Kurir Area {vid} ===")

    sub = df[df["voronoi_id"] == vid].copy().reset_index(drop=True)

    coords = [(START_LAT, START_LON)]
    coords += list(zip(sub["latitude"], sub["longitude"]))

    dist_matrix = build_distance_matrix(coords)
    route, total_distance = aco_route_fast(dist_matrix, coords)

    delivery_points = len(coords) - 1
    # total_distance sudah termasuk kembali ke depot
    travel_time_hours = total_distance / AVG_SPEED_KMH
    # service time hanya di titik delivery (bukan depot)
    total_time_min = travel_time_hours * 60 + delivery_points * SERVICE_TIME_MIN

    print("Jumlah titik:", delivery_points)
    print("Total jarak (km):", round(total_distance, 2))
    print("Total waktu (menit):", round(total_time_min, 1))

    # =========================
    # URUTAN KIRIM PER RESI
    # =========================
    cum_dist = 0
    cum_time = 0

    for seq in range(1, len(route)):
        i_prev = route[seq-1]
        i_curr = route[seq]

        step_dist = dist_matrix[i_prev][i_curr]
        step_time = (step_dist / AVG_SPEED_KMH) * 60 + SERVICE_TIME_MIN

        cum_dist += step_dist
        cum_time += step_time

        row = sub.iloc[i_curr-1]

        route_rows.append({
            "voronoi_id": vid,
            "sequence": seq,
            "resi": row["resi"],
            "latitude": row["latitude"],
            "longitude": row["longitude"],
            "distance_step_km": step_dist,
            "distance_cumulative_km": cum_dist,
            "time_cumulative_min": cum_time
        })

    # =========================
    # SUMMARY PER KURIR (1x)
    # =========================
    summary_rows.append({
        "voronoi_id": vid,
        "total_distance_km": total_distance,
        "total_time_min": total_time_min,
        "n_stop": delivery_points
    })

    all_distances.append(total_distance)
    all_points.append(delivery_points)
    all_times.append(total_time_min)
    all_vids.append(vid)
     # =========================
    # MAP VISUALIZATION
    # =========================
    if vid == MAP_VORONOI_ID:
        m = folium.Map(location=[START_LAT, START_LON], zoom_start=12)

        ordered_coords = [coords[i] for i in route]
        ordered_coords.append(coords[0])  # kembali ke depot

        folium.Marker(
            [START_LAT, START_LON],
            popup="START - Distribution Center",
            icon=folium.Icon(color="red")
        ).add_to(m)

        for seq in range(1, len(route)):
            node_index = route[seq]
            row = sub.iloc[node_index - 1]

            lat = row["latitude"]
            lon = row["longitude"]
            resi = row["resi"]

            popup_text = f"""
            <b>Urutan Kirim:</b> {seq}<br>
            <b>No Resi:</b> {resi}
            """

            folium.Marker(
                [lat, lon],
                popup=folium.Popup(popup_text, max_width=250),
                tooltip=f"Stop {seq}",
                icon=DivIcon(
                    icon_size=(36, 36),
                    icon_anchor=(18, 18),
                    html=f"""
                    <div style="
                        background-color:#2A81CB;
                        color:white;
                        border-radius:50%;
                        width:28px;
                        height:28px;
                        text-align:center;
                        font-weight:bold;
                        line-height:28px;
                        border:2px solid white;
                        box-shadow:0 0 3px rgba(0,0,0,0.6);
                    ">
                        {seq}
                    </div>
                    """
                )
            ).add_to(m)

        folium.PolyLine(ordered_coords).add_to(m)

        map_path = f"D:/IPB/TESIS/PENELITIAN/CODE/output/aco/rute_{running_date}_voronoi_{vid}.html"
        m.save(map_path)
        print("🗺️ Map tersimpan:", map_path)

# =========================
# SAVE SUMMARY
# =========================
output_path = f"D:/IPB/TESIS/PENELITIAN/CODE/output/aco/hasil_optimasi_rute_{running_date}.xlsx"

df_route = pd.DataFrame(route_rows)
df_summary = pd.DataFrame(summary_rows)

with pd.ExcelWriter(output_path) as writer:
    df_route.to_excel(writer, sheet_name="route_sequence", index=False)
    df_summary.to_excel(writer, sheet_name="courier_summary", index=False)

print("\n✅ Semua rute selesai dihitung")
print("📁 File hasil:", output_path)
print(f"🗺️ Map area {MAP_VORONOI_ID}: rute_{running_date}_voronoi_{MAP_VORONOI_ID}.html")

# =====================================================
# SUMMARY GLOBAL – JARAK
# =====================================================
all_distances = np.array(all_distances)
all_points = np.array(all_points)

df_summary_global = pd.DataFrame([{
    "jumlah_area_voronoi": len(all_vids),
    "total_point": int(all_points.sum()),
    "total_jarak_global_km": all_distances.sum(),
    "mean_jarak_km": all_distances.mean(),
    "std_jarak_km": all_distances.std(),
    "min_jarak_km": all_distances.min(),
    "max_jarak_km": all_distances.max()
}])

print("\nSUMMARY GLOBAL – JARAK\n")
print(df_summary_global.round(3))

# =====================================================
# SUMMARY GLOBAL – JUMLAH POINT
# =====================================================
df_summary_point_global = pd.DataFrame([{
    "total_point_global": int(all_points.sum()),
    "mean_point_per_cell": all_points.mean(),
    "std_point_per_cell": all_points.std(),
    "min_point_per_cell": all_points.min(),
    "max_point_per_cell": all_points.max()
}])

print("\nSUMMARY GLOBAL – JUMLAH POINT PER CELL\n")
print(df_summary_point_global.round(2))