import numpy as np
import pandas as pd
import googlemaps
import folium
from datetime import datetime
import time
import os
import json


# ==========================================
# CONFIG
# ==========================================
running_date = "2025-07-28"

API_KEY = "AIzaSyAdNOaRAzzRFTl_spTUbvm9O_yz8qBByew"
INPUT_FILE = f"D:/IPB/TESIS/PENELITIAN/CODE/output/ccvd/ccvd_voronoi_assignment_final_{running_date}.xlsx"
OUTPUT_EXCEL = f"D:/IPB/TESIS/PENELITIAN/CODE/output/aco/gmap/aco_routing_result_{running_date}.xlsx"
MAP_OUTPUT_HTML = f"D:/IPB/TESIS/PENELITIAN/CODE/output/aco/gmap/aco_route_map_voronoi_{running_date}.html"
CACHE_FILE = f"D:/IPB/TESIS/PENELITIAN/CODE/output/aco/gmap/cache_distance_{running_date}.json"

GLOBAL_DEPOT = (-6.535158, 106.799133)

# ACO parameter
N_ANTS = 25
N_ITER = 80
ALPHA = 1.0
BETA = 3.0
RHO = 0.5
Q = 100

# pilih salah satu voronoi untuk divisualkan
VORONOI_TO_PLOT = 1

gmaps = googlemaps.Client(key=API_KEY)

# ==============================
# CACHE LOAD/SAVE
# ==============================
def load_cache(path):
    if os.path.exists(path):
        try:
            with open(path, "r") as f:
                return json.load(f)
        except:
            return {}
    return {}

def save_cache(path, cache):
    with open(path, "w") as f:
        json.dump(cache, f)

def key_pair(a, b):
    return f"{a[0]:.6f},{a[1]:.6f}|{b[0]:.6f},{b[1]:.6f}"

# ==========================================
# BATCH CALL GMAP
#===========================================
def request_distance(origin, destination, max_retry=5):

    for attempt in range(max_retry):
        try:
            res = gmaps.distance_matrix(
                [origin],
                [destination],
                mode="driving",
                departure_time=datetime.now()
            )

            el = res["rows"][0]["elements"][0]

            if el["status"] != "OK":
                raise Exception(el["status"])

            return el["distance"]["value"], el["duration"]["value"]

        except Exception as e:

            wait = 2 ** attempt
            print(f"Retry {attempt+1} | wait {wait}s | error {e}")
            time.sleep(wait)

    raise Exception("Gagal request Google Maps setelah retry")
# ==========================================
# DISTANCE + TIME MATRIX (meter, second)
# ==========================================
def get_distance_time_matrix(coords, batch_size=10):
    n = len(coords)
    dist = np.zeros((n, n))
    dur = np.zeros((n, n))

    new_cache_entries = 0

    for i in range(n):
        for j in range(n):

            if i == j:
                continue

            key = key_pair(coords[i], coords[j])

            if key in cache:
                dist[i][j] = cache[key]["distance"]
                dur[i][j] = cache[key]["duration"]
                continue

            # === CALL GOOGLE MAPS ===
            distance_val, duration_val = request_distance(coords[i], coords[j])

            cache[key] = {
                "distance": distance_val,
                "duration": duration_val
            }

            new_cache_entries += 1
            time.sleep(1)  # rate limit aman

    print("Cache baru ditambahkan:", new_cache_entries)
    return dist, dur

# ==========================================
# ACO TSP MULTI OBJECTIVE (DIST + TIME)
# ==========================================
def ant_colony_tsp(dist_matrix, time_matrix):

    n = len(dist_matrix)
    pheromone = np.ones((n, n))

    best_route = None
    best_cost = float("inf")

    # normalisasi agar skala seimbang
    dist_norm = dist_matrix / np.max(dist_matrix)
    time_norm = time_matrix / np.max(time_matrix)

    cost_matrix = 0.5 * dist_norm + 0.5 * time_norm

    for it in range(N_ITER):

        routes = []
        costs = []

        for ant in range(N_ANTS):

            visited = [0]
            current = 0

            while len(visited) < n:

                probs = []
                nodes = []

                for j in range(n):
                    if j not in visited:
                        tau = pheromone[current][j] ** ALPHA
                        eta = (1 / cost_matrix[current][j]) ** BETA
                        probs.append(tau * eta)
                        nodes.append(j)

                probs = np.array(probs) / np.sum(probs)
                next_node = np.random.choice(nodes, p=probs)

                visited.append(next_node)
                current = next_node

            cost = sum(cost_matrix[visited[i]][visited[i+1]] for i in range(n-1))

            routes.append(visited)
            costs.append(cost)

            if cost < best_cost:
                best_cost = cost
                best_route = visited

        pheromone *= (1 - RHO)

        for r, c in zip(routes, costs):
            for i in range(len(r)-1):
                pheromone[r[i]][r[i+1]] += Q / c

        print(f"Iter {it} | best cost = {best_cost:.4f}")

    return best_route

# ==========================================
# ROUTE VISUALIZATION
# ==========================================
def plot_route_map(coords, route, filename):

    m = folium.Map(location=coords[0], zoom_start=12)

    for i, idx in enumerate(route):
        lat, lon = coords[idx]
        folium.Marker(
            [lat, lon],
            popup=f"Stop {i}",
        ).add_to(m)

    path = [coords[i] for i in route]
    folium.PolyLine(path).add_to(m)

    m.save(filename)
    print("Map saved:", filename)

# ==========================================
# LOAD DATA
# ==========================================
df = pd.read_excel(INPUT_FILE)
cache = load_cache(CACHE_FILE)

results = []
summary = []

# ==========================================
# PROCESS PER COURIER
# ==========================================
for vid, group in df.groupby("voronoi_id"):
    time.sleep(0.2)
    print(f"\n=== Optimasi Kurir Area {vid} ===")

    coords = [GLOBAL_DEPOT] + list(zip(group["latitude"], group["longitude"]))

    dist_matrix, time_matrix = get_distance_time_matrix(coords)
    save_cache(CACHE_FILE, cache)

    best_route = ant_colony_tsp(dist_matrix, time_matrix)

    total_dist = sum(
        dist_matrix[best_route[i]][best_route[i+1]]
        for i in range(len(best_route)-1)
    )

    total_time = sum(
        time_matrix[best_route[i]][best_route[i+1]]
        for i in range(len(best_route)-1)
    )

    ordered = group.iloc[[i-1 for i in best_route[1:]]]

    for seq, (_, row) in enumerate(ordered.iterrows(), start=1):
        results.append({
            "voronoi_id": vid,
            "sequence": seq,
            "resi": row["resi"],
            "latitude": row["latitude"],
            "longitude": row["longitude"],
            "total_distance_km": total_dist / 1000,
            "total_time_min": total_time / 60
        })

    summary.append({
        "voronoi_id": vid,
        "total_distance_km": total_dist / 1000,
        "total_time_min": total_time / 60,
        "n_stop": len(group)
    })

    if vid == VORONOI_TO_PLOT:
        plot_route_map(coords, best_route, MAP_OUTPUT_HTML)
        
# ==========================================
# EXPORT
# ==========================================
df_route = pd.DataFrame(results)
df_summary = pd.DataFrame(summary)

with pd.ExcelWriter(OUTPUT_EXCEL) as writer:
    df_route.to_excel(writer, sheet_name="route_sequence", index=False)
    df_summary.to_excel(writer, sheet_name="courier_summary", index=False)

print("\n✅ Optimasi selesai")
print("Excel:", OUTPUT_EXCEL)
print("Map:", MAP_OUTPUT_HTML)