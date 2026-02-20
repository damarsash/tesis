import numpy as np
import random
import math
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, PillowWriter
# =========================
# DATA KOTA (BOGOR)
# =========================
cities = {
   0: (-6.5971, 106.8060),  # Kebun Raya
   1: (-6.5944, 106.7892),  # BTM
   2: (-6.6030, 106.8000),  # Empang
   3: (-6.5850, 106.8045),  # Baranangsiang
   4: (-6.6100, 106.8140),  # Tajur
   5: (-6.5690, 106.8020),  # Yasmin
   6: (-6.6015, 106.8180),  # Pajajaran
   7: (-6.5820, 106.7920),  # Semplak
   8: (-6.6150, 106.8065),  # Bondongan
   9: (-6.5900, 106.8200)   # Sukasari
}
# =========================
# FUNGSI JARAK
# =========================
def distance(a, b):
   return math.sqrt((a[0] - b[0])**2 + (a[1] - b[1])**2)
# =========================
# ANT COLONY OPTIMIZATION
# =========================
class AntColony:
   def __init__(self, cities, n_ants=20, n_iterations=50,
                alpha=1, beta=5, evaporation=0.5, Q=100):
       self.cities = cities
       self.n_cities = len(cities)
       self.n_ants = n_ants
       self.n_iterations = n_iterations
       self.alpha = alpha
       self.beta = beta
       self.evaporation = evaporation
       self.Q = Q
       self.distances = np.zeros((self.n_cities, self.n_cities))
       for i in range(self.n_cities):
           for j in range(self.n_cities):
               self.distances[i][j] = distance(cities[i], cities[j])
       self.pheromone = np.ones((self.n_cities, self.n_cities))
       self.best_path = None
       self.best_length = float('inf')
       self.history = []
       self.best_distances = []
   def run(self):
       for iteration in range(self.n_iterations):
           all_paths = []
           all_lengths = []
           for _ in range(self.n_ants):
               path = self.construct_path()
               length = self.path_length(path)
               all_paths.append(path)
               all_lengths.append(length)
               if length < self.best_length:
                   self.best_length = length
                   self.best_path = path
           self.update_pheromone(all_paths, all_lengths)
           self.history.append(self.best_path.copy())
           self.best_distances.append(self.best_length)
           print(f"Iterasi {iteration+1} | Jarak terbaik = {self.best_length:.6f}")
       return self.best_path, self.best_length
   def construct_path(self):
       start = random.randint(0, self.n_cities - 1)
       path = [start]
       visited = {start}
       while len(visited) < self.n_cities:
           current = path[-1]
           next_city = self.select_next_city(current, visited)
           path.append(next_city)
           visited.add(next_city)
       path.append(start)
       return path
   def select_next_city(self, current, visited):
       probs = []
       for city in range(self.n_cities):
           if city not in visited:
               pher = self.pheromone[current][city] ** self.alpha
               heur = (1 / self.distances[current][city]) ** self.beta
               probs.append((city, pher * heur))
       total = sum(p[1] for p in probs)
       r = random.uniform(0, total)
       cum = 0
       for city, prob in probs:
           cum += prob
           if r <= cum:
               return city
   def update_pheromone(self, paths, lengths):
       self.pheromone *= (1 - self.evaporation)
       for path, length in zip(paths, lengths):
           for i in range(len(path) - 1):
               a, b = path[i], path[i+1]
               self.pheromone[a][b] += self.Q / length
               self.pheromone[b][a] += self.Q / length
   def path_length(self, path):
       return sum(self.distances[path[i]][path[i+1]] for i in range(len(path) - 1))
# =========================
# ANIMASI + SIMPAN GIF
# =========================
def animate_routes_gif(cities, history, filename="aco_bogor.gif"):
   fig, ax = plt.subplots()
   lats = [cities[i][0] for i in cities]
   longs = [cities[i][1] for i in cities]
   ax.scatter(longs, lats)
   for i in cities:
       ax.text(cities[i][1], cities[i][0], str(i))
   line, = ax.plot([], [], marker='o')
   ax.set_xlabel("Longitude")
   ax.set_ylabel("Latitude")
   ax.grid(True)
   def update(frame):
       path = history[frame]
       x = [cities[i][1] for i in path]
       y = [cities[i][0] for i in path]
       line.set_data(x, y)
       ax.set_title(f"ACO Bogor - Iterasi {frame+1}")
       return line,
   ani = FuncAnimation(fig, update, frames=len(history), interval=300)
   ani.save(filename, writer=PillowWriter(fps=3))
   plt.close()
   print(f"GIF tersimpan: {filename}")
# =========================
# GRAFIK KONVERGENSI
# =========================
def plot_distance(best_distances):
   plt.figure()
   plt.plot(best_distances)
   plt.xlabel("Iterasi")
   plt.ylabel("Jarak Terbaik")
   plt.title("Grafik Jarak vs Iterasi (ACO)")
   plt.grid(True)
   plt.show()
# =========================
# MAIN PROGRAM
# =========================
if __name__ == "__main__":
   aco = AntColony(cities)
   best_path, best_distance = aco.run()
   print("\nRute Terbaik:", best_path)
   print("Total Jarak:", best_distance)
   animate_routes_gif(cities, aco.history, "D:/IPB/TESIS/PENELITIAN/CODE/output/aco_bogor.gif")
   plot_distance(aco.best_distances)