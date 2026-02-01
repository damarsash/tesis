import pandas as pd
from collections import defaultdict
import numpy as np

# ======================================================
# 1. INPUT DATA (CONTOH A–F)
# ======================================================
data = {
    "Area": ["A", "B", "C", "D", "E", "F"],
    "Resi": [1, 4, 6, 10, 2, 1]
}

neighbors = {
    "A": ["D", "B"],
    "B": ["D", "C"],
    "C": ["B", "D", "E", "F"],
    "D": ["A", "B", "C", "F"],
    "E": ["C", "D"],
    "F": ["C", "E"]
}

df = pd.DataFrame(data).set_index("Area")

# ======================================================
# 2. PARAMETER
# ======================================================
MAX_ITER = 1000
VERBOSE = True   # ubah False jika tidak mau log

# ======================================================
# 3. TARGET LOAD
# ======================================================
total_resi = df["Resi"].sum()
n_area = len(df)
target = total_resi // n_area   # floor, diskrit

print(f"\nTotal Resi   : {total_resi}")
print(f"Jumlah Area : {n_area}")
print(f"Target Resi : {target}\n")

# ======================================================
# 4. DISCRETE NEIGHBOR LOAD BALANCING
# ======================================================
resi = df["Resi"].to_dict()

for it in range(MAX_ITER):
    moved = False

    for area in resi.keys():

        if resi[area] >= target:
            continue

        # cari tetangga dengan resi lebih besar
        for nb in neighbors[area]:
            if resi[nb] > resi[area]:

                # transfer 1 paket
                resi[nb] -= 1
                resi[area] += 1
                moved = True

                if VERBOSE:
                    print(f"Iter {it:03d}: {nb} → {area}")

                break   # 1 paket per iterasi per area

    if not moved:
        break

# ======================================================
# 5. OUTPUT AKHIR
# ======================================================
final = pd.DataFrame.from_dict(resi, orient="index", columns=["Final_Resi"])

print("\n=== HASIL AKHIR STABIL ===\n")
print(final)

# Statistik
values = final["Final_Resi"].values

print("\n--- Statistik ---")
print(f"Rata-rata : {values.mean():.2f}")
print(f"Std Dev   : {values.std():.2f}")
print(f"Min       : {values.min()}")
print(f"Max       : {values.max()}")
