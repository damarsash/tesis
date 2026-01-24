# ============================================================
# HITUNG JARAK TEMPUH HARIAN KURIR (START DEPOT, NO RETURN)
# ============================================================

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(geosphere)
library(stringr)

# ------------------------------------------------------------
# 0. DEFINISI TITIK DEPOT (TITIK AWAL SETIAP HARI)
# ------------------------------------------------------------
DEPOT_LAT <- -6.535158
DEPOT_LON <- 106.799133

# ------------------------------------------------------------
# 1. BACA DATA RAW
# ------------------------------------------------------------
data_raw <- read_excel(
  "D:/IPB/TESIS/PENELITIAN/CODE/source/CLBT0_21JUL_20AUG.xlsx",
  sheet = "Raw"
)

# ------------------------------------------------------------
# 2. BERSIHKAN & KONVERSI KOORDINAT
# ------------------------------------------------------------
data_clean <- data_raw %>%
  mutate(
    latitude  = as.numeric(latitude)  / 1e6,
    longitude = as.numeric(longitude) / 1e6
  ) %>%
  filter(
    as.Date(`205_dt`) == as.Date("2025-08-20"),
    !is.na(latitude),
    !is.na(longitude),
    between(longitude, 90, 140),
    between(latitude,  -10, 10),
    str_detect(position_name, regex("COURIER", ignore_case = TRUE)),
    str_detect(distribution_point, regex("CLBT0", ignore_case = TRUE))
  )

# ------------------------------------------------------------
# 3. HITUNG JARAK TEMPUH PER KURIR PER HARI
#    (START DEPOT, OPEN ROUTE)
# ------------------------------------------------------------
data_kurir_harian <- data_clean %>%
  arrange(`205_id`, `205_dt`, `205_tm`) %>%
  group_by(`205_id`, `205_dt`) %>%
  mutate(
    # titik sebelumnya
    lon_prev = lag(longitude),
    lat_prev = lag(latitude),
    
    # jika titik pertama, start dari depot
    lon_prev = ifelse(row_number() == 1, DEPOT_LON, lon_prev),
    lat_prev = ifelse(row_number() == 1, DEPOT_LAT, lat_prev),
    
    # jarak antar titik (meter)
    seg_meter = distHaversine(
      cbind(lon_prev, lat_prev),
      cbind(longitude, latitude)
    )
  ) %>%
  summarise(
    total_awb_harian  = n(),
    total_distance_km = sum(seg_meter, na.rm = TRUE) / 1000
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 4. LIHAT HASIL DATA (TABEL)
# ------------------------------------------------------------
print(data_kurir_harian)

#write_xlsx(list(jarak_kurir_harian = data_kurir_harian),path = "D:/IPB/TESIS/PENELITIAN/CODE/out/jarak_kurir_harian_dan_total.xlsx")

total_resi <- sum(data_kurir_harian$total_awb_harian)
mean_resi <- mean(data_kurir_harian$total_awb_harian)
sd_resi <- sd(data_kurir_harian$total_awb_harian)
min_awb <- min(data_kurir_harian$total_awb_harian)
max_awb <- max(data_kurir_harian$total_awb_harian)

total_jarak <- sum(data_kurir_harian$total_distance_km)
mean_jarak <- mean(data_kurir_harian$total_distance_km)
sd_jarak <- sd(data_kurir_harian$total_distance_km)
min_jarak <- min(data_kurir_harian$total_distance_km)
max_jarak <- max(data_kurir_harian$total_distance_km)

cat("Jumlah Paket :", total_resi, "\n")
cat("Rata-rata jumlah resi per cluster:", mean_resi, "\n")
cat("Standar deviasi jumlah resi per cluster:", sd_resi, "\n")
cat("Minimum :", min_awb, "\n")
cat("Maximum :", max_awb, "\n")

cat("Total Jarak (Km) :", total_jarak, "\n")
cat("Rata-rata jarak per cluster(Km):", mean_jarak, "\n")
cat("Standar deviasi jarak per cluster (Km):", sd_jarak, "\n")
cat("Minimum (Km):", min_jarak, "\n")
cat("Maximum (Km):", max_jarak, "\n")