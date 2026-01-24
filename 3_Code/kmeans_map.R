# Install dan panggil library
#install.packages(c("leaflet", "RColorBrewer", "readxl", "tidyverse", "sf"))
library(readxl)
library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
library(dplyr)

# --- 1. Baca data utama ---
data <- read_excel("D:/IPB/TESIS/PENELITIAN/CODE/source/CLBT0_21JUL_20AUG.xlsx", 
                   sheet = "Raw") %>%
  mutate(
    latitude = latitude / 1e6,
    longitude = longitude / 1e6
  ) %>%
  filter(
    as.Date(`205_dt`) == as.Date("2025-08-20"),
    #as.Date(`205_dt`) >= as.Date("2025-07-20") & as.Date(`205_dt`) <= as.Date("2025-08-21"),
    !is.na(longitude),
    !is.na(latitude),
    between(longitude, -97, 140),
    between(latitude, -20, 5)
  )

# --- 2. Jalankan K-means ---
k <- data %>% distinct(`205_id`) %>% nrow()
# Safety check (hindari error kmeans)
stopifnot(k > 1)

set.seed(42)
kmeans_result <- kmeans(data[, c("latitude", "longitude")], centers = k)
data$cluster_id <- kmeans_result$cluster

# --- 3. Baca nama cluster dari sheet lain ---
cluster_lookup <- read_excel("D:/IPB/TESIS/PENELITIAN/CODE/source/CLBT0_21JUL_20AUG.xlsx", 
                             sheet = "courier_nik")

# Pastikan kolom cluster_id berupa integer agar join cocok
cluster_lookup <- cluster_lookup %>% mutate(cluster_id = as.integer(cluster_id))

# Gabungkan nama cluster ke data utama
data <- data %>%
  left_join(cluster_lookup, by = "cluster_id")

# --- 4. Warna berdasarkan nama cluster ---
pal <- colorFactor(
  palette = colorRampPalette(brewer.pal(9, "Set1"))(k), 
  domain = data$kurir
)

# --- 5. Visualisasi Leaflet ---
leaflet(data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    label = ~paste(resi, ":", kurir),
    color = ~pal(kurir),
    radius = 8,
    stroke = FALSE, fillOpacity = 0.8
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~kurir,
    title = "Cluster"
  )

# --- 6. Kalkulasi jarak tempuh ---
data_distance <- data %>%
  arrange(kurir, `205_tm`) %>%
  group_by(kurir) %>%
  mutate(
    lon_prev = lag(longitude),
    lat_prev = lag(latitude),
    seg_meter = distHaversine(
      cbind(lon_prev, lat_prev),
      cbind(longitude, latitude)
    )
  ) %>%
  ungroup()

distance_stats_per_cluster <- data_distance %>%
  group_by(kurir) %>%
  summarise(
    total_distance_km = sum(seg_meter, na.rm = TRUE) / 1000,
    mean_distance_km  = mean(seg_meter, na.rm = TRUE) / 1000,
    sd_distance_km    = sd(seg_meter, na.rm = TRUE) / 1000,
    min_distance_km   = min(seg_meter, na.rm = TRUE) / 1000,
    max_distance_km   = max(seg_meter, na.rm = TRUE) / 1000,
    total_stop        = n()
  ) %>%
  ungroup()



#=========HASIL JUMLAH PAKET============
resi_count_per_cluster <- data %>% count(kurir, name = "resi_count")

print(n=k,resi_count_per_cluster)
total_resi <- sum(resi_count_per_cluster$resi_count)
mean_resi <- mean(resi_count_per_cluster$resi_count)
sd_resi <- sd(resi_count_per_cluster$resi_count)
min_awb <- min(resi_count_per_cluster$resi_count)
max_awb <- max(resi_count_per_cluster$resi_count)

cat("Jumlah Klaster :", k, "\n")
cat("Jumlah Paket :", total_resi, "\n")
cat("Rata-rata jumlah resi per cluster:", mean_resi, "\n")
cat("Standar deviasi jumlah resi per cluster:", sd_resi, "\n")
cat("Minimum :", min_awb, "\n")
cat("Maximum :", max_awb, "\n")

#=========HASIL JARAK TEMPUH============
print(distance_stats_per_cluster, n = k)

cat("Jumlah Klaster          :", nrow(distance_stats_per_cluster), "\n")
cat("Total Jarak (km)        :", sum(distance_stats_per_cluster$total_distance_km), "\n")
cat("Rata-rata Jarak (km)    :", mean(distance_stats_per_cluster$total_distance_km), "\n")
cat("Standar Deviasi (km)    :", sd(distance_stats_per_cluster$total_distance_km), "\n")
cat("Minimum Jarak (km)      :", min(distance_stats_per_cluster$total_distance_km), "\n")
cat("Maximum Jarak (km)      :", max(distance_stats_per_cluster$total_distance_km), "\n")
