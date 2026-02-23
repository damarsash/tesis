# =====================================================
# 0. LIBRARY
# =====================================================
library(readxl)
library(writexl)
library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(geosphere)

DATE_FILTER <- "2025-08-20"

# =====================================================
# 1. BACA DATA UTAMA
# =====================================================
data <- read_excel(
  "D:/IPB/TESIS/PENELITIAN/CODE/source/CLBT0_21JUL_20AUG.xlsx",
  sheet = "Raw"
) %>%
  mutate(
    latitude  = latitude / 1e6,
    longitude = longitude / 1e6
  ) %>%
  filter(
    as.Date(`205_dt`) == as.Date(DATE_FILTER),
    !is.na(longitude),
    !is.na(latitude),
    between(longitude, -97, 140),
    between(latitude, -20, 5)
  )

# =====================================================
# 2. K-MEANS CLUSTERING
# =====================================================
k <- data %>% distinct(`205_id`) %>% nrow()
stopifnot(k > 1)

set.seed(42)
kmeans_result <- kmeans(
  data[, c("latitude", "longitude")],
  centers = k
)

data$cluster_id <- kmeans_result$cluster

# =====================================================
# 2.1 CENTROID K-MEANS
# =====================================================
centroid_df <- as.data.frame(kmeans_result$centers) %>%
  rename(
    centroid_latitude  = latitude,
    centroid_longitude = longitude
  ) %>%
  mutate(cluster_id = row_number())

# =====================================================
# 3. NAMA CLUSTER (KURIR)
# =====================================================
cluster_lookup <- read_excel(
  "D:/IPB/TESIS/PENELITIAN/CODE/source/CLBT0_21JUL_20AUG.xlsx",
  sheet = "courier_nik"
) %>%
  mutate(cluster_id = as.integer(cluster_id))

data <- data %>%
  left_join(cluster_lookup, by = "cluster_id") %>%
  left_join(centroid_df, by = "cluster_id")

# =====================================================
# 4. VISUALISASI LEAFLET + LABEL NIK KURIR
# =====================================================

# centroid + NIK kurir
centroid_label <- centroid_df %>%
  left_join(cluster_lookup, by = "cluster_id")

pal <- colorFactor(
  palette = colorRampPalette(brewer.pal(9, "Set1"))(k),
  domain = data$kurir
)

leaflet(data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  addCircleMarkers(
    ~longitude, ~latitude,
    label = ~paste(resi, ":", kurir),
    color = ~pal(kurir),
    radius = 7,
    stroke = FALSE,
    fillOpacity = 0.8
  ) %>%
  
  addCircleMarkers(
    data = centroid_label,
    ~centroid_longitude,
    ~centroid_latitude,
    radius = 10,
    color = "black",
    fillColor = "yellow",
    fillOpacity = 1,
    stroke = TRUE,
    weight = 2
  ) %>%
  
  addLabelOnlyMarkers(
    data = centroid_label,
    ~centroid_longitude,
    ~centroid_latitude,
    label = ~paste("NIK:", kurir),
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "top",
      textOnly = TRUE,
      style = list(
        "font-size" = "12px",
        "font-weight" = "bold",
        "color" = "black"
      )
    )
  )

# =====================================================
# 5. HITUNG JARAK TEMPUH ANTAR RESI
# =====================================================
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

# =====================================================
# 6. JARAK RESI → CENTROID
# =====================================================
data_distance <- data_distance %>%
  mutate(
    dist_to_centroid_km = distHaversine(
      cbind(longitude, latitude),
      cbind(centroid_longitude, centroid_latitude)
    ) / 1000
  )

# =====================================================
# 7. STATISTIK JARAK PER CLUSTER
# =====================================================
distance_stats_per_cluster <- data_distance %>%
  group_by(kurir) %>%
  summarise(
    total_distance_km       = sum(seg_meter, na.rm = TRUE) / 1000,
    mean_distance_km        = mean(seg_meter, na.rm = TRUE) / 1000,
    sd_distance_km          = sd(seg_meter, na.rm = TRUE) / 1000,
    min_distance_km         = min(seg_meter, na.rm = TRUE) / 1000,
    max_distance_km         = max(seg_meter, na.rm = TRUE) / 1000,
    mean_centroid_dist_km   = mean(dist_to_centroid_km, na.rm = TRUE),
    total_stop              = n()
  ) %>%
  ungroup()

# =====================================================
# 8. STATISTIK JUMLAH RESI
# =====================================================
resi_count_per_cluster <- data %>%
  count(kurir, name = "resi_count")

total_resi <- sum(resi_count_per_cluster$resi_count)

cat("\n=== STATISTIK RESI ===\n")
cat("Jumlah Klaster :", k, "\n")
cat("Total Resi     :", total_resi, "\n")
cat("Rata-rata      :", mean(resi_count_per_cluster$resi_count), "\n")
cat("Std Dev        :", sd(resi_count_per_cluster$resi_count), "\n")
cat("Minimum        :", min(resi_count_per_cluster$resi_count), "\n")
cat("Maximum        :", max(resi_count_per_cluster$resi_count), "\n")

# =====================================================
# 9. STATISTIK JARAK GLOBAL
# =====================================================
cat("\n=== STATISTIK JARAK TEMPUH ===\n")
cat("Jumlah Klaster          :", nrow(distance_stats_per_cluster), "\n")
cat("Total Jarak (km)        :", sum(distance_stats_per_cluster$total_distance_km), "\n")
cat("Rata-rata Jarak (km)    :", mean(distance_stats_per_cluster$total_distance_km), "\n")
cat("Standar Deviasi (km)    :", sd(distance_stats_per_cluster$total_distance_km), "\n")
cat("Minimum Jarak (km)      :", min(distance_stats_per_cluster$total_distance_km), "\n")
cat("Maximum Jarak (km)      :", max(distance_stats_per_cluster$total_distance_km), "\n")

# =====================================================
# 10. SIMPAN OUTPUT
# =====================================================
write_xlsx(
  list(
    data_distance = data_distance,
    centroid      = centroid_df,
    summary_jarak = distance_stats_per_cluster,
    summary_resi  = resi_count_per_cluster
  ),
  path = paste0(
    "D:/IPB/TESIS/PENELITIAN/CODE/output/hasil_kmeans_",
    DATE_FILTER,
    ".xlsx"
  )
)

