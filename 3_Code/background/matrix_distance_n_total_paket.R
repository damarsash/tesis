library(readxl)
library(dplyr)
library(ggplot2)
library(geosphere)   # untuk distHaversine
library(stringr)

# 1. BACA DATA RAW ---------------------------------------------------------
data_raw <- read_excel("D:/IPB/TESIS/PENELITIAN/CODE/source/CLBT0_21JUL_20AUG.xlsx",
                       sheet = "Raw")

# 2. BERSIHKAN & KONVERSI KOORDINAT ---------------------------------------
data_clean <- data_raw %>%
  mutate(
    # konversi ke numeric dulu (ada kemungkinan '.' dsb)
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude),
    
    # di file ini lat/long disimpan x 1e6 → balik ke derajat
    latitude  = latitude  / 1e6,
    longitude = longitude / 1e6
  ) %>%
  # filter koordinat valid + khusus CLBT0 & COURIER (sesuai logika kode kamu sebelumnya)
  filter(
    !is.na(latitude), !is.na(longitude),
    between(longitude, 90, 140),   # Asia Tenggara
    between(latitude,  -10, 10),
    str_detect(position_name, regex("COURIER", ignore_case = TRUE)),
    str_detect(distribution_point, regex("CLBT0", ignore_case = TRUE))
  )

# 3. HITUNG TOTAL AWB & TOTAL JARAK PER KURIR -----------------------------
# asumsi 205_name = nama kurir, 205_tm = waktu scan POP/delivery
data_kurir <- data_clean %>%
  arrange(`205_id`, `205_dt`, `205_tm`) %>%
  group_by(`205_id`) %>%
  # hitung jarak antar titik berurutan per kurir
  mutate(
    lon_prev = lag(longitude),
    lat_prev = lag(latitude),
    seg_meter = distHaversine(
      cbind(lon_prev, lat_prev),
      cbind(longitude, latitude)
    )
  ) %>%
  summarise(
    total_awb         = n(),                           # jumlah resi per kurir
    total_distance_km = sum(seg_meter, na.rm = TRUE)/1000
  ) %>%
  ungroup()

# 4. URUTKAN KURIR DARI AWB TERBESAR (BIAR MIRIP GRAFIK CONTOH) ----------
data_kurir <- data_kurir %>%
  arrange(desc(total_awb)) %>%
  mutate(
    `205_id` = factor(`205_id`, levels = `205_id`)
  )

# 5. HITUNG SCALE FACTOR UNTUK SECONDARY AXIS -----------------------------
scale_factor <- max(data_kurir$total_awb, na.rm = TRUE) /
  max(data_kurir$total_distance_km, na.rm = TRUE)

# 6. PLOT: TOTAL Paket (BIRU) + TOTAL DISTANCE (MERAH PUTUS-PUTUS) ---------
ggplot(data_kurir, aes(x = `205_id`, group = 1)) +
  # Total Paket
  geom_line(aes(y = total_awb, colour = "Total Paket"), size = 1) +
  geom_point(aes(y = total_awb, colour = "Total Paket"), size = 1.5) +
  
  # Total Distance (diskalakan ke sumbu kiri)
  geom_line(aes(y = total_distance_km * scale_factor,
                colour = "Total Jarak (km)"),
            linetype = "dashed", size = 1) +
  
  # Sumbu-Y kiri & kanan
  scale_y_continuous(
    name = "Total Paket",
    sec.axis = sec_axis(~ . / scale_factor, name = "Total Jarak (km)")
  ) +
  
  # Warna & legend
  scale_colour_manual(
    name   = "Metrik",
    values = c("Total Paket" = "blue",
               "Total Jarak (km)" = "red")
  ) +
  
  labs(
    title = "Perbandingan Total Pengiriman (Paket) dan Jarak Tempuh Kurir",
    x     = "NIK Kurir"
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 70, hjust = 1, vjust = 1),
    legend.position = "right"
  )
