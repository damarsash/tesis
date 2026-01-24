#install.packages(c("writexl"))

library(readxl)
library(sf)
library(dplyr)
library(writexl)

# Matikan s2
sf_use_s2(FALSE)

# =========================
# 1. Load Excel
# =========================
df <- read_excel(
  "D:/IPB/TESIS/PENELITIAN/Dataset/AFTER PRAPROCESS/CLBT0_21JUL_20AUG.xlsx",
  sheet = "Raw"
) %>%
  mutate(
    latitude = latitude / 1e6,
    longitude = longitude / 1e6
  ) %>%
  filter(!is.na(longitude), !is.na(latitude))

# =========================
# 2. Konversi ke POINT sf
# =========================
points_sf <- st_as_sf(
  df,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
)

# =========================
# 3. Load & perbaiki polygon
# =========================
polygon_text <- readLines("D:/IPB/TESIS/PENELITIAN/CODE/source/polygon.txt")

polygons_sf <- st_as_sfc(polygon_text, crs = 4326) %>%
  st_make_valid() %>%
  st_sf(geometry = .)

# =========================
# 4. Spatial filtering
# =========================
points_in_polygon <- st_join(
  points_sf,
  polygons_sf,
  join = st_within,
  left = FALSE
)

# =========================
# 5. Simpan hasil
# =========================
result <- st_drop_geometry(points_in_polygon)

write_xlsx(
  result,
  "D:/IPB/TESIS/PENELITIAN/CODE/output/hasil_filter_polygon.xlsx"
)

cat("Total awal :", nrow(df), "\n")
cat("Di polygon :", nrow(result), "\n")
