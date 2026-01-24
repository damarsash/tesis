library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)

# --- 1. Baca data utama ---
data <- read_excel("D:/IPB/TESIS/PENELITIAN/CODE/source/CLBT0_21JUL_20AUG.xlsx", 
                   sheet = "Raw")

#====DATA SELECTION==========
data_ori <- read_excel("D:/IPB/TESIS/PENELITIAN/CODE/source/CLBT0_21JUL_20AUG.xlsx", 
                   sheet = "Raw")

summary(data_ori)
dim(data_ori)
variable_names <- names(data_ori)
print(variable_names)

# See the data types of variables (column)
str(data_ori)
head(data_ori)
#==========================    


# --- 2. Hitung jumlah AWB per 205_name ---
awb_per_205 <- data %>%
  count(`205_id`, name = "awb_count") %>%
  arrange(desc(awb_count))

# Hitung statistik deskriptif
mean_awb <- mean(awb_per_205$awb_count)
sd_awb <- sd(awb_per_205$awb_count)
min_awb <- min(awb_per_205$awb_count)
max_awb <- max(awb_per_205$awb_count)

# Tampilkan hasil
cat("STATISTIK JUMLAH AWB PER 205_NAME:\n")
cat("Rata-rata        :", mean_awb, "\n")
cat("Standar deviasi  :", sd_awb, "\n")
cat("Minimal          :", min_awb, "\n")
cat("Maksimal         :", max_awb, "\n\n")



#=======HISTOGRAM=======
# --- Tentukan breaks dinamis ---
bin_width <- 250  # bisa kamu ganti: 10, 15, 25, dll

range_awb <- range(awb_per_205$awb_count, na.rm = TRUE)

breaks_awb <- seq(
  from = floor(range_awb[1] / bin_width) * bin_width,
  to   = ceiling(range_awb[2] / bin_width) * bin_width,
  by   = bin_width
)

# --- Buat label kelas (mis. "0–20", "21–40", dst) ---
labels_awb <- paste(
  head(breaks_awb, -1),      # batas bawah
  breaks_awb[-1],            # batas atas
  sep = "–"
)

# --- Tambahkan kelas interval ke awb_per_205 ---
awb_per_205 <- awb_per_205 %>%
  mutate(
    kelas_interval = cut(
      awb_count,
      breaks = breaks_awb,
      include.lowest = TRUE,
      right = TRUE,
      labels = labels_awb
    )
  )

# --- Hitung frekuensi per kelas ---
kelas_freq <- awb_per_205 %>%
  count(kelas_interval, name = "frekuensi")

# Pastikan faktor berurutan
kelas_freq$kelas_interval <- factor(
  kelas_freq$kelas_interval,
  levels = labels_awb,
  ordered = TRUE
)

# --- Cari kelas yang mengandung nilai mean_awb ---
mean_kelas <- awb_per_205 %>%
  filter(awb_count >= mean_awb) %>%
  slice_min(awb_count, n = 1) %>%
  pull(kelas_interval)

mean_kelas_index <- which(levels(kelas_freq$kelas_interval) == mean_kelas)

# --- Plot histogram kategori + garis rata-rata (berbasis kelas) ---
ggplot(kelas_freq, aes(x = kelas_interval, y = frekuensi)) +
  geom_col(fill = "steelblue", color = "black") +
  geom_vline(xintercept = mean_kelas_index,
             linetype = "dashed", color = "red", size = 1) +
  annotate(
    "text",
    x = mean_kelas_index + 0.2,
    y = max(kelas_freq$frekuensi, na.rm = TRUE),
    label = "Average",
    color = "red",
    hjust = 0, vjust = -0.5, size = 4
  ) +
  labs(
    title = "Histogram Total Package per Courier Based On Interval",
    x = "Total Package Class",
    y = "Frequency"
  ) +
  theme_minimal()
