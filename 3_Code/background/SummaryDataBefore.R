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
