# Load required libraries
library(tidyverse)
library(corrplot)
library(Hmisc)

# 1. Read and clean dataset
data <- read.csv("AOD_CAMS_merged.csv", stringsAsFactors = FALSE)
colnames(data) <- trimws(colnames(data))

# 2. Select variables and assign formatted publication titles
num_data <- data %>% 
  select(
    AOD, PM25, PM10, SO2, NO, NO2, NOX, CO, O3,
    Temp, RH, WindSpeed, SolarRad, BP, Rain_daily_total
  ) %>%
  rename(
    "AOD"                 = "AOD",
    "PM2.5"               = "PM25",
    "PM10"                = "PM10",
    "SO2"                 = "SO2",
    "NO"                  = "NO",
    "NO2"                 = "NO2",
    "NOx"                 = "NOX",
    "CO"                  = "CO",
    "O3"                  = "O3",
    "Temperature"         = "Temp",
    "Relative Humidity"   = "RH",
    "Wind Speed"          = "WindSpeed",
    "Solar Radiation"     = "SolarRad",
    "Barometric Pressure" = "BP",
    "Daily Rainfall"      = "Rain_daily_total"
  )

# 3. Calculate Pearson correlation coefficients and p-values
corr_res <- rcorr(as.matrix(num_data), type = "pearson")
r_matrix <- corr_res$r
p_matrix <- corr_res$P
p_matrix[is.na(p_matrix)] <- 1

# 4. Generate high-resolution PNG with zero border padding
png("Correlation_Matrix5.png", width = 2800, height = 2800, res = 300)

# Set tight graphical device parameters (minimal margins)
par(mar = c(0.2, 0.2, 0.2, 0.2), xpd = TRUE)

corrplot(
  r_matrix,
  p.mat = p_matrix,
  method = "circle",
  type = "lower",
  sig.level = c(0.001, 0.01, 0.05),
  insig = "label_sig",
  pch.cex = 0.9,
  pch.col = "black",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 0.9,
  cl.cex = 0.85,
  diag = FALSE,
  col = colorRampPalette(c("#3B4CC0", "#8CBD8F", "#FFFFFF", "#F7A889", "#B40426"))(200),
  #title = "Pearson Correlation Matrix of Atmospheric & Meteorological Variables",
  mar = c(0, 0, 1.2, 0)
)

dev.off()

