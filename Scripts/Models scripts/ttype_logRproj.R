setwd("~/Work/Research/Astronomy/Projects/environmental-quenching/Scripts/Models scripts/")

library(data.table)
library(binom)
library(ggplot2)
library(InformationValue)
library(caret)
library(dplyr)

source("~/Work/Research/Astronomy/Projects/environmental-quenching/Scripts/Themes/my_theme.R")

df    <- fread("dados.csv")
df <- df[-which(df$logvelDisp_e < log10(50)),]
df$SF <- factor(df$SF)

sigma_breaks <- c(min(df$logvelDisp_e), log10(130), log10(150), max(df$logvelDisp_e))  # Defina aqui os limites dos bins de sigma

# Dividir os dados em bins e calcular a média em cada bin
data_binned <- df %>%
  mutate(TType_bin = cut(TType, breaks = 100),
         logRproj_rvir_bin = cut(logRproj_rvir, breaks = 100),
         sigma_bin = cut(logvelDisp_e, breaks = sigma_breaks, include.lowest = TRUE)) %>%
  group_by(TType_bin, logRproj_rvir_bin, sigma_bin, SF) %>%
  summarise(median_TType = median(TType),
            median_logRproj_rvir = median(logRproj_rvir)) %>%
  ungroup()

# Plotar o gráfico
ggplot(data_binned, aes(x = median_logRproj_rvir, y = median_TType, color = factor(SF))) +
  geom_smooth(aes(group = SF)) +
  facet_grid(. ~ sigma_bin) + 
  labs(x = "logRproj", y = "t-type", color = "Class") +
  scale_color_manual(values = c("red", "blue")) 

