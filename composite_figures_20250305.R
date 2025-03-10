# I would like to get a single figure that contains all 4 boxplots arranged 2x2 (CHLa and Mc in the top row, TN and TP in the bottom row).

# I would also like to get a composite figure (1x3) of the regressions for CHLa vs TP (top), CHLa vs. TN (middle) 
# and TP vs. TN (bottom) using the station-average values.

# I would like to get a composite of the regressions for MC vs CHLa (top), MC vs. TP (middle) and MC vs. TN (bottom) using 
# the station average values (note that where p is not significant, you do not need to include the R2 value).


library(ggplot2)
library(patchwork)

# Load plots
boxplot_CHLa <- readRDS("data/R_data/boxplot_CHLa_mean.rds")
boxplot_MC <- readRDS("data/R_data/boxplot_MC_mean.rds")
boxplot_TN <- readRDS("data/R_data/boxplot_TN_mean.rds")
boxplot_TP <- readRDS("data/R_data/boxplot_TP_mean.rds")

CHLa_vs_TN <- readRDS("data/R_data/CHLa_vs_TN_stationYrAvg.rds")
CHLa_vs_TP <- readRDS("data/R_data/CHLa_vs_TP_stationYrAvg.rds")
TN_vs_TP <- readRDS("data/R_data/TN_vs_TP_stationYrAvg.rds")

MC_vs_CHLa <- readRDS("data/R_data/MC_vs_CHLa_stationYrAvg.rds")
MC_vs_TN <- readRDS("data/R_data/MC_vs_TN_stationYrAvg.rds")
MC_vs_TP <- readRDS("data/R_data/MC_vs_TP_stationYrAvg.rds")

boxplot_composite <- (boxplot_CHLa + boxplot_MC) / (boxplot_TN + boxplot_TP)
ggsave("boxplots_composite.jpg", boxplot_composite, width = 8, height = 6, dpi = 600)


regression_composite <- CHLa_vs_TP / CHLa_vs_TN / TN_vs_TP
ggsave("regression_composite.jpg", regression_composite, width = 4.5, height = 8, dpi = 600)

regression_composite_MC <- MC_vs_CHLa / MC_vs_TN / MC_vs_TP
ggsave("regression_composite_MC.jpg", regression_composite_MC, width = 4.5, height = 8, dpi = 600)
