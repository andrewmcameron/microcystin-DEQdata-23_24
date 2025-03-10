# Script to generate new versions of boxplots created figures, using a new version of the data set following additional QC by Paul.
  ## 

# DELIVERABLE
# new set of monthly box plots for CHLa, MC, TN and TP.
  ## single set of 4 months (i.e., averaging across years).


source("C:/Users/andre/my_functions.R")

library(tidyverse)
library(ggplot2)

  data<- openxlsx::read.xlsx("data/received/Integrated Dataset 2023-24 Reservoir Microcystin Survey.xlsx", sheet = 4, startRow = 2)
  
 data <-  data %>%
  select(1:10) %>%
  mutate(Date = convertExcelDateTime(data, "Date")) %>%
  mutate(month = factor(month(Date), levels = 1:12, labels = month.abb))

 
 # summary statistics
 # mean by month
 summary_stats <- data %>%
   filter(!is.na(month)) %>%
   filter(month(Date) %in% c(6:9)) %>%
   group_by(month) %>%
   summarize(median_CHLa = median(CHLa, na.rm = TRUE),
             median_TN = median(TN, na.rm = TRUE),
             median_TP = median(as.numeric(TP), na.rm = TRUE),
             median_MC = median(MC, na.rm = TRUE))
 write_csv(summary_stats, "data/derived/boxplot_medianByMonth.csv")

 # ------------------ ! BOX PLOTS ! ------------------------------
#CHLa
chla_data <- data %>%
  filter(!is.na(CHLa),
         month %in% c("Jun", "Jul", "Aug", "Sep")
         ) %>%
  group_by(StationID, month) %>%
  summarize(mean = mean(CHLa, na.rm = TRUE))
            
p <- ggplot(chla_data, aes(x = month, y = mean)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.45, size = 0.7) + 
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "grey30") +
  labs(title = NULL, 
       x = NULL, 
       y = "CHLa (\u03BCg L\u207B\u00B9)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),    
        axis.title = element_text(size = 14))

#ggsave("figures/boxplot_CHLa_mean.jpg", width = 8, height = 6, dpi = 500)
saveRDS(p, "data/R_data/boxplot_CHLa_mean.rds")

#TN
tn_data <- data %>%
  filter(!is.na(TN),
         month %in% c("Jun", "Jul", "Aug", "Sep")
  ) %>%
  group_by(StationID, month) %>%
  summarize(mean = mean(TN, na.rm = TRUE))

p <- tn_data %>%
  ggplot(aes(x = month, y = mean)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.45, size = 0.7) + 
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "grey30") +
  labs(title = NULL, 
       x = NULL, 
       y = "TN (mg L\u207B\u00B9)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),    
        axis.title = element_text(size = 14))

#ggsave("figures/boxplot_TN_mean.jpg", width = 8, height = 6, dpi = 500)
saveRDS(p, "data/R_data/boxplot_TN_mean.rds")

#TP
tp_data <- data %>%
  filter(!is.na(TP),
         month %in% c("Jun", "Jul", "Aug", "Sep")
  ) %>%
  mutate(TP = as.numeric(TP)) %>%
  group_by(StationID, month) %>%
  summarize(mean = mean(TP, na.rm = TRUE))

p <- tp_data %>%
  ggplot(aes(x = month, y = mean)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.45, size = 0.7) + 
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "grey30") +
  labs(title = NULL, 
       x = NULL, 
       y = "TP (mg L\u207B\u00B9)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),    
        axis.title = element_text(size = 14))

#ggsave("figures/boxplot_TP_mean.jpg", width = 8, height = 6, dpi = 500)
saveRDS(p, "data/R_data/boxplot_TP_mean.rds")

#MC
mc_data <- data %>%
  filter(!is.na(MC),
         month %in% c("Jun", "Jul", "Aug", "Sep")
  ) %>%
  mutate(MC = as.numeric(MC)) %>%
  group_by(StationID, month) %>%
  summarize(mean = mean(MC, na.rm = TRUE))

p <- mc_data %>%
  ggplot(aes(x = month, y = mean)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.45, size = 0.7) + 
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "grey30") +
  labs(title = NULL, 
       x = NULL, 
       y = "MC (\u03BCg L\u207B\u00B9)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),    
        axis.title = element_text(size = 14))

#ggsave("figures/boxplot_MC_mean.jpg", width = 8, height = 6, dpi = 500)
saveRDS(p, "data/R_data/boxplot_MC_mean.rds")

#Data for export
export_data <- data %>%
  select(-month)


#write_csv(export_data, "data/StationObs_JunetoSep_20250223.csv")




