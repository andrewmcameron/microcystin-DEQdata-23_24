# ## new set of monthly box plots for CHLa, MC, TN and TP.
# Last time we made these separately by year (see attached), but I think it would be better to have a single set of 4 months (i.e., averaging across years).
# I would also like to get updated versions of the scatterplots: CHLa vs TN, CHLa vs. TP, TN vs. TP, MC vs CHLa, MC vs. TN and MC vs. CHLa (with model lines shown, confidence intervals as shading, p values and R2 values).  These are all based on ordinary least squares regression.
# I would like to get one set of plots that is based on the individual observations (i.e., each data point is a single station-month) and a second set of plots that is based on station-average values (averaged across months in a given year).  Note that for the latter, some of the stations will have 2 data points because they were sampled in both years.

source("C:/Users/andre/my_functions.R")

library(tidyverse)

  data<- openxlsx::read.xlsx("data/received/Integrated Dataset 2023-24 Reservoir Microcystin Survey.xlsx", sheet = 4, startRow = 2)
  
 data <-  data %>%
  select(1:9) %>%
  mutate(Date = convertExcelDateTime(data, "Date")) %>%
  mutate(month = factor(month(Date), levels = 1:12, labels = month.abb))


#Boxplots
library(ggplot2)
#CHLa
chla_data <- data %>%
  filter(!is.na(CHLa),
         month %in% c(6:9)
         ) %>%
  group_by(StationID, month) %>%
  summarize(mean = mean(CHLa, na.rm = TRUE))
            
ggplot(chla_data, aes(x = month, y = mean)) +
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

ggsave("figures/boxplot_CHLa.jpg", width = 8, height = 6, dpi = 500)

#TN
data_wide %>%
  filter(month %in% 5:9) %>%
  filter(!is.na(TN)) %>%
  ggplot(aes(x = month_year, y = TN)) +
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

ggsave("figures/boxplot_TN.jpg", width = 8, height = 6, dpi = 500)

#TP
data_wide %>%
  filter(month %in% 5:9) %>%
  filter(!is.na(TP)) %>%
  ggplot(aes(x = month_year, y = TP, color = year_group)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.45, size = 0.7) + 
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "grey30") +
  labs(title = NULL, 
       x = NULL, 
       y = "TP (mg L\u207B\u00B9)") +
  scale_color_manual(values = c("2023" = "#08415C", "2024" = "salmon3")) + # Customize colors
  theme_minimal() +
  theme(legend.position = "none",
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),    
        axis.title = element_text(size = 14))

ggsave("figures/boxplot_TP.jpg", width = 8, height = 6, dpi = 500)

#Data for export
export_data <- data_wide %>%
  select(-month, -year, -month_year) %>%
  rename(DateTime = CollectionDateTime,
         Depth = SamplingDepth)  