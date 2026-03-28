#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 07-14-25                                                     
#                                                                  
# Check coverage of Airmen Certification data
#                                                              
#-------------------------------------------------------------- 

library(tidyverse)
library(janitor)
library(haven)
library(readxl)
library(ggrepel)
library(scales)
library(patchwork)
library(ggstats)
library(viridis)

# # read in the main cleaned ATR pilots data
# pilot_data <- read_csv("data/processed_airmen_data/pooled_raw_pilot_data.csv", na = c("", "NA")) |> 
#   relocate(c(type, level, expire_date, starts_with("rating")), .after = med_exp_date) |> 
#   filter(year != 2025)
# 
# # Number of ATR pilots by year in my dataset
# airmen <- pilot_data |> 
#   filter(type == "P" & level == "A") |> 
#   group_by(year) |> 
#   count() |> 
#   mutate(data_type = "airmen")

# write_csv(airmen, "data/processed_airmen_data/airmen_atr_coverage.csv")

# Read in the number of ATR pilots by year in my dataset
airmen <- read_csv("data/processed_airmen_data/airmen_atr_coverage.csv") 

# Number of ATR pilots in the official FAA aggregate data
faa <- read_excel("data/raw_airmen_aggregate_stats/faa_atr_coverage.xlsx")

# Merge the two datasets
coverage <- rbind(airmen, faa)

label_info <- coverage |>
  filter(year == "2019") |> 
  mutate(
    label = case_when(
      data_type == "airmen" ~ "Airmen Certificate Data",
      data_type == "faa" ~ "Official Aggregate Data"
    )
  ) 

coverage_count_plot <- coverage |> 
  ggplot(aes(x = year, y = n, color = data_type, shape = data_type)) +
  geom_point(size = 3) +
  geom_line(size = 1.2) +
  scale_y_continuous(
    name = NULL,
    labels = label_number(scale = 1/1000, suffix = "K"),
    breaks = seq(100000, 180000, 10000)
  ) +
  # rotate x-axis labels by 45 degrees
  scale_x_continuous(
    name = NULL,
    breaks = seq(2007, 2024, 1)
  ) +
  geom_text(
    data = label_info,
    aes(label = label, x = year + 3 , y = n + 5000),
    fontface = "bold", size = 6, hjust = "right", vjust = "bottom"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
    axis.text.y = element_text(size = 17),
    plot.title = element_text(size = 25),
    legend.position = "none"
    ) 
  
ggsave("output_graphs/airmen_atr_coverage_count.png", width = 8, height = 6.5)


# pivot coverage data to wider
# Calculate the percentage of coverage
coverage_wide <- coverage |> 
  pivot_wider(
    names_from = data_type,
    values_from = n
  ) |> 
  mutate(
    coverage = airmen / faa 
  )


# Create a bar plot of the coverage percentage
coverage_perc_plot <- coverage_wide |> 
  filter(!is.na(coverage)) |>
  ggplot(aes(x = as.factor(year), y = coverage)) +
  geom_col() +
  scale_y_continuous(
    name = NULL,
    labels = label_percent(scale = 100, accuracy = 1),
    breaks = seq(0, 1, 0.1)
  ) +
  scale_x_discrete(
    name = NULL,
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 17),
    axis.text.y = element_text(size = 17),
    plot.title = element_text(size = 25)
    ) 
  

ggsave("output_graphs/airmen_atr_coverage_percentage.png", width = 8, height = 6.5)



coverage_count_plot + coverage_perc_plot


ggsave("output_graphs/airmen_atr_coverage_both.png", 
       width = 16, height = 6.5, dpi = 300)





