#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 11-22-25                                                     
#
# Descriptive graphs on PIT
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
library(usmap)
library(stargazer)

# read in processed taxsim output data
pit <- read_csv("clean_data/all_years_pit_bls.csv") 

# read in state FIPS code and abbrev crosswalk
xwalk <- read_excel("data/xwalks/StateFIPSicsprAB.xls") |> 
  clean_names() |> 
  select(fips, ab) |>
  rename(state = ab)

# prep the pit data for plotting
pit_for_plot <- pit |> 
  left_join(xwalk) |> 
  select(pilot_type, percentile, year, state, srate, astr, atr, -fips) |>
  mutate(
    astr = astr * 100, # convert to pp
    atr = atr * 100 
  )

pit_airline_mean <- pit_for_plot |> 
  filter(pilot_type == "airline", percentile == "mean")

pit_airline_median <- pit_for_plot |> 
  filter(pilot_type == "airline", percentile == "median")

pit_commercial_mean <- pit_for_plot |> 
  filter(pilot_type == "commercial", percentile == "mean")

pit_commercial_median <- pit_for_plot |> 
  filter(pilot_type == "commercial", percentile == "median")


# # summary stats by year
# pit_binned |>
#   group_by(year) |> 
#   summarize(
#     mean_srate = mean(srate_p95, na.rm = TRUE),
#     sd_srate = sd(srate_p95, na.rm = TRUE),
#     mean_astr = mean(astr_p95, na.rm = TRUE),
#     sd_astr = sd(astr_p95, na.rm = TRUE),
#     mean_atr = mean(atr_p95, na.rm = TRUE),
#     sd_atr = sd(atr_p95, na.rm = TRUE)
#   ) 
# 
# summary(pit_binned)
# 
# sum_stat_dta <- pit_binned |> 
#   select(year, srate_p95, astr_p95, atr_p95) |> 
#   as.data.frame()
# 
# 
# # Summary stats table
# stargazer(sum_stat_dta, title="Summary Statistics: 3 measures of PIT for a 95th income percentile taxpayer", digits=2, 
#           covariate.labels=c("Year","Marginal state tax rate", "Ave. state tax rate", "Ave. tax rate"),
#           out="output_tables/sum_stat_pit.tex",
#           notes = "Notes: Measures constructed using NBER TAXSIM.",
#           notes.align = "l",
#           font.size = "small"     
# )


# Map plot on marginal state tax rate for a 95th income percentile taxpayer, 2009-2022
plot_usmap(
  data = pit_airline_mean,
  values = "atr"
) +
  scale_fill_viridis_c(alpha = 0.9, direction = -1) +
  facet_wrap(~ year) +
  labs(
    fill = "",
    title = ""
  ) +
  theme(
    legend.position = c(.6, .03),
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size = 17)
  ) 

ggsave("output_graphs/atr_maps_bls.png")

pit |> filter(year > 2016) |> 
  filter(pilot_type == "airline", percentile == "mean") |> 
  arrange(fips)

# Map plot on average state tax rate for a 95th income percentile taxpayer, 2009-2022
plot_usmap(
  data = pit_binned,
  values = "astr_p95"
) +
  scale_fill_viridis_c(alpha = 0.9, direction = -1) +
  facet_wrap(~ year) +
  labs(
    fill = "",
    title = "Average state tax rate, 95th income percentile, 2009-2022"
  ) +
  theme(
    legend.position = c(.6, .03),
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size = 17)
  ) 

ggsave("output_graphs/astr_maps.png")


# Map plot on average tax rate for a 95th income percentile taxpayer, 2009-2022
plot_usmap(
  data = pit_binned,
  values = "atr_p95"
) +
  scale_fill_viridis_c(alpha = 0.9, direction = -1) +
  facet_wrap(~ year) +
  labs(
    fill = "",
    title = "Average tax rate, 95th income percentile, 2009-2022"
  ) +
  theme(
    legend.position = c(.55, .03),
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size = 17)
  ) 

ggsave("output_graphs/atr_maps.png")


