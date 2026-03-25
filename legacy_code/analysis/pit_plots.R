#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 03-26-25                                                     
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
pit <- read_csv("clean_data/all_years_pit_wide.csv") 

# read in state FIPS code and abbrev crosswalk
xwalk <- read_excel("data/xwalks/StateFIPSicsprAB.xls") |> 
  clean_names() |> 
  select(fips, ab) |>
  rename(state = ab)

# prep the pit data for plotting
pit_binned <- pit |> 
  left_join(xwalk) |> 
  filter(year >= 2009) |> 
  select(year, state, srate_p95, astr_p95, atr_p95, -fips) |>
  mutate(
    astr_p95 = astr_p95 * 100, # convert to percentage
    atr_p95 = atr_p95 * 100, # convert to percentage
    srate_p95_binned = cut(srate_p95, breaks = c(0, 3, 5, 7, 10), right = FALSE),
    astr_p95_binned = cut(astr_p95, breaks = c(0, 3, 5, 7, 10), right = FALSE),
    atr_p95_binned = cut(atr_p95, breaks = c(15, 20, 25, 30, 35), right = FALSE)
  )

# summary stats by year
pit_binned |>
  group_by(year) |> 
  summarize(
    mean_srate = mean(srate_p95, na.rm = TRUE),
    sd_srate = sd(srate_p95, na.rm = TRUE),
    mean_astr = mean(astr_p95, na.rm = TRUE),
    sd_astr = sd(astr_p95, na.rm = TRUE),
    mean_atr = mean(atr_p95, na.rm = TRUE),
    sd_atr = sd(atr_p95, na.rm = TRUE)
  ) 
  
summary(pit_binned)

sum_stat_dta <- pit_binned |> 
  select(year, srate_p95, astr_p95, atr_p95) |> 
  as.data.frame()


# Summary stats table
stargazer(sum_stat_dta, title="Summary Statistics: 3 measures of PIT for a 95th income percentile taxpayer", digits=2, 
          covariate.labels=c("Year","Marginal state tax rate", "Ave. state tax rate", "Ave. tax rate"),
          out="output_tables/sum_stat_pit.tex",
          notes = "Notes: Measures constructed using NBER TAXSIM.",
          notes.align = "l",
          font.size = "small"     
)


# Map plot on marginal state tax rate for a 95th income percentile taxpayer, 2009-2022
plot_usmap(
  data = pit_binned,
  values = "srate_p95"
) +
  scale_fill_viridis_c(alpha = 0.9, direction = -1) +
  facet_wrap(~ year) +
  labs(
    fill = "",
    title = "Marginal state tax rate, 95th income percentile, 2009-2022"
  ) +
  theme(
    legend.position = c(.6, .03),
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size = 17)
  ) 

ggsave("output_graphs/srate_maps.png")

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


