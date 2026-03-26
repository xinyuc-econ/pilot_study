#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 11-23-25                                                     
#                                                                  
# Merge pilots data with tax rate data
# Produce scatterplots and simple OLS results
#                                                              
#-------------------------------------------------------------- 
library(tidyverse)
library(janitor)
library(haven)
library(readxl)
library(ggrepel)
library(stargazer)
library(xtable)
library(binsreg)

# read in data 
prop_pilots <- read_csv("clean_data/sum_stat_prop_atr_pilots.csv") |> filter(year >= 2009, year <= 2022)

prop_pilots |> distinct(year)

# median airline pilot wage
pit <- read_csv("clean_data/all_years_pit_bls.csv") |> 
  filter(pilot_type == "airline", percentile == "median") |> 
  select(year, fips, starts_with("astr"), starts_with("atr"))

state_fip_xwalk <- read_excel("data/xwalks/StateFIPSicsprAB.xls") |> 
  clean_names() |> 
  select(fips, name, ab) |> 
  rename(
    statefull = name,
    state = ab
  )

pit_processed <- pit |> 
  left_join(state_fip_xwalk) |> 
  select(-fips, -statefull)

m <- prop_pilots |> 
  left_join(pit_processed) |> 
  mutate(
    astr = astr*100,
    atr = atr*100
  )


# Binned scatterplot: relationship between proportion of pilots and average state tax rate, all years pooled
binscatter_m <- m |> 
  as.data.frame() 


# baseline: no AK no HI
p_noAKHI_astr <- binsreg(prop_atr_pilots, astr,
                  data = binscatter_m |> filter(state != "AK" & state != "HI"),
                  polyreg = 1)

p_noAKHI_astr$bins_plot + 
  scale_y_continuous(
    limits = c(0.04, 0.12),
    breaks = seq(0.04, 0.12, 0.02)
  ) +
  labs(
    x = "Average state tax rate (%)",
    y = "Share of pilots in tot. working pop. (%)",
    caption = "Notes: tax measure simulated using median wage of airline pilots"
    ) +
  theme(
    axis.title  = element_text(size = 24),
    axis.text = element_text(size = 20),
    strip.text.x = element_text(size = 28),
    plot.caption  = element_text(size = 14)
  )  

ggsave("output_graphs/binscatter_astr_pilots_noAKHI_bls.png", width = 10, height = 8)

p_noAKHI_atr <- binsreg(prop_atr_pilots, atr,
                    data = binscatter_m |> filter(state != "AK" & state != "HI"),
                    polyreg = 1)

p_noAKHI_atr$bins_plot + 
  scale_x_continuous(
    limits = c(19, 29),
    breaks = seq(19, 29, 2)
  ) +
  scale_y_continuous(
    limits = c(0.04, 0.12),
    breaks = seq(0.04, 0.12, 0.02)
  ) +
  labs(
    x = "Average tax rate (%)",
    y = "Share of pilots in tot. working pop. (%)",
    caption = "Notes: tax measure simulated using median wage of airline pilots"
    ) +
  theme(
    axis.title  = element_text(size = 24),
    axis.text = element_text(size = 20),
    strip.text.x = element_text(size = 28),
    plot.caption  = element_text(size = 14)
  )  

ggsave("output_graphs/binscatter_atr_pilots_noAKHI_bls.png", width = 10, height = 8)

