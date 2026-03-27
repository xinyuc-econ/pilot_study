#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 04-04-25                                                     
#                                                                  
# Simpler OLS reg with binary outcome for moving or not
#                                                              
#-------------------------------------------------------------- 

library(tidyverse)
library(janitor)
library(haven)
library(readxl)
library(ggrepel)
library(stargazer)
library(xtable)
library(fixest)
library(marginaleffects)

# read in the main cleaned ATR pilots data
pilots <- read_csv("clean_data/main_US_pilots_atr.csv")

xwalk <- read_excel("data/xwalks/StateFIPSicsprAB.xls") |> 
  clean_names() |> 
  select(fips, ab) |> 
  rename(origin_state = ab, origin_fips = fips)

pit <- read_csv("clean_data/all_years_pit.csv") |> 
  filter(percentile == "p95") |> 
  select(year, fips, srate, atr, astr) 

# subset to relevant variables
subset_pilots <- pilots |> 
  select(year, fips, statefull, state, unique_id, first_name, last_name, num_years) 

# compute within state tax changes over time
atr_changes <- pit |> 
  filter(year %in% c(2009, 2010, 2011, 2014, 2015, 2016, 2017, 2019, 2022)) |>
  select(year, fips, atr) |> 
  mutate(atr = round(atr, 4)) |>
  arrange(year) |> 
  arrange(fips) |> 
  group_by(fips) |>
  mutate(
    atr_change = (log(atr) - log(lag(atr)))*100, # percentage change in atr
  ) 

# prepare another atr dataset for merging with origin states
origin_atr_changes <- atr_changes |>
  rename(origin_fips = fips, origin_atr = atr, origin_atr_change = atr_change)

# identify pilots who moved state-to-state
all_pilots <- subset_pilots |> 
  left_join(atr_changes) |> # merge in atr changes for the destination states
  arrange(year) |> 
  arrange(unique_id) |> 
  group_by(unique_id) |> 
  rename(
    dest_state = state, # state the pilot lives in year t is the destination state
    dest_atr = atr, # ATR in year t
    dest_atr_change = atr_change, # ATR change in year t
  ) |> 
  mutate(
    origin_state = lag(dest_state), # state the pilot lived in year t-h is the origin state
    moved = ifelse(dest_state != origin_state, 1, 0) # if destination state is different from origin state, then the pilot moved
  ) |> 
  ungroup()


# construct a dataset that indicate who are the movers, origin & destination states, and changes in the ATR of origin & destination states
processed_all_pilots <- all_pilots |> 
  left_join(xwalk) |> 
  left_join(origin_atr_changes) 


write_csv(processed_all_pilots, "clean_data/mover_analysis_all_pilots.csv")


# filter out the first year where a pilot appears, bc by construction we don't observe the origin state
# filter out 2024, because there is no PIT data
test_data <- processed_all_pilots |> 
  filter(!is.na(origin_fips) & (year != 2024))  


test_data |> 
  summary()

o1 <- feols(moved ~ origin_atr_change | year + origin_state + dest_state, 
            data = test_data,
            cluster = ~ unique_id)

summary(o1)

o2 <- feols(moved ~ dest_atr_change | year + dest_state, 
            data = test_data,
            cluster = ~ unique_id)

summary(o2)


test_data_consec_years <- test_data |> 
  filter(year %in% c(2010, 2011, 2015, 2016, 2017)) 


c_o1 <- feols(moved ~ origin_atr_change | year + origin_state, 
            data = test_data_consec_years,,
            cluster = ~ unique_id)

summary(c_o1)

c_o2 <- feols(moved ~ dest_atr_change | year + dest_state, 
            data = test_data_consec_years,
            cluster = ~ unique_id)

summary(c_o2)








