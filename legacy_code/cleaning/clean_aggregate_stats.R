#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 07-15-25                                                     
#                                                                  
# Construct dataset for official FAA aggregate stats from 2016-2024
#                                                              
#-------------------------------------------------------------- 

library(tidyverse)
library(janitor)
library(haven)
library(readxl)
library(ggrepel)
library(stargazer)
library(xtable)
library(purrr)
library(usmap)
library(viridis)
library(sf)
  
# Define the years of interest
# 2024 has a slightly different format, so let's handle it later
years <- 2016:2023

# Define a function to read and process a single year's file
read_atr_data <- function(y) {
  file_path <- paste0("data/raw_airmen_aggregate_stats/", y, "-civil-airmen-stats.xlsx")
  
  read_excel(file_path, sheet = "Table 5", skip = 4) |> 
    clean_names() |> 
    select(faa_region_and_state, airline_transport_1) |> 
    rename(
      statefull = faa_region_and_state,
      atr_pilots = airline_transport_1
    ) |> 
    mutate(year = y)
}


# Read and combine all years
atr_all_years <- map_dfr(years, read_atr_data)


# Now add in 2024 data
d_2024 <- read_excel("data/raw_airmen_aggregate_stats/2024-civil-airmen-stats.xlsx", 
                     sheet = "T5", skip = 4) |> 
  clean_names() |> 
  select(faa_region_and_state, airline_transport_1) |> 
  rename(
    statefull = faa_region_and_state,
    atr_pilots = airline_transport_1
  ) |> 
  mutate(year = 2024)

# Combine the 2024 data with the previous years
atr_all_years <- bind_rows(atr_all_years, d_2024)


# Check the distinct statefull values
atr_all_years |> 
  distinct(statefull) |> 
  print(n = Inf)


# Read in xwalk for 51 states
xwalks <- read_excel("data/xwalks/StateFIPSicsprAB.xls") |> 
  clean_names() |> 
  rename(
    statefull = name
  ) |>
  select(statefull, fips, ab)


# Use the xwalk to filter out the 51 states
allstates_atr_all_years <- atr_all_years |> 
  mutate(
    statefull = if_else(statefull == "Alaskan Region--Total", "Alaska", statefull)
  ) |> 
  filter(statefull %in% xwalks$statefull) |>
  left_join(xwalks, by = "statefull") |> 
  rename(
    state = ab,
    faa_n_atr_pilots = atr_pilots
  ) |> 
  relocate(year, statefull, state, fips, faa_n_atr_pilots)


# save the FAA data
write_csv(allstates_atr_all_years, "clean_data/faa_sum_stat_prop_atr_pilots.csv")

