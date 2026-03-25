#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 03-19-25                                                     
#                                                                  
# Construct state by year PIT measures using TAXSIM output
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

taxsim_out_p90 <- read_dta("clean_data/taxsim_output/taxsim_out_p90.dta")
taxsim_out_p95 <- read_dta("clean_data/taxsim_output/taxsim_out_p95.dta")
taxsim_out_p96 <- read_dta("clean_data/taxsim_output/taxsim_out_p96.dta")
taxsim_out_p97 <- read_dta("clean_data/taxsim_output/taxsim_out_p97.dta")
taxsim_out_p98 <- read_dta("clean_data/taxsim_output/taxsim_out_p98.dta")
taxsim_out_p99 <- read_dta("clean_data/taxsim_output/taxsim_out_p99.dta")

# Construct different tax measures
# 1. srate
# 2. ASTR
# 3. ATR

process_taxsim_out <- function(data, data_name) {
  data |> 
    select(year, state, fiitax, siitax, v10, srate) |> 
    mutate(
      percentile = sub(".*_", "", data_name),
      astr = siitax / v10,
      atr = (siitax + fiitax) / v10
    ) 
}

processed_p90 <- process_taxsim_out(taxsim_out_p90, "taxsim_out_p90")
processed_p95 <- process_taxsim_out(taxsim_out_p95, "taxsim_out_p95")
processed_p96 <- process_taxsim_out(taxsim_out_p96, "taxsim_out_p96")
processed_p97 <- process_taxsim_out(taxsim_out_p97, "taxsim_out_p97")
processed_p98 <- process_taxsim_out(taxsim_out_p98, "taxsim_out_p98")
processed_p99 <- process_taxsim_out(taxsim_out_p99, "taxsim_out_p99")

# Combine all processed data
all_processed <- bind_rows(
  processed_p90,
  processed_p95,
  processed_p96,
  processed_p97,
  processed_p98,
  processed_p99
)

# Import xwalk
xwalk <- read_csv("data/xwalks/irs_soi_fips_crosswalk.csv") |> 
  clean_names() |> 
  rename(state = irs_soi_code, fips = fips_code)

# replace the irs state code with fips code
all_processed <- all_processed |>
  left_join(xwalk, by = "state") |> 
  select(-state)

write_csv(all_processed, "clean_data/all_years_pit.csv")

all_processed_wider <- all_processed |> 
  select(year, fips, astr, atr, percentile) |>
  pivot_wider(names_from = percentile, values_from = c(srate, astr, atr))

write_csv(all_processed_wider, "clean_data/all_years_pit_wide.csv")


