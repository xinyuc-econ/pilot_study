#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 11-23-25                                                     
#                                                                  
# Construct state by year PIT measures using TAXSIM output
# Alternative BLS wage measure
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

taxsim_out_airline_median <- read_dta("clean_data/taxsim_output/taxsim_out_53-2011median.dta")
taxsim_out_airline_mean <- read_dta("clean_data/taxsim_output/taxsim_out_53-2011mean.dta")
taxsim_out_commercial_median <- read_dta("clean_data/taxsim_output/taxsim_out_53-2012median.dta")
taxsim_out_commercial_mean <- read_dta("clean_data/taxsim_output/taxsim_out_53-2012mean.dta")


# Construct different tax measures
# 1. srate
# 2. ASTR
# 3. ATR

process_taxsim_out <- function(data, data_name) {
  data |> 
    select(year, state, fiitax, siitax, v10, srate) |> 
    mutate(
      pilot_type = sub("^[^_]*_[^_]*_([^_]*).*", "\\1", data_name),
      percentile = sub(".*_", "", data_name),
      astr = siitax / v10,
      atr = (siitax + fiitax) / v10
    ) 
}

processed_airline_median <- process_taxsim_out(taxsim_out_airline_median, "taxsim_out_airline_median")
processed_airline_mean <- process_taxsim_out(taxsim_out_airline_mean, "taxsim_out_airline_mean")
processed_commercial_median <- process_taxsim_out(taxsim_out_commercial_median, "taxsim_out_commercial_median")
processed_commercial_mean <- process_taxsim_out(taxsim_out_commercial_mean, "taxsim_out_commercial_mean")

# Combine all processed data
all_processed <- bind_rows(
  processed_airline_median,
  processed_airline_mean,
  processed_commercial_median,
  processed_commercial_mean
)

# Import xwalk
xwalk <- read_csv("data/xwalks/irs_soi_fips_crosswalk.csv") |> 
  clean_names() |> 
  rename(state = irs_soi_code, fips = fips_code)

# replace the irs state code with fips code
all_processed <- all_processed |>
  left_join(xwalk, by = "state") |> 
  select(-state)

write_csv(all_processed, "clean_data/all_years_pit_bls.csv")

# all_processed_wider <- all_processed |> 
#   select(year, fips, srate, astr, atr, percentile) |>
#   pivot_wider(names_from = percentile, values_from = c(srate, astr, atr))
# 
# write_csv(all_processed_wider, "clean_data/all_years_pit_wide.csv")


