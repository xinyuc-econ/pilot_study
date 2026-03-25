#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 03-19-25                                                     
#                                                                  
# Prepare a dataset for TAXSIM
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


# SOI ---------------------------------------------------------------------


# Read in SOI data on the AGI percentile floor (in 2022 $)
soi <- read_excel("data/soi/soi_income_p.xlsx")

xwalk <-  read_csv("data/xwalks/irs_soi_fips_crosswalk.csv")

cpi <- read_csv("data/soi/CPI_U_2yr_Moving_Avg.csv") |> 
  clean_names() |> 
  select(year, cpi_u)

cpi_22 <- cpi |> filter(year == 2022) |> pull(cpi_u)

irs_soi_code <- xwalk |> clean_names() |>  pull(irs_soi_code) 

# Convert to nominal $ percentile income cutoffs
processed_soi <- soi |> 
  left_join(cpi) |> 
  mutate(
    nom_p99 = real_p99 * (cpi_22/cpi_u),
    nom_p98 = real_p98 * (cpi_22/cpi_u),
    nom_p97 = real_p97 * (cpi_22/cpi_u),
    nom_p96 = real_p96 * (cpi_22/cpi_u),
    nom_p95 = real_p95 * (cpi_22/cpi_u),
    nom_p90 = real_p90 * (cpi_22/cpi_u),
  ) |> 
  select(-starts_with("real"), -cpi_u)

write_csv(processed_soi, "data/soi/processed_soi.csv")

# Duplicate each row for every state
expanded_soi <- processed_soi |> 
  # Replicate rows for each state
  slice(rep(1:n(), each = length(irs_soi_code)))  |> 
  # Add the state column
  mutate(
    state = rep(irs_soi_code, times = nrow(processed_soi)),
    mstat = 2
    )

long_soi <- expanded_soi |> 
  pivot_longer(
    cols = starts_with("nom"),
    names_to = "percentile",
    values_to = "pwages"
  ) |> 
  mutate(percentile = sub(".*_", "", percentile)) # only keep "pXX" where XX is the percentile

# Write a function to save the data for each percentile
save_taxsim_data <- function(percentile_value) {
  long_soi |> 
    filter(percentile == percentile_value) |> 
    select(-percentile) |> 
    write_csv(paste0("data/data_for_taxsim/data_for_taxsim_", percentile_value, ".csv"))
}

save_taxsim_data("p99")
save_taxsim_data("p98")
save_taxsim_data("p97")
save_taxsim_data("p96")
save_taxsim_data("p95")
save_taxsim_data("p90")

