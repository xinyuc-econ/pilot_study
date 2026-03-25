#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 11-20-25                                                     
#                                                                  
# Construct a cleaned dataset of pilots wages from the BLS data
# Raw data downloaded from https://www.bls.gov/oes/tables.htm
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



# clean BLS data ----------------------------------------------------------

# Read in the raw data files for all years 
files <- list.files(
  path = "data/bls",
  pattern = "^national_M\\d{4}_dl\\.xlsx?$",
  recursive = TRUE,
  full.names = TRUE
)

# bind all rows together and clean
bls_wages <- map_dfr(files, ~ {
  df <- read_excel(.x) |> 
    clean_names() |> 
    # only keep variables I need: mean&median wages, wage distribution
    select(occ_code, occ_title, a_mean, a_pct10, a_pct25, a_median, a_pct75, a_pct90) |> 
    # only keep pilots
    # 53-2010 Aircraft Pilots and Flight Engineers - broad
    # 53-2011 Airline Pilots, Copilots, and Flight Engineers - detailed
    # 53-2012 Commercial Pilots
    
    filter(occ_code %in% c("53-2010","53-2011", "53-2012"))
  
  # Extract the year from the filename, e.g. "national_M2010_dl.xls"
  year <- str_extract(basename(.x), "\\d{4}") |> as.integer()
  
  df |> mutate(year = year) |> relocate(year)
})

# Notes: # are top codes, varies by year

summary(bls_wages)

bls_wages_num <- bls_wages |>
  mutate(
    across(
      starts_with("a"),
      ~ as.numeric(na_if(.x, "#"))
    )
  )

write_csv(bls_wages_num, "data/bls/processed_bls.csv")


# Transform into data for taxsim ------------------------------------------

xwalk <-  read_csv("data/xwalks/irs_soi_fips_crosswalk.csv")

irs_soi_code <- xwalk |> clean_names() |>  pull(irs_soi_code) 

# Duplicate each row for every state
expanded_bls <- bls_wages_num |> 
  # Replicate rows for each state
  slice(rep(1:n(), each = length(irs_soi_code))) |> 
  # Add the state column
  mutate(
    state = rep(irs_soi_code, times = nrow(bls_wages_num)),
    mstat = 1
  )

long_bls <- expanded_bls |> 
  pivot_longer(
    cols = starts_with("a"),
    names_to = "percentile",
    values_to = "pwages"
  ) |> 
  mutate(percentile = sub(".*_", "", percentile)) |>  # only keep "pXX" where XX is the percentile
  filter(occ_code %in% c("53-2011", "53-2012")) |> 
  select(-occ_title)

# Write a function to save the data for each percentile and pilot occupation type
save_taxsim_data <- function(percentile_value, occ_code_value) {
  long_bls |> 
    filter(percentile == percentile_value, occ_code == occ_code_value) |> 
    select(-percentile, -occ_code) |> 
    write_csv(paste0("data/data_for_taxsim/data_for_taxsim_", occ_code_value, percentile_value,".csv"))
}

# mean and median wages for airline pilots
save_taxsim_data("mean", "53-2011")
save_taxsim_data("median", "53-2011")

# mean and median wages for commercial pilots
save_taxsim_data("mean", "53-2012")
save_taxsim_data("median", "53-2012")








