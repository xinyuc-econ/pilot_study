#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 03-13-25                                                     
#                                                                  
# Combining all available years of Airmen Certification data into one big dataset 
# Sample restricted to
# Years: 09, 10, 11, 14, 15, 16, 17, 19, 22, 24, 25
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

# Read in the raw basic files for all years 
basic_files <- list.files(
  "data/raw_airmen_data", 
  pattern = "^(RELDOMCB_\\d{4}|PILOT_BASIC_\\d{4})\\.csv$", 
  full.names = TRUE)

# Write a function that processes the basic data files to ensure uniform formatting
process_basic_files <- function(file) {
  read_csv(file, na = c("", "NA")) |> 
    clean_names() |>
    # Select only the columns we need
    select(
      unique_id, first_name, last_name, street_1, street_2, city,
      state, zip_code, country, region, med_class, med_date, med_exp_date
      ) |>
    # Save each processed file as a new CSV with name airmen_basic_<year>.csv
    write_csv(paste0("data/processed_airmen_data/airmen_basic_", str_extract(file, "\\d{4}"), ".csv"))
}

# Loop through the list of raw basic data files and process them
lapply(basic_files, process_basic_files)

# Read in the processed basic data files
processed_basic_files <- list.files(
  "data/processed_airmen_data", 
  pattern = "^airmen_basic_\\d{4}\\.csv$", 
  full.names = TRUE)

processed_basic_files

# Stack the 10 years of processed basic data into one big dataset
stacked_basic <- read_csv(
  processed_basic_files, 
  id = "file", # Create an ID variable, which is the file name
  col_types = "ccccccccccicc" # Make sure the columns in each data set have the same type
  ) |> 
  mutate(
    year = parse_number(file) # Create a year variable based on the file name
  ) |> 
  arrange(year) |> 
  select(-file) # Drop the file column

## check the size of the data file
# pryr::object_size(stacked_basic)


# Read in the raw certification files for all years 
cert_files <- list.files(
  "data/raw_airmen_data", 
  pattern = "^(RELDOMCC_\\d{4}|PILOT_CERT_\\d{4})\\.csv$", 
  full.names = TRUE)

# Write a function that processes the certification data files to ensure uniform formatting
process_cert_files <- function(file) {
  read_csv(file, na = c("", "NA")) |> 
    clean_names() |>
    # Select only the columns we need
    select(
      unique_id, first_name, last_name, type, level, expire_date, starts_with("rating")
      ) |>
    # Save each processed file as a new CSV with name airmen_cert_<year>.csv
    write_csv(paste0("data/processed_airmen_data/airmen_cert_", str_extract(file, "\\d{4}"), ".csv"))
}

# loop through the raw certification data files and process them
lapply(cert_files, process_cert_files)


# Read in the processed certification data files
processed_cert_files <- list.files(
  "data/processed_airmen_data", 
  pattern = "^airmen_cert_\\d{4}\\.csv$", 
  full.names = TRUE)

processed_cert_files

# Stack the 10 years of processed certification data into one big dataset
stacked_cert <- read_csv(
  processed_cert_files, 
  id = "file",
  col_types = paste0(rep("c", 17), collapse = "") # Make sure the columns in each data set are all characters
  ) |> 
  mutate(
    year = parse_number(file) # Create a year variable based on the file name
  ) |> 
  relocate(year) |> 
  select(-file) # Drop the file column 

# Subset the stacked certification data to only include ones who hold a pilot certificate
# Another purpose is to make sure that in each year, each unique_id only appears once
# since individuals can have multiple certificates
# Later I will use this to identify the pilots in the basic file
pilots_stacked_cert <- stacked_cert |> 
  filter(type == "P") 

# seems like no one had two levels of "P" certificate type
# Should double-check this
pilots_stacked_cert |> 
  count(year, unique_id) |> 
  filter(n > 1)

# Merge the stacked basic and pilot certification data
# this only includes those w/ some type of pilot certification
m1 <- pilots_stacked_cert |> 
  left_join(stacked_basic)

# save the dataset 
# the dataset contains information in both basic and certification files 
# for those with some level of pilot certification
write_csv(m1, "data/processed_airmen_data/pooled_raw_pilot_data.csv")

