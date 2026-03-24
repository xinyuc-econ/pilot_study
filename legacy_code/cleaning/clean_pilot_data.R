#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 03-15-25                                                     
#                                                                  
# Clean pilot data
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


system.time({
pilot_data <- read_csv("data/processed_airmen_data/pooled_raw_pilot_data.csv", na = c("", "NA")) |> 
  relocate(c(type, level, expire_date, starts_with("rating")), .after = med_exp_date) |> 
  filter(year != 2025)


names(pilot_data)

# check that each unique id only appears once in each year
# Ans: yes
pilot_data |> 
  count(year, unique_id) |> 
  filter(n > 1)

# there's 1 observation with a missing value for country
pilot_data |> 
  count(is.na(country))

# add variables to investigate international migration
processed_pilot_data <- pilot_data |> 
  filter(!is.na(country)) |> # drop that 1 observation with a missing country value
  group_by(unique_id) |> 
  mutate(
    # compute the number of years a pilot has appeared in the dataset
    num_years = n_distinct(year),
    # compute the number of years a pilot lived in the US
    num_USA = sum(country == "USA"),
    # Identify those who have never lived in the US 
    never_in_US = (num_USA == 0),
    # define those who at least lived in the US for a year, but have also lived in another country
    # as pilots who have migrated in/out of the US
    migrate_in_out_US = (num_USA > 0) & (num_USA < num_years) 
  ) |> 
  ungroup() 

# remove the original dataset to save space
rm(pilot_data)

# sanity check: 2009-2011 should not have any pilots who never lived in the US
# Ans: yes
# proportion of pilots in the dataset by year who never lived in the US is 3.78%-7.22%
processed_pilot_data |> 
  group_by(year) |> 
  summarise(
    n_never_in_US = sum(never_in_US == TRUE),
    prop_never_in_US = n_never_in_US / n()
  )

# proportion of unique pilots who never lived in the US is 5.54%
processed_pilot_data |> 
  distinct(unique_id, never_in_US) |>
  summarize(
    prop_unique_never_in_US = sum(never_in_US) / n()
  )


# subset to the pilots who migrated in/out of US 
pilots_migrate_in_out_US <- processed_pilot_data |> 
  filter(migrate_in_out_US == 1) 

# inspect when and which country did these pilots migrate to
pilots_migrate_in_out_US |> 
  select(year, unique_id, first_name, last_name, state, country) |> 
  arrange(unique_id) |> view()

# subset to the pilots who have ever lived in the US
ever_in_US_pilots <- processed_pilot_data |> 
  filter(never_in_US == FALSE)

# remove prev dataset to save space
rm(processed_pilot_data)

# check the proportion of unique pilots who have migrated in/out of the US, among the ones who over lived in the US
# Ans: 1.4%
ever_in_US_pilots |> 
  distinct(unique_id, migrate_in_out_US) |> 
  summarize(
    prop_unique_migrate_in_out_US = sum(migrate_in_out_US) / n()
  )

# same thing, but by year
# benchmark is 1-2% of pilots in each year
ever_in_US_pilots |> 
  distinct(year, unique_id, migrate_in_out_US) |> 
  group_by(year) |> 
  summarize(
    n_unique = n(),
    prop_unique_migrate_in_out_US = sum(migrate_in_out_US) / n()
  )

# restrict sample to any level of pilots who have always lived in the US
always_in_US_pilots <- ever_in_US_pilots |> 
  filter(migrate_in_out_US == FALSE) 

# remove prev dataset to save space
rm(ever_in_US_pilots)

# Subset to pilots who have always lived in the 50 states + DC
always_in_main_US_pilots <- always_in_US_pilots |> 
  group_by(unique_id) |> 
  mutate(
    num_not_in_main_US = sum(state %in% c("PR", "VI", "AA", "AE", "GU", "AP", "AS", "MP", "FM", "MH", "PW")),
    always_in_main_US = (num_not_in_main_US == 0),
    both_main_other_US = (num_not_in_main_US) > 0 & (num_years > num_not_in_main_US)
  ) |> 
  filter(always_in_main_US == TRUE) |>
  select(-num_not_in_main_US, -always_in_main_US, -both_main_other_US)

# check how many have always lived in the 50 states + DC, how many have moved to/from other US territories, 
# and how many have always lived in other US territories
check <- always_in_US_pilots |> 
  group_by(unique_id) |> 
  mutate(
    num_not_in_main_US = sum(state %in% c("PR", "VI", "AA", "AE", "GU", "AP", "AS", "MP", "FM", "MH", "PW")),
    always_in_main_US = (num_not_in_main_US == 0),
    both_main_other_US = (num_not_in_main_US) > 0 & (num_years > num_not_in_main_US)
  ) 

check |> 
  ungroup() |> 
  count(always_in_main_US)

check |> ungroup() |> count(both_main_other_US)

check |> ungroup() |> count((num_not_in_main_US == num_years))


# import crosswalk from state 2-letter code to state full name and fips
state_fip_xwalk <- read_excel("data/xwalks/StateFIPSicsprAB.xls") |> 
  clean_names() |> 
  select(fips, name, ab) |> 
  rename(
    statefull = name,
    state = ab
  )

# restrict the sample to pilots who have always lived in the 50 states + DC in the US
# collapse the pilot certification levels to 3 levels
main_US_pilots_any <- state_fip_xwalk |> 
 left_join(always_in_main_US_pilots) |> 
  select(-c(num_USA, never_in_US, migrate_in_out_US)) |>                                                                                                                          
  relocate(year) |>
  mutate(
    level_collapsed = case_when(     
      level == "A" ~ "ATR",        # commercial airline transport pilot
      level == "C" ~ "C",          # pilots who can work for pay but can't work for major airlines
      TRUE ~ "other"            # all other pilots: student, recreational, etc
    )
  )

# sanity check plot: number and composition of pilot certification holders in each year
main_US_pilots_any |>
  ggplot(aes(x = as.factor(year), fill = level_collapsed)) +
  geom_bar() +
  scale_y_continuous(
    name = NULL,
    labels = label_number(scale = 1/1000, suffix = "K"),
    breaks = seq(0, 550000, 50000),
    limits = c(0, 550000)
  ) +
  scale_x_discrete(name = NULL) + 
  scale_fill_manual(
    values = viridis(3),
    labels = c("Airlzaine Transport Pilot", "Commercial Pilot", "Other"),
    name = "Pilot Certification Level"
    ) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    plot.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  labs(title = "Number and Composition of Pilot Certification Holders by Year")

ggsave("output_graphs/num_composition_pilots_by_year.png", width = 10, height = 7)
  
# Summary stats: number of pilots over the years in each state
sum_stat_pilots_st_yr <- main_US_pilots_any |> 
  count(year, state)

# check trend in num of pilots for a given state over the years
sum_stat_pilots_st_yr |> 
  filter(state == "NY") |> 
  ggplot(aes(x = year, y = n)) +
  geom_point() +
  geom_line()


# Summary data: number and composition of pilots in each year
main_US_pilots_any |>
  group_by(year, level_collapsed) |>
  summarize(
    n_level = n(),
    .groups = "drop"  # Drops the grouping after summarizing
  ) |>
  group_by(year) |>
  mutate(
    n_total = sum(n_level),  # Calculate the total number of pilots for each year
    prop = n_level / n_total  # Calculate the proportion of each level of pilots for each year
  ) |>
  ungroup()


# now subset to airline transport pilots
main_US_pilots_atr <- main_US_pilots_any |> 
  filter(level_collapsed == "ATR") |> 
  select(-level_collapsed, -level, -expire_date) 

# sanity check plot: number of airline transport pilots over the years
main_US_pilots_atr |> 
  ggplot(aes(x = as.factor(year))) + 
  geom_bar(fill = "#FFCB05") +
  scale_y_continuous(
    name = NULL,
    labels = label_number(scale = 1/1000, suffix = "K"),
    breaks = seq(0, 120000, 10000),
    limits = c(0, 120000)
  ) +
  scale_x_discrete(name = NULL) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    plot.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  ) +
  labs(title = "Number of Airline Transport Pilots by Year")

ggsave("output_graphs/num_atr_pilots_by_year.png", width = 9, height = 7)

# Summary plots: number of airline transport pilots over the years in each state
main_US_pilots_atr |> 
  count(year, state) |> 
  ggplot(aes(x = year, y = n)) +
  geom_point() +
  geom_line() + 
  facet_wrap(~state)

write_csv(main_US_pilots_atr, "clean_data/main_US_pilots_atr.csv")

# Summary data: number of airline transport pilots in each year
atr_sum_stat_st_yr <- main_US_pilots_atr |> 
  count(year, state)

# Import data on total working population
tot_working_pop_list_files <- list.files(
  "data/tot_working_pop_weights", 
  pattern = "tot_working_pop.csv$", 
  full.names = TRUE
)

# Combine all years of tot_working_pop data
tot_working_pop <- read_csv(tot_working_pop_list_files, id = "file") |> 
  mutate(
    year = as.numeric(str_extract(file, "\\d{4}"))
  ) |> 
  select(-file) |> 
  rename(state = statefips)

# sum stats on prop of airline transport pilots (relative to total working population) in each state
sum_stat_prop_pilots <- main_US_pilots_atr |> 
  count(year, state) |> 
  rename(n_atr_pilots = n) |> 
  left_join(tot_working_pop) |> 
  mutate(
    prop_atr_pilots = n_atr_pilots / tot_work_pop * 100
  )

# save as a csv in the clean dataset folder
write_csv(sum_stat_prop_pilots, "clean_data/sum_stat_prop_atr_pilots.csv")

})
