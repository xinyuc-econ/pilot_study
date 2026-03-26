#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 03-31-25                                                     
#                                                                  
# Identify and analyze pilots who are movers
# Summary stats on movers
# Total and average annual state-to-state migration flows of pilots
#                                                              
#-------------------------------------------------------------- 

library(tidyverse)
library(janitor)
library(haven)
library(readxl)
library(ggrepel)
library(stargazer)
library(xtable)

# read in the main cleaned ATR pilots data
pilots <- read_csv("clean_data/main_US_pilots_atr.csv")

# subset to relevant variables
subset_pilots <- pilots |> 
  select(year, fips, statefull, state, unique_id, first_name, last_name, street_1, city, zip_code, num_years)

# identify pilots who moved state-to-state
moved_pilots <- subset_pilots |> 
  arrange(year) |> 
  arrange(unique_id) |> 
  group_by(unique_id) |> 
  rename(
    dest_state = state, # state the pilot lives in year t is the destination state
  ) |> 
  mutate(
    origin_state = lag(dest_state), # state the pilot lived in year t-h is the origin state
    moved = ifelse(dest_state != origin_state, 1, 0) # if destination state is different from origin state, then the pilot moved
    ) |>
  relocate(c(origin_state, moved), .after = dest_state) |> 
  ungroup()

# summary stats on the proportion of pilots who moved state-to-state by time period 
prop_moved_by_year <- moved_pilots |> 
  group_by(year) |> 
  summarise(
    n_pilots = n(), # compute number of unique pilots observed in each year
    n_moved = sum(moved, na.rm = TRUE), # compute number of unique pilots who moved state-to-state
    prop_moved = n_moved / n_pilots * 100 # compute proportion of unique pilots who moved state-to-state
  ) |> 
    mutate(
      lag_year = lag(year),
      `Time period` = paste(lag_year, year, sep = "-"), # create a time period variable (bc I don't always have consecutive years)
      monthly_prop_moved = case_when(
        `Time period` == "2009-2010" ~ prop_moved / 6, # Nov. 2009 - May 2010
        `Time period` == "2010-2011" ~ prop_moved / 16, # May 2010 - Sep 2011
        `Time period` == "2011-2014" ~ prop_moved / 36, # Sep. 2011 - Sep. 2014
        `Time period` == "2014-2015" ~ prop_moved / 12, # Sep. 2014 - Sep. 2015
        `Time period` == "2015-2016" ~ prop_moved / 14, # Sep. 2015 - Nov. 2016
        `Time period` == "2016-2017" ~ prop_moved / 7, # Nov. 2016 - June. 2017
        `Time period` == "2017-2019" ~ prop_moved / 24, # June. 2017 - June. 2019
        `Time period` == "2019-2022" ~ prop_moved / 40, # June. 2019 - Oct 2022
        `Time period` == "2022-2024" ~ prop_moved / 23 # Oct 2022 - Sep 2014
      ),
      `Time period` = case_when(
        `Time period` == "2009-2010" ~ "11/2009 - 05/2010",
        `Time period` == "2010-2011" ~ "05/2010 - 09/2011",
        `Time period` == "2011-2014" ~ "09/2011 - 09/2014",
        `Time period` == "2014-2015" ~ "09/2014 - 09/2015",
        `Time period` == "2015-2016" ~ "09/2015 - 11/2016",
        `Time period` == "2016-2017" ~ "11/2016 - 06/2017",
        `Time period` == "2017-2019" ~ "06/2017 - 06/2019",
        `Time period` == "2019-2022" ~ "06/2019 - 10/2022",
        `Time period` == "2022-2024" ~ "10/2022 - 09/2024"
      )
    ) |> 
    filter(!is.na(lag_year)) |> 
    select(-year, -lag_year, -prop_moved) |> 
    relocate(`Time period`, .before = n_pilots) |>
    rename(
      `# of pilots` = n_pilots,
      `# of movers` = n_moved,
      `Ave. monthly % moved` = monthly_prop_moved
    ) |> 
    as.data.frame()

# save as a table
print(xtable(prop_moved_by_year, type = "latex", 
             caption = "Proportion of pilots who moved state-to-state by time period", 
             align = "ccccc",
             digits=c(0,0,0,0,2)), 
      include.rownames=FALSE, file = "output_tables/prop_moved_by_year.tex")

# How many times did each unique pilot move state-to-state?
times_moved_unique_pilots <- moved_pilots |> 
  group_by(unique_id) |>
  mutate(
    num_moves = sum(moved, na.rm = TRUE) # create a variable that counts the number of times each unique pilot moved state-to-state
  ) |> 
  relocate(num_moves, .after = moved) |> 
  ungroup() 


# Histogram on number of moves 
times_moved_unique_pilots |> 
  distinct(unique_id, num_moves) |> 
  ggplot(aes(num_moves)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal()

# Histogram on number of moves, conditional on moving at least once
times_moved_unique_pilots |> 
  distinct(unique_id, num_moves) |> 
  filter(num_moves > 0) |>
  ggplot(aes(num_moves)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal()

# proportion of unique pilots who moved at least once
# Ans: 22.5% (41,351 out of 183,626)
times_moved_unique_pilots |> 
  distinct(unique_id, num_moves) |> 
  mutate(
    ever_moved = ifelse(num_moves > 0, 1, 0)
  ) |> 
  summarise(
    n_pilots = n(), # number of unique pilots
    n_ever_moved = sum(ever_moved), # number of unique pilots who moved at least once
    n_moved_once = sum(num_moves == 1), # number of unique pilots who moved once
    prop_moved = mean(ever_moved) * 100 # proportion of unique pilots who moved at least once
  )

# average moves per pilot, unconditional
# Ans: 0.306
times_moved_unique_pilots|> 
  distinct(unique_id, num_moves)|> 
  summarise(
    mean_moves = mean(num_moves)
  )

# average moves per pilot, conditional on moving at least once
# Ans: 1.40
times_moved_unique_pilots |> 
  filter(num_moves > 0) |> 
  summarise(
    mean_moves = mean(num_moves)
  )
  
# Table analogous to Table 1 in MW2017, all states
tot_outflow <- moved_pilots |> 
  group_by(unique_id) |> 
  mutate(
    lag_year = lag(year),
    time_period_yrs = year - lag_year,
  ) |> 
  relocate(c(lag_year, time_period_yrs), .after = year) |> 
  filter(!is.na(lag_year)) |> 
  ungroup() 


# row is origin state, column is destination state, each cell is the number of pilots who moved from origin to destination
count_table <- table(tot_outflow$origin_state, tot_outflow$dest_state)

# Convert the table to a matrix or data frame for compatibility with xtable
count_table_matrix <- as.matrix(count_table)

print(xtable(count_table_matrix, type = "latex", 
             caption = "Total number of moves of pilots observed, 2009-2024"), 
      file = "output_tables/total_outflow_observed_all_states.tex")  

# Out of 51*50*9 = 22,950 origin*dest*time period cells (excluding those who stayed in the same state), 
# 11705 have positive outflows
tot_outflow |> 
  filter(moved == 1) |>
  distinct(year, origin_state, dest_state)

tot_outflow |> 
  filter(moved == 0) |> 
  count(year, origin_state, dest_state) 

# read in summary data on the number of pilots in each state 
sum_stat_prop_atr_pilots <- read_csv("clean_data/sum_stat_prop_atr_pilots.csv") |> select(year, state, n_atr_pilots)

# pick the top 25 states with the highest number of pilots averaged across the years
top25_states <- sum_stat_prop_atr_pilots |> 
  group_by(state) |> 
  summarize(
    avg_n_atr_pilots = mean(n_atr_pilots)
  ) |> 
  arrange(desc(avg_n_atr_pilots)) |> 
  slice(1:10) |> 
  pull(state)

sum_stat_prop_atr_pilots |> distinct(year)

# check the average number of pilots in each state
sum_stat_prop_atr_pilots |> 
  group_by(state) |> 
  summarize(
    avg_n_atr_pilots = mean(n_atr_pilots)
  ) |> 
  arrange(desc(avg_n_atr_pilots)) |> 
  print(n = Inf)

# Table analogous to Table 1 in MW2017, top 20 states
tot_outflow_top25 <- moved_pilots |> 
  filter(origin_state %in% top25_states & dest_state %in% top25_states) |>
  group_by(unique_id) |> 
  mutate(
    lag_year = lag(year),
    time_period_yrs = year - lag_year,
  ) |> 
  relocate(c(lag_year, time_period_yrs), .after = year) |> 
  filter(!is.na(lag_year)) |> 
  ungroup() 


# Table for top25 states: row is origin state, column is destination state, each cell is the number of pilots who moved from origin to destination
count_table_top25 <- table(tot_outflow_top25$origin_state, tot_outflow_top25$dest_state)

# Convert the table to a matrix or data frame for compatibility with xtable
count_table_top25_matrix <- as.matrix(count_table_top25)

# save as a latex table
print(xtable(count_table_top25_matrix, type = "latex", 
             caption = "Total observed migration flows of pilots, top 25 states, 2009-2024"), 
      file = "output_tables/total_outflow_observed_top25.tex")  


# Table on imputed average annual flow of pilots, top 20 states
# USING ONLY DATA FROM CONSECUTIVE YEARS, 2009-11, 2014-17
annual_outflow_top25 <- tot_outflow_top25 |>
  filter(origin_state %in% top25_states & dest_state %in% top25_states) |>
  filter(time_period_yrs == 1) 

count_table_consec_yrs_top25 <- table(annual_outflow_top25$origin_state, annual_outflow_top25$dest_state)

# convert to matrix form and average across 5 years
annual_outflow_top25_matrix <- as.matrix(count_table_consec_yrs_top25) / (2017-2014+2011-2009)

# save as a latex table
print(xtable(annual_outflow_top25_matrix, type = "latex", 
             caption = "Average annual migration flow of pilots, top 10 states, 2009-2011 and 2014-2017"), 
      file = "output_tables/annual_outflow_observed_top25.tex")  



