###########################################################
# Xinyu Chen                                                   
# 11-19-25                                                     
#                                                                  
# Agrawal and Foremny (2019) stock model
# AFM (2019) stock model
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
library(binsreg)
library(patchwork)
source("code/pilots_style_fixest.R")


# Read in pilot data ------------------------------------------------------
pilots <- read_csv("clean_data/mover_analysis_all_pilots_bls_mean.csv")

pilots <- pilots |> 
  filter(year <= 2022) |> 
  group_by(unique_id) |> 
  mutate(num_years = n())

# Pilots data -------------------------------------------------------------

## Create state pairs by year data structure ------------------------------------------------------
states <- state.abb  # This gives 50 state abbreviations
states <- c(states, "DC")  # Add 51st (e.g., DC)
years <- c(2009, 2010, 2011, 2014, 2015, 2016, 2017, 2019, 2022)

ordered_pairs <- expand.grid(origin_state = states, dest_state = states)

year_state_pair <- expand_grid(year = years, ordered_pairs)


## number of pilots in state by year (not balanced) ------------------------
dest_pop <- pilots |> 
  group_by(year, dest_state) |> 
  count() |> 
  rename(dest_n = n)

origin_pop <- dest_pop |> 
  rename(origin_state = dest_state, origin_n = dest_n)

## number of pilots in state by year (balanced) ------------------------
b_dest_pop <- pilots |> 
  filter(num_years == 9) |> 
  group_by(year, dest_state) |> 
  count() |> 
  rename(b_dest_n = n)

b_origin_pop <- b_dest_pop |> 
  rename(origin_state = dest_state, b_origin_n = b_dest_n)

pop_d <- year_state_pair |> 
  left_join(origin_pop, by = c("year", "origin_state")) |> 
  left_join(dest_pop, by = c("year", "dest_state")) |> 
  left_join(b_origin_pop, by = c("year", "origin_state")) |> 
  left_join(b_dest_pop, by = c("year", "dest_state")) |> 
  filter(origin_state == "CA")


# Merge in tax data -------------------------------------------------------

# read in the tax data
tax <- read_csv("clean_data/all_years_pit_bls.csv") |> 
  filter(pilot_type == "airline") |> 
  # filter(percentile == "mean") |> 
  select(year, fips, percentile, atr)

# read in the state abbrev. and FIPS crosswalk
xwalk <- read_excel("data/xwalks/StateFIPSicsprAB.xls") |> 
  rename(fips = FIPS, state_name = NAME, origin_state = AB) |>
  select(fips, origin_state)

# merge in state abbreviation
tax <- tax |> 
  left_join(xwalk) |> 
  relocate(year, origin_state, fips, atr)


## construct origin tax data and destination tax data ----------------------
origin_tax <- tax |> 
  rename(
    origin_fips = fips,
    origin_atr = atr
  ) |> 
  filter(year %in% c(2009, 2010, 2011, 2014, 2015, 2016, 2017, 2019, 2022))

dest_tax <- tax |> 
  rename(
    dest_fips = fips,
    dest_atr = atr,
    dest_state = origin_state
  )|> 
  filter(year %in% c(2009, 2010, 2011, 2014, 2015, 2016, 2017, 2019, 2022))


pop_d |> 
  distinct(year)

## merge -------------------------------------------------------------------


m <- dest_tax |> 
  left_join(pop_d, by = c("year", "dest_state"))

pop_d |> 
  distinct(year, origin_state)

# Final dataset for regressions -------------------------------------------
reg_d <- m |> 
  mutate(
    lnet_atr_diff = log(1 - dest_atr),
    lb_n_diff = log(b_dest_n) ,
    ln_diff = log(dest_n)
  ) |> 
  # baseline excludes Alaska and Hawaii
  filter(origin_state!="AK" & origin_state!= "HI" & dest_state!="AK" & dest_state!= "HI")


# Regressions -------------------------------------------------------------

afm25_b_median <- feols(lb_n_diff ~ lnet_atr_diff | year + dest_state,
                 data = reg_d |> filter(percentile=="median"),
                 cluster = ~dest_state
)

summary(afm25_b_median)

afm25_b_mean <- feols(lb_n_diff ~ lnet_atr_diff | year + dest_state,
                        data = reg_d |> filter(percentile=="mean"),
                        cluster = ~dest_state
)

summary(afm25_b_mean)


etable(afm25_b_median, afm25_b_mean,
       tex = TRUE,
       replace = TRUE,
       fontsize = "small",
       style.tex = style_fixest,
       cluster = ~dest_state,
       file = "output_tables/stock2025_bls_b.tex")



## AFM 2025, balanced ----------------------------------------------------------------

afm25_b <- feols(lb_n_diff ~ lnet_atr_diff | year + dest_state,
                 data = reg_d |> filter(origin_state == "MI"),
                 cluster = ~dest_state
)

summary(afm25_b)



## AFM 2025, balanced, diff pivot state ------------------------------------
pivot_mi <- reg_d |> 
  filter(origin_state == "MI") |> 
  mutate(
    lbn = log(b_dest_n),
    lnet_atr = log(1 - dest_atr)
  )
  

pivot_ca <- reg_d |> 
  filter(origin_state == "CA") |> 
  mutate(
    lbn = log(b_dest_n),
    lnet_atr = log(1 - dest_atr)
  )

check_mi <- pivot_mi |> 
  select(year, origin_state, dest_state, b_origin_n, b_dest_n, origin_atr, dest_atr, lbn, lnet_atr)

check_ca <- pivot_ca |> 
  select(year, origin_state, dest_state, b_origin_n, b_dest_n, origin_atr, dest_atr, lbn, lnet_atr)

### No pivot state, but filter to origin state == "CA"---------------------------------------------------

afm25_b_ca <- feols(lbn ~ lnet_atr | year + dest_state,
                 data = pivot_ca,
                 cluster = ~dest_state
)

summary(afm25_b_ca)



### No pivot state, but filter to origin state == "MI"---------------------------------------------------
afm25_b_mi <- feols(lbn ~ lnet_atr | year + dest_state,
                    data = pivot_ca,
                    cluster = ~dest_state
)

summary(afm25_b_mi)




summary(pivot_ca)
summary(pivot_mi)


### cross-section -----------------------------------------------------------

cross_sec <- feols(lbn ~ lnet_atr,
                   data = pivot_mi |> filter(year == 2019)
)

summary(cross_sec)





## AFM 2025, not balanced ----------------------------------------------------------------


afm25_xb <- feols(ln_diff ~ lnet_atr_diff | year + dest_state,
                 data = reg_d |> filter(origin_state == "CA"),
                 cluster = ~dest_state
)

summary(afm25_xb)




