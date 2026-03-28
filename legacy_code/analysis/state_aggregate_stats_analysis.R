#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 07-28-25                                                     
#                                                                  
# Official FAA aggregate stats analysis - state level
# Data coverage map + stock analysis
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
library(patchwork)
library(fixest)
source("code/pilots_style_fixest.R")


# Read in the FAA aggregate stats data
allstates_atr_all_years <- read_csv("clean_data/faa_sum_stat_prop_atr_pilots.csv") 

# Read in the Airmen Certification data
my_atr_all_years <- read_csv("clean_data/sum_stat_prop_atr_pilots.csv") |> 
  rename(my_n_atr_pilots = n_atr_pilots)


# Combine the FAA and my data
m_dta <- allstates_atr_all_years |> 
  left_join(my_atr_all_years, by = c("state", "year")) |> 
  mutate(
    diff = faa_n_atr_pilots - my_n_atr_pilots
  ) |> 
  mutate(
    coverage = my_n_atr_pilots / faa_n_atr_pilots *100
  ) |> 
  filter(!is.na(coverage)) 

# Get usmap coordinates for each state
state_map <- usmap::us_map("states")

# Step 1: Compute centroids
state_centers <- state_map %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  as_tibble() %>%
  bind_cols(state_map %>% st_drop_geometry() %>% select(abbr)) |> 
  select(-abbr...7) |> 
  rename(
    abbr = abbr...2
  )

# Step 2: Merge with your data
m_dta_labeled <- m_dta %>%
  left_join(state_centers, by = c("state" = "abbr"))


# Step 1: Choose small or overlapping states manually
repel_states <- c("RI", "CT", "NJ", "DE", "MD", "MA", "DC", "VT", "NH")

# Step 2: Split the data into two groups
labels_repel <- m_dta_labeled %>% filter(state %in% repel_states)
labels_regular <- m_dta_labeled %>% filter(!state %in% repel_states)

# Step 3: Plot
ggplot() +
  geom_sf(data = state_map, fill = NA, color = "black") +
  geom_sf(data = left_join(state_map, m_dta, by = c("abbr" = "state")),
          aes(fill = coverage)) +
  scale_fill_viridis_c(
    name = "Data Coverage (%)",
    alpha = 0.9, direction = -1,
    ) +
  # Regular labels (centered)
  geom_text(
    data = labels_regular,
    aes(x = X, y = Y, label = paste0(round(coverage, 0), "%")),
    color = "black", size = 4
  ) +
  facet_wrap(~year) +
  # Repelled labels to the right
  geom_text_repel(
    data = labels_repel,
    aes(x = X, y = Y, label = paste0(round(coverage, 0), "%")),
    nudge_x = 6e5,         # same as offset above
    #hjust = 0,             # align text to left
    direction = "x",       # only repel along the x-axis
    segment.color = "gray50",
    point.padding = 0.2,
    max.overlaps = Inf,
    force = 5,                # controls repulsion strength
    force_pull = 0.5,         
    size = 4,
    min.segment.length = 0
  ) +
  theme_void() + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    strip.text = element_text(size = 15),
        ) 

ggsave("output_graphs/state_coverage_atr_pilots.png", width = 20, height = 10)

# Mean and Median data coverage is 73%. Min 51.93%. Max 87.69%.
m_dta |> 
  summary()



# Stock analysis using official aggregate stats ---------------------------

states <- state.abb  # This gives 50 state abbreviations
states <- c(states, "DC")  # Add 51st (e.g., DC)
years <- 2016:2024

ordered_pairs <- expand.grid(origin_state = states, dest_state = states) |> 
  filter(origin_state != dest_state)

year_state_pair <- expand_grid(year = years, ordered_pairs)


dest_pop <- allstates_atr_all_years |> 
  rename(
    dest_state = state,
    dest_n = faa_n_atr_pilots
    ) |> 
  select(-statefull, -fips)

origin_pop <- dest_pop |> 
  rename(origin_state = dest_state, origin_n = dest_n)

pop_d <- year_state_pair |> 
  left_join(origin_pop, by = c("year", "origin_state")) |> 
  left_join(dest_pop, by = c("year", "dest_state")) 


# read in the tax data
tax <- read_csv("clean_data/all_years_pit_bls.csv") |> 
  filter(pilot_type == "airline") |> 
  select(year, state_name, fips, percentile, astr, atr)

tax |> 
  distinct(percentile)

# read in the state abbrev. and FIPS crosswalk
xwalk <- read_excel("data/xwalks/StateFIPSicsprAB.xls") |> 
  rename(fips = FIPS, state_name = NAME, origin_state = AB) |>
  select(fips, state_name, origin_state)

tax <- tax |> 
  left_join(xwalk)

# Construct (six) different tax measures
# p90, p95, p99; atr, astr
# pivot to wide
tax_origin <- tax |>
  rename(origin_atr = atr, origin_astr = astr, origin_statefull = state_name, origin_fips = fips) |>
  pivot_wider(
    names_from = percentile,
    values_from = c(origin_atr, origin_astr),
    names_sep = "_"
  )


# rename the columns for destination state
tax_dest <- tax_origin |> 
  rename_with(~ gsub("origin", "dest", .x))

m <- pop_d |> 
  left_join(tax_origin) |> 
  left_join(tax_dest) |> 
  relocate(year, starts_with("origin"), starts_with("dest"))

# Compute LHS: log stock ratio of destination state versus origin state
# Compute RHS: log net tax difference between destination and origin state, six measures
reg_d <- m |> 
  mutate(
    ln_n_ratio = log(dest_n / origin_n),
    ln_net_atr_diff_median = log(1 - dest_atr_median) - log(1 - origin_atr_median),
    ln_net_atr_diff_mean = log(1 - dest_atr_mean) - log(1 - origin_atr_mean)
  ) |> 
  filter(year < 2023) 


summary(reg_d)



# run the regressions
reg1 <- feols(ln_n_ratio ~ ln_net_atr_diff_mean | year + dest_state, 
              data = reg_d |> filter(origin_state == "CA"),
              cluster = ~dest_state
              )

summary(reg1)


reg2 <- feols(ln_n_ratio ~ ln_net_atr_diff_mean | year + origin_state + dest_state, 
              data = reg_d,
              cluster = ~dest_state^origin_state + origin_state^year + dest_state^year
              )

summary(reg2)

reg3 <- feols(ln_n_ratio ~ ln_net_atr_diff_p99 | year + origin_state + dest_state, 
              data = reg_d)

summary(reg3)

reg4 <- feols(ln_n_ratio ~ ln_net_astr_diff_p90 | year + origin_state + dest_state, 
              data = reg_d)

summary(reg4)

reg5 <- feols(ln_n_ratio ~ ln_net_astr_diff_p95 | year + origin_state + dest_state, 
              data = reg_d)

summary(reg5)


reg6 <- feols(ln_n_ratio ~ ln_net_astr_diff_p99 | year + origin_state^dest_state, 
              data = reg_d)

summary(reg6)

etable(reg1, reg2, reg3, reg4, reg5, reg6, 
       tex = TRUE,
       replace = TRUE,
       style.tex = style_fixest,
       cluster = ~dest_state^origin_state + origin_state^year + dest_state^year,
       title = "Stock Analysis Results, 3-way clustering, Aggregate Stats Data, 2016-2022",
       file = "output_tables/aggregate_stat_stock_reg_2019_3way_cluster.tex")













