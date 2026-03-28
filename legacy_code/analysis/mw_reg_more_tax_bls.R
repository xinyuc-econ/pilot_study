#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 11-23-25                                                     
#                                                                  
# Moretti and Wilson (2017) regression specification, with more tax measures
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
library(purrr)
library(binsreg)
library(patchwork)
source("code/pilots_style_fixest.R")

# read in the main cleaned ATR pilots data
pilots <- read_csv("clean_data/mover_analysis_all_pilots_bls_mean.csv") |> filter(year <= 2022)

# add a base year, which is a lagged year
new_pilots <- pilots |> 
  group_by(unique_id) |> 
  mutate(
    base_year = lag(year)
  ) |> 
  relocate(
    base_year, .before = year
  ) |> 
  ungroup() |> 
  select(base_year, year, unique_id, first_name, last_name, origin_state, origin_fips, dest_state, moved) |> 
  group_by(unique_id) |> 
  mutate(
    num_years = n()
  ) |> 
  ungroup()

# Construct tax datasets --------------------------------------------------

# atr for different income percentiles in each origin and dest state by year
tax <-  read_csv("clean_data/all_years_pit_bls.csv") |> 
  filter(pilot_type == "airline") |> 
  select(year, fips, percentile, atr)

# read in the state abbrev. and FIPS crosswalk
xwalk <- read_excel("data/xwalks/StateFIPSicsprAB.xls") |> 
  rename(fips = FIPS, state = AB) |>
  select(fips, state)

# add state AB to tax dataset
tax <- tax |> 
  left_join(xwalk)

# Construct tax data for origin state
origin_tax <- tax |> 
  rename(
    origin_fips = fips,
    origin_atr = atr,
    origin_state = state
  ) |> 
  pivot_wider(
    names_from = percentile,
    values_from = origin_atr,
    names_prefix = "origin_atr_"
  )

# Construct tax data for destination state
dest_tax <- tax |> 
  rename(
    dest_fips = fips,
    dest_atr = atr,
    dest_state = state
  )|> 
  pivot_wider(
    names_from = percentile,
    values_from = dest_atr,
    names_prefix = "dest_atr_"
  )


# Construct migration flow dataset ----------------------------------------


## Maybe start with a balanced panel of pilots (showed up in all 9 --------

b_num_origin_dest <- new_pilots |> 
  filter(num_years == 9) |> 
  group_by(base_year, year, origin_state, dest_state) |>
  summarize(
    num_od = n()
  ) |> 
  ungroup() |> 
  mutate(time_period = paste(base_year, year, sep = "-")) |> 
  filter(
    time_period %in%  c(
      "2009-2010", "2010-2011", "2011-2014","2014-2015", 
      "2015-2016", "2016-2017", "2017-2019", "2019-2022"
    )
  ) |> 
  select(-time_period) |> 
  mutate(moved = ifelse(origin_state != dest_state, 1, 0))

b_num_moved <- b_num_origin_dest |> 
  filter(moved == 1) |> 
  select(-moved)

b_num_stayed <- b_num_origin_dest |> 
  filter(moved == 0) |> 
  select(-moved, -dest_state) |> 
  rename(num_oo = num_od)

b_mig_flow <- b_num_moved |> 
  left_join(b_num_stayed, by = c("base_year", "year", "origin_state"))

b_mig_flow_new <- b_mig_flow |>
  mutate(
    state_min = pmin(origin_state, dest_state),
    state_max = pmax(origin_state, dest_state),
    state_pair_nodir = paste(state_min, state_max, sep = "_")
  )

b_mig_flow_new |> 
  group_by(base_year, year, state_pair_nodir) |> 
  mutate(
    gross_flow = sum(num_od)/num_oo
  ) |> summary()

flows_out <- b_mig_flow_new |>
  filter(origin_state != dest_state) |>    
  group_by(year, origin_state) |>
  summarise(
    outflow = sum(num_od),
    .groups = "drop"
  ) |>
  rename(state = origin_state)

flows_in <- b_mig_flow_new |>
  filter(origin_state != dest_state) |>    
  group_by(year, dest_state) |>
  summarise(
    inflow = sum(num_od),
    .groups = "drop"
  ) |>
  rename(state = dest_state)

stock <- new_pilots |> 
  filter(num_years == 9) |> 
  group_by(year, origin_state) |>
  summarize(
    stock = n()
  ) |> 
  rename(state = origin_state)

# for computing stock elasticity: 0.10399
flows_in |> 
  left_join(flows_out) |> 
  left_join(stock) |> 
  mutate(
    ratio = (inflow + outflow) / stock
  ) |> 
  summary()

# Merge the tax dataset with the migration flow dataset -------------------

b_m <- b_mig_flow |> 
  left_join(origin_tax, by = c("year", "origin_state")) |> 
  left_join(dest_tax, by = c("year", "dest_state"))

d_for_reg <- b_m |> 
  pivot_longer(
    cols = c(starts_with("origin_atr"), starts_with("dest_atr")),
    names_to = c(".value", "percentile"),
    names_pattern = "(origin_atr|dest_atr)_(p\\d+|median|mean)"
  ) |> 
  mutate(
    lodds_ratio = log(num_od / num_oo),
    lnet_tax_origin = log((1 - origin_atr)),
    lnet_tax_dest = log((1 - dest_atr)),
    lnet_tax_diff = lnet_tax_dest - lnet_tax_origin
  ) |> 
  filter(origin_state!="AK" & origin_state!= "HI" & dest_state!="AK" & dest_state!= "HI")

summary(d_for_reg)

d_for_reg |> 
  filter(base_year == 2019, year == 2022, origin_state == "MN", dest_state == "AZ")

# Regressions -------------------------------------------------------------


# list of percentiles you want
percentiles <- c("median", "mean")



# function to run the regression for a given percentile
run_reg_spec2 <- function(p) {
  feols(
    lodds_ratio ~ lnet_tax_diff | year + origin_state + dest_state + origin_state^dest_state,
    data = d_for_reg |> filter(percentile == p),
    cluster = ~dest_state^origin_state + origin_state^year + dest_state^year
  )
}




## Reg spec2 results to table ----------------------------------------------
# run for all percentiles
regs_spec2 <- map(percentiles, run_reg_spec2)

# name the list by percentile for convenience
names(regs_spec2) <- percentiles

# check results
lapply(regs_spec2, summary)

etable(regs_spec2, 
       tex = TRUE,
       replace = TRUE,
       fontsize = "small",
       style.tex = style_fixest,
       cluster = ~dest_state^origin_state + origin_state^year + dest_state^year,
       title = "Moretti and Wilson (2017) Regression Specification Results, 3-way clustering",
       file = "output_tables/mw_reg_3way_cluster_spec2_bls_xb.tex")




d_for_reg <- d_for_reg |> filter(percentile == "mean")

# 1a. For the log odds‐ratio:
fit_y <- feols(lodds_ratio ~ 1 | origin_state^dest_state + year,
               data = d_for_reg)

d_for_reg$y_dm <- resid(fit_y)

# 1b. For the log net‐of‐tax rate:
fit_log_origin_net_atr <- feols(lnet_tax_origin ~ 1 | origin_state^dest_state + year,
                                data = d_for_reg)

d_for_reg$x_origin_dm <- resid(fit_log_origin_net_atr)

fit_log_dest_net_atr <- feols(lnet_tax_dest ~ 1 | origin_state^dest_state + year,
                              data = d_for_reg)

d_for_reg$x_dest_dm <- resid(fit_log_dest_net_atr)

fit_log_atr_diff <- feols(lnet_tax_diff ~ 1 | origin_state^dest_state + year,
                          data = d_for_reg)

d_for_reg$x_diff_dm <- resid(fit_log_atr_diff)

d_for_binscatter <- d_for_reg |> as.data.frame()


d_for_reg |> summary()

names(d_for_binscatter)

p1 <- binsreg(y_dm, x_origin_dm,
              data = d_for_binscatter,
              polyreg = 1,
              nbins = 50
)

p2 <- binsreg(y_dm, x_dest_dm, data = d_for_binscatter,
              polyreg = 1,
              nbins = 50
)

p3 <- binsreg(y_dm, x_diff_dm, data = d_for_binscatter,
              polyreg = 1,
              nbins = 50
)

left_p <- p1$bins_plot +
  labs(
    title = "Origin State",
    x = "log net-of-tax",
    y = "migration log odds ratio") +
  theme(
    axis.title  = element_text(size = 25),
    axis.text = element_text(size = 20),
    strip.text.x = element_text(size = 25),
    title = element_text(size = 25)
  )

ggsave(
  filename = "output_graphs/mw_reg_binscatter_origin_bls_mean.png",
  width = 7,
  height = 6
)

right_p <- p2$bins_plot +
  labs(
    title = "Destination State",
    x = "log net-of-tax",
    y = "migration log odds ratio") +
  theme(
    axis.title  = element_text(size = 25),
    axis.text = element_text(size = 20),
    strip.text.x = element_text(size = 25),
    title = element_text(size = 25)
  )

ggsave(
  filename = "output_graphs/mw_reg_binscatter_dest_bls_mean.png",
  width = 7,
  height = 6
)


diff_p <- p3$bins_plot +
  labs(
    title = "Destination-Origin State Diff.",
    x = "log net-of-tax, 95th percentile income",
    y = "migration log odds ratio") +
  theme(
    axis.title  = element_text(size = 20),
    axis.text = element_text(size = 18),
    strip.text.x = element_text(size = 18),
    title = element_text(size = 20)
  )

ggsave(
  filename = "output_graphs/mw_reg_binscatter_diff.png",
  width = 7,
  height = 6
)

left_p + right_p

ggsave(
  filename = "output_graphs/mw_reg_binscatter_both.png",
  width = 20,
  height = 9
)


# # summary statistics -- prep data
# sum_stat <- d_for_reg |> 
#   select(p_odt, p_oot, log_odds_ratio, net_atr_diff) |> 
#   as.data.frame()
# 
# # summary statistics -- save as a table
# stargazer(sum_stat, 
#           title = "Summary statistics",
#           covariate.labels=c("$P_{odt}$", "$P_{oot}$", "$\\log (P_{odt}/P_{oot})$", "Net ATR diff."),
#           digits = 2,
#           summary.stat = c("n", "mean", "sd", "min", "max"),
#           font.size = "small",     
#           out = "output_tables/sum_stat_mw_reg.tex")


