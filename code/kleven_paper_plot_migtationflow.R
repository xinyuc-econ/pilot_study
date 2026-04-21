library(tidyverse)
library(ggplot2)
library(ggthemes)
library(fixest)
library(ggfixest)


d <- read_csv("data/derived/aviationdb/mw_regression_dataset_soi_p95_unbalanced.csv")

stock <- read_csv("data/derived/aviationdb/sum_stat_prop_atr_pilots.csv") |> 
  select(year, dest_state = state, n_atr_pilots)

# in migration to each state in each year
d_min <- d |> 
  select(base_year, year, origin_state, dest_state, num_od, num_oo) |> 
  group_by(base_year, year, dest_state) |> 
  summarise(
    in_migration = sum(num_od)
  ) |> 
  ungroup()

m <- stock |> 
  left_join(d_min, by = c("year", "dest_state")) |> 
  mutate(
    zero_PIT = if_else(dest_state %in% c("TX", "FL", "NV", "WA", "WY", "SD", "TN", "AK", "NH"), "Zero-PIT", "Not Zero-PIT"),
    zero_PIT = factor(zero_PIT, levels = c("Zero-PIT", "Not Zero-PIT")),
    share = in_migration / n_atr_pilots * 100
  ) |> 
  filter(year > 2001) |> 
  filter(dest_state != "AK" & dest_state != "HI")



d_plot_base <- m |>
  group_by(year, zero_PIT) |> 
  summarise(
    n = sum(in_migration),
    ave_share = mean(share)
  ) |> 
  ungroup()

base_year_n <- d_plot_base |>
  filter(year == 2002) |>
  rename(
    base_n = n,
    base_share = ave_share
  ) |>
  select(-year)

d_plot_base <- d_plot_base |> 
  left_join(base_year_n) |> 
  mutate(
    norm_n = n / base_n,
    norm_share = ave_share / base_share
  )

d_plot_base |> 
  ggplot(aes(x = year, y = norm_share, color = zero_PIT, group = zero_PIT, shape = zero_PIT, linetype = zero_PIT)) +
  geom_point(size = 3) + 
  geom_line(linewidth = 1.2) + 
  geom_vline(
    xintercept = 2017, 
    linetype = "dashed",
    color = "red",
    linewidth = 1.2
  ) 



# Regressions -------------------------------------------------------------

d_for_reg <- m |> 
  select(year, dest_state, zero_PIT, share, in_migration) |> 
  mutate(
    treat = if_else(zero_PIT == "Zero-PIT", 1, 0),
    post = if_else(year>=2018, 1, 0),
    lshare = log(share),
    ln = log(in_migration)
  ) |> 
  select(-zero_PIT)

# DiD coeff
r1 <- feols(share ~ treat:post | dest_state + year, data = d_for_reg, cluster = ~dest_state)

summary(r1)

# Event-study regression
r_es <- feols(
  lshare ~ i(year, treat, ref = 2010) | dest_state + year,
  data = d_for_reg,
  cluster = ~dest_state
)

summary(
  r_es
)

iplot(r_es)
