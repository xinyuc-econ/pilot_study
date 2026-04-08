library(tidyverse)
library(ggplot2)

d <- read_csv("data/derived/aviationdb/faa_official_atr_pilots_by_state_year.csv")

d_states <- d |> 
  mutate(
    zero_PIT = if_else(state %in% c("TX", "FL", "NV", "WA", "WY", "SD", "TN", "AK", "NH"), "Zero-PIT", "Not Zero-PIT"),
    state_type = case_when(
      zero_PIT == "Not Zero-PIT" ~ "Not Zero-PIT",
      zero_PIT == "Zero-PIT" ~ state
    )
  ) |> 
  filter(state!="AK" & state !="HI" & state != "FL") |> 
  group_by(year, zero_PIT) |> 
  summarise(
    n = sum(faa_n_atr_pilots)
  ) |> 
  ungroup()

base <- d_states |> 
  filter(year == 2001) |> 
  rename(
    base_n = n
  ) |> 
  select(-year)

m <- d_states |> 
  left_join(base) |> 
  mutate(
    norm_n = n / base_n
  )


m |>
  ggplot(aes(x = year, y = norm_n, color = zero_PIT, group = zero_PIT)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 2017, linetype = "dashed") 
