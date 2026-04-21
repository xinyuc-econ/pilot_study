library(tidyverse)
library(ggplot2)
library(ggthemes)
library(fixest)
library(ggfixest)


d <- read_csv("data/derived/aviationdb/faa_official_atr_pilots_by_state_year.csv")

twp <- read_csv("data/derived/aviationdb/sum_stat_prop_atr_pilots.csv") |> 
  select(year, state, tot_work_pop)


# baseline clean: add zero_PIT indicator, exclude AK and HI
# merge in total working population
d_base <- d |> 
  mutate(
    zero_PIT = if_else(state %in% c("TX", "FL", "NV", "WA", "WY", "SD", "TN", "AK", "NH"), "Zero-PIT", "Not Zero-PIT"),
    zero_PIT = factor(zero_PIT, levels = c("Zero-PIT", "Not Zero-PIT"))
  ) |> 
  filter(state!="AK" & state !="HI") |> 
  left_join(twp) |> 
  mutate(
    share = faa_n_atr_pilots / tot_work_pop * 100
  )


# Graphs: Normalized pilot population in zero- vs non-zero-PIT state plots --------------

## Exclude AK and HI -------------------------------------------------------

# aggregate pilot population by zero/non-zero PIT states
d_plot_base <- d_base |> 
  group_by(year, zero_PIT) |> 
  summarise(
    n = sum(faa_n_atr_pilots),
    ave_share = mean(share),
    n_tot = sum(tot_work_pop)
  ) |> 
  ungroup()

base_year_n <- d_plot_base |>
  filter(year == 2001) |>
  rename(
    base_n = n,
    base_ave_share = ave_share,
    base_tot_work_pop = n_tot
  ) |>
  select(-year)


d_plot_base <- d_plot_base |> 
  left_join(base_year_n) |> 
  mutate(
    norm_n = n / base_n,
    norm_share = ave_share / base_ave_share,
    norm_n_tot = n_tot / base_tot_work_pop
  )


p_base <- d_plot_base |>
  ggplot(aes(x = year, y = norm_n_tot, color = zero_PIT, group = zero_PIT, shape = zero_PIT, linetype = zero_PIT)) +
  geom_point(size = 3) + 
  geom_line(linewidth = 1.2) + 
  geom_vline(
    xintercept = 2017, 
    linetype = "dashed",
    color = "red",
    linewidth = 1.2
    ) +
  annotate(
    "text",
    x = 2012,
    y = 1.4,   # adjust this
    label = "TCJA SALT\nDeduction Cap",
    hjust = 0,
    size = 4,
    color = "red"
  ) +
  scale_color_manual(values = c("black", "grey50")) +
  scale_x_continuous(name = NULL,
                     limits = c(2000, 2025),
                     breaks = seq(2000, 2025, by = 5)
                     ) +
  scale_y_continuous(
    name = "Normalized Pilot Population (basis 1 = 2001)",
    #limits = c(0.97, 1.5),
    #breaks = seq(1, 1.5, 0.1)
                     ) +
  labs(shape = NULL, linetype = NULL, color = NULL) +
  guides(
    shape = guide_legend(nrow = 2),
    #color = guide_legend(nrow = 2),
    linetype = guide_legend(nrow = 2)
  ) +
  theme_bw()  +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    legend.title = NULL,
    legend.text = element_text(size = 18),
    legend.key.width = unit(2.5, "cm"),
    legend.position = "bottom"
  ) 

ggsave("output/aviationdb/figures/norm_pilot_pop_by_zero_PIT.png", p_base, width = 8, height = 6)



## Also exclude FL-------------------------------------------------------------------------

# aggregate pilot population by zero/non-zero PIT states
d_plot_base_noFL <- d_base |> 
  filter(state != "FL") |> 
  group_by(year, zero_PIT) |> 
  summarise(
    n = sum(faa_n_atr_pilots)
  ) |> 
  ungroup()

base_year_n_noFL <- d_plot_base_noFL |> 
  filter(year == 2001) |> 
  rename(
    base_n = n
  ) |> 
  select(-year)

d_plot_base_noFL <- d_plot_base_noFL |> 
  left_join(base_year_n_noFL) |> 
  mutate(
    norm_n = n / base_n
  )

p_base_noFL <- d_plot_base_noFL |>
  ggplot(aes(x = year, y = norm_n, color = zero_PIT, group = zero_PIT, shape = zero_PIT, linetype = zero_PIT)) +
  geom_point(size = 3) + 
  geom_line(linewidth = 1.2) + 
  geom_vline(
    xintercept = 2017, 
    linetype = "dashed",
    color = "red",
    linewidth = 1.2
  ) +
  annotate(
    "text",
    x = 2012,
    y = 1.4,   # adjust this
    label = "TCJA SALT\nDeduction Cap",
    hjust = 0,
    size = 4,
    color = "red"
  ) +
  scale_color_manual(values = c("black", "grey50")) +
  scale_x_continuous(name = NULL,
                     limits = c(2000, 2025),
                     breaks = seq(2000, 2025, by = 5)
  ) +
  scale_y_continuous(
    name = "Normalized Pilot Population (basis 1 = 2001)",
    limits = c(0.97, 1.5),
    breaks = seq(1, 1.5, 0.1)
  ) +
  labs(shape = NULL, linetype = NULL, color = NULL) +
  guides(
    shape = guide_legend(nrow = 2),
    #color = guide_legend(nrow = 2),
    linetype = guide_legend(nrow = 2)
  ) +
  theme_bw()  +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 18),
    axis.ticks = element_line(color = "black", linewidth = 2),
    legend.title = NULL,
    legend.text = element_text(size = 18),
    legend.key.width = unit(2.5, "cm"),
    legend.position = "bottom"
  ) 

ggsave("output/aviationdb/figures/norm_pilot_pop_by_zero_PIT_noFL.png", p_base_noFL, width = 8, height = 6)



# Event Study ---------------------------------------------------------

d_for_reg <- d_base |> 
  select(year, state, zero_PIT, share, faa_n_atr_pilots) |> 
  mutate(
    treat = if_else(zero_PIT == "Zero-PIT", 1, 0),
    post = if_else(year>=2018, 1, 0),
    lshare = log(share),
    ln = log(faa_n_atr_pilots)
  ) |> 
  select(-zero_PIT)

# DiD coeff
r1 <- feols(ln ~ treat:post | state + year, data = d_for_reg, cluster = ~state)

summary(r1)

# Event-study regression
r_es <- feols(
  ln ~ i(year, treat, ref = 2017) | state + year,
  data = d_for_reg,
  cluster = ~state
)


p_es <- ggiplot(
  r_es,
  ref.line = 2017,
  ref.line.par = list(col = "red", lty = 2, lwd = 1),
  main = "",
  xlab = NULL,
  ylab = NULL,
  geom_style = "ribbon",
  theme = theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.text.x = element_text(hjust = 1, size = 13),
      axis.text.y = element_text(size = 13),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 8)
  ) +
  annotate(
    "label",
    x = Inf, y = -0.12,
    label = "DID: 0.079 (0.043)",
    hjust = 1.1, vjust = -0.6,
    size = 4.5
    )

ggsave("output/aviationdb/figures/pilot_pop_zero_PIT_event_study.png", p_es, width = 8, height = 6)






