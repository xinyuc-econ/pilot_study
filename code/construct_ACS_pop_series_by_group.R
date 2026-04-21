##################################################################################
# Pilot project
#
# Construct population series by group using ACS data
# 
##################################################################################

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
library(janitor)
library(tidyverse)
library(Hmisc)

ddi <- read_ipums_ddi("data/raw/usa_00032.xml")
data <- read_ipums_micro(ddi)


# General cleaning --------------------------------------------------------

data_min <- data |> 
  clean_names() |> 
  # general restrictions
  filter(
    # non-institutionalized
    gq != 3,  
    # 50 states + DC
    statefip <= 56,
    # non-farm
    farm == 1,
    # employed
    empstat == 1,
    # works for salary
    classwkr == 2,
    # full-time
    uhrswork >= 35, 
    wkswork2 >= 5
  ) |> 
  select(
    year, statefip, perwt, occ2010, inctot, tranwork
  )

# More cleaning; group by income quantiles
data_clean <- data_min |> 
  mutate(
    # actually no missings in this sample
    inctot_clean = if_else(inctot %in% c(9999998, 9999999), NA, inctot)
  ) |> 
  # drop people with negative or missing total income
  filter(inctot_clean > 0 & !is.na(inctot_clean)) |> 
  # group people into quantiles by year based on total income
  group_by(year) |> 
  mutate(
    income_quintile = cut(
      inctot_clean,
      breaks = unique(Hmisc::wtd.quantile(
        x = inctot_clean,
        weights = perwt,
        probs = seq(0, 1, by = 0.2),
        na.rm = TRUE
      )),
      include.lowest = TRUE,
      labels = paste0("Q", 1:(length(unique(Hmisc::wtd.quantile(
        x = inctot_clean,
        weights = perwt,
        probs = seq(0, 1, by = 0.2),
        na.rm = TRUE
      ))) - 1))
    )
  ) |> 
  ungroup()

# Pilots --------------------------------------------------------------
pilots <- data_clean |> 
  filter(occ2010 == 9030) |> 
  mutate(
    zero_PIT = if_else(statefip %in% c(48, 12, 32, 53, 56, 46, 47, 2, 33), "Zero-PIT", "Not Zero-PIT"),
  ) |> 
  group_by(year, zero_PIT) |> 
  summarise(
    n = sum(perwt)
  ) |> 
  ungroup() 


pilots_base <- pilots |> 
  filter(year == 2001) |> 
  select(-year) |> 
  rename(base_n = n)

pilots |> 
  left_join(pilots_base) |> 
  mutate(norm_n = n/base_n) |> 
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


data_clean |> 
  distinct(statefip) |> print(n=Inf)


# Non-pilots --------------------------------------------------------------

xpilots <- data_clean |> 
  filter(statefip!=15 & statefip !=2 & statefip != 48 & statefip != 12) |> 
  filter(occ2010 %in% c(2200, 2300, 2310, 2320)) |> 
  #filter(income_quintile=="Q4") |> 
  mutate(
    zero_PIT = if_else(statefip %in% c(48, 12, 32, 53, 56, 46, 47, 2, 33), "Zero-PIT", "Not Zero-PIT"),
    zero_PIT = factor(zero_PIT, levels = c("Zero-PIT", "Not Zero-PIT"))
  ) |> 
  group_by(year, zero_PIT) |> 
  summarise(
    n = sum(perwt)
  ) |> 
  ungroup() 

# xpilots_base <- xpilots |> 
#   group_by(zero_PIT) |> 
#   summarise(
#     n = mean(n)
#   ) |> 
#   rename(base_n = n)

xpilots_base <- xpilots |> 
  filter(year==2001) |> 
  select(-year) |> 
  rename(base_n = n)


xpilots |> 
  left_join(xpilots_base) |> 
  mutate(norm_n = n / base_n) |> 
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

data_clean |> 
  filter(year==2024, income_quintile=="Q5") 

d_for_reg <- data_clean |> 
  filter(statefip!=15 & statefip !=2) |> 
  #filter(occ2010 == 1020) |> 
  filter(income_quintile=="Q5") |> 
  mutate(
    zero_PIT = if_else(statefip %in% c(48, 12, 32, 53, 56, 46, 47, 2, 33), "Zero-PIT", "Not Zero-PIT"),
    zero_PIT = factor(zero_PIT, levels = c("Zero-PIT", "Not Zero-PIT"))
  ) |> 
  xpilots |> 
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