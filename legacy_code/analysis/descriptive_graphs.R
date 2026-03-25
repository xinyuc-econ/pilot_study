#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 03-16-25                                                     
#                                                                  
# Make descriptive graphs on the ATR pilots data
# 1) Map plots on proportion of pilots in each state in each year
# 2) Map plot on changes in proportion of pilots in each state over time
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
library(usmap)
library(stargazer)
library(ggpattern)
library(binsreg)

# load cleaned data on proportion of ATR pilots in each state by year
dta <- read_csv("clean_data/sum_stat_prop_atr_pilots.csv")

sum_stat_dta <- dta |> 
  mutate(
    n_atr_pilots = n_atr_pilots / 1000,
    tot_work_pop = tot_work_pop / 1000
  ) |> 
  as.data.frame()

# Summary stats table
stargazer(sum_stat_dta, title="Summary Statistics: Number and Percentage of ATR pilots in each state", digits=3, 
          covariate.labels=c("Year","Number of ATR pilots (K)", "Total working pop. (K)", "\\%, ATR pilots"),
          out="output_tables/sum_stat_atr_pilots.tex",
          notes = "Notes: Total working population computed using NBER CPS morg data.",
          notes.align = "l",
          font.size = "small"     
          )

# Put the proportion of ATR pilots in bins
dta_binned <- dta |> 
  mutate(
    prop_bins = cut(prop_atr_pilots, breaks = c(0.01, 0.05, 0.1, 0.2, 0.55))
  )

# check how much variation there is
dta_binned |> 
  count(prop_bins)

# Map plot on proportion of ATR pilots in each state in each year
plot_usmap(
  data = dta_binned,
  regions = "states",
  values = "prop_bins"
) +
  scale_fill_viridis_d(alpha = 0.9, direction = -1) +
  facet_wrap(~ year) +
  labs(
    fill = "",
    title = "Percentage of ATR pilots relative to total working popoluation"
  ) +
  theme(
    legend.position = c(.7, .03),
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 12),
    plot.title = element_text(size = 17)
  ) 
  
ggsave("output_graphs/prop_atr_pilots_map.png")
       
                        
dta_2024 <- dta_binned |> 
  filter(year == 2024) 


# Map plot on proportion of ATR pilots in each state in 2024
plot_usmap(
  data = dta_2024,
  regions = "states",
  values = "prop_bins"
) +
  scale_fill_viridis_d(alpha = 0.9, direction = -1, name = "%") +
  labs(
    fill = "",
    title = "Share of Pilots Relative to State Total Working Popoluation, 2024"
  ) +
  theme(
    legend.position = c(0.9, .03),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    plot.title = element_text(size = 25),
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend box
  ) 


ggsave("output_graphs/prop_atr_pilots_map_2024.png", width = 16, height = 10)

d_dta <- dta |> 
  filter(year %in% c(2009, 2024)) |> 
  arrange(year) |> 
  arrange(state) |> 
  group_by(state) |> 
  mutate(
    lag_prop_atr_pilots = lag(prop_atr_pilots),
    d_prop_atr_pilots = log(prop_atr_pilots) - log(lag_prop_atr_pilots),
    lag_n_atr_pilots = lag(n_atr_pilots),
    d_n_atr_pilots = log(n_atr_pilots) - log(lag_n_atr_pilots)
  ) |> 
  ungroup() |>
  filter(!is.na(d_prop_atr_pilots)) |> 
  mutate(
    d_prop_bins = cut(d_prop_atr_pilots, breaks = c(-0.35, -0.15, -0.01, 0.01, 0.15, 0.4))
  ) 


d_dta |> summary()
plot_usmap(
  data = d_dta,
  regions = "states",
  values = "d_prop_bins"
) +
  scale_fill_viridis_d(alpha = 0.9, direction = -1) +
  labs(
    fill = "",
    title = "Changes in proportion of ATP relative to total working population, 2009-2024"
  ) +
  theme(
    legend.position = c(0.9, .03),
    legend.text = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    plot.title = element_text(size = 25),
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend box
  ) 

ggsave("output_graphs/d_prop_atr_pilots_map.png", width = 16, height = 10)


# Alternatively "where pilots live" plot (wrt tot pilot pop by yr) --------

alt_dta_binned <- dta |> 
  group_by(year) |> 
  mutate(
    N_atr_pilots = sum(n_atr_pilots)
  ) |> 
  ungroup() |> 
  mutate(
    prop_atr_pilots_ofall = n_atr_pilots/N_atr_pilots *100,
    prop_bins = cut(prop_atr_pilots_ofall, breaks = c(0, 1, 2, 3, 10, 13))
  ) 


alt_dta_binned |> 
  summary()

plot_usmap(
  data = alt_dta_binned,
  regions = "states",
  values = "prop_bins"
) +
  scale_fill_viridis_d(alpha = 0.9, direction = -1, name = "%") +
  labs(
    fill = "",
    title = "Share of Pilots Relative to Total # of Pilots, 2024"
  ) +
  theme(
    legend.position = c(0.9, .03),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    plot.title = element_text(size = 25),
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent') #transparent legend box
  ) 

ggsave("output_graphs/prop_atr_pilots_allpilots_map_2024.png")

alt_dta_binned_2024 <- alt_dta_binned |> filter(year == 2024)

m <- lm(prop_atr_pilots_ofall ~ tot_work_pop,
        data = alt_dta_binned_2024)

x <- alt_dta_binned_2024 |> 
  mutate(pred = predict(m, newdata = alt_dta_binned_2024),
         resid = prop_atr_pilots_ofall - pred,
         resid_binned = cut(resid, c(-4, -1, 0, 1, 4, 7)))

summary(x)


# improved graph ----------------------------------------------------------

zero_states <- c("TX","FL","NV","WA","WY","SD","TN","AK","NH")


library(ggpattern)
library(sf)

states_map <- usmap::us_map(regions = "states") %>%
  st_as_sf()


# merge your data (x) onto the sf map
map_sf <- states_map %>%
  left_join(x, by = c("abbr" = "state"))%>%
  mutate(zero_tax = abbr %in% zero_states)

ggplot(map_sf) +
  geom_sf(aes(fill = resid_binned), color = "white", alpha = 0.9) +
  scale_fill_viridis_d(name = "Residual Bin", direction = -1) +
  labs(title = "Residuals from Regression of Pilot Share on State Total Working Population (2024)") +
  theme_minimal()  +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    plot.title = element_text(size = 25),
    legend.title = element_text(size = 20)
  )

ggsave("output_graphs/resid_pilots_share_map_2024.png", width = 13, height = 8)

ggplot(map_sf) +
  geom_sf(aes(fill = resid_binned), color = "white", alpha = 0.9) +
  scale_fill_viridis_d(name = "Residual Bin", direction = -1) +
  geom_sf_pattern(
    data = subset(map_sf, zero_tax),
    aes(pattern = zero_tax),
    fill = NA,
    pattern_color = "black",
    pattern_density = 0.1,
    pattern_spacing = 0.025,   # distance between stripes (smaller = thinner/more stripes)
    pattern_angle = 45        # diagonal stripes
  ) +
  scale_pattern_manual(name = "Zero PIT", values = c("TRUE" = "crosshatch")) +
  labs(title = "Residuals from Regression of Pilot Share on State Total Working Population (2024)") +
  theme_minimal()  +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    plot.title = element_text(size = 25),
    legend.title = element_text(size = 20)
  )

ggsave("output_graphs/resid_pilots_share_map_zerotax_2024.png", width = 13, height = 8)

