#------------------------------------------------------------- 
# Xinyu Chen                                                   
# 03-16-25                                                     
#                                                                  
# Merge pilots data with tax rate data
# Produce scatterplots and simple OLS results
#                                                              
#-------------------------------------------------------------- 
library(tidyverse)
library(janitor)
library(haven)
library(readxl)
library(ggrepel)
library(stargazer)
library(xtable)
library(binsreg)

# read in data 
prop_pilots <- read_csv("clean_data/sum_stat_prop_atr_pilots.csv") 

pit <- read_csv("clean_data/all_years_pit_wide.csv") |> select(year, fips, starts_with("astr"), starts_with("atr"))


state_fip_xwalk <- read_excel("data/xwalks/StateFIPSicsprAB.xls") |> 
  clean_names() |> 
  select(fips, name, ab) |> 
  rename(
    statefull = name,
    state = ab
  )

pit_processed <- pit |> 
  left_join(state_fip_xwalk) |> 
  select(-fips, -statefull)

m <- prop_pilots |> 
  left_join(pit_processed) |> 
  mutate(
    astr_p95 = astr_p95*100,
    atr_p95 = atr_p95*100
  )


sum_stat_zero_vs_pos_tax <-  m |> 
    filter(year != 2024) |>
  mutate(
    zero_tax = (astr_p95 == 0)
  ) |> 
  group_by(year, zero_tax) |> 
  summarize(
    mean_prop_atr_pilots = mean(prop_atr_pilots),
    mean_atr_p95 = mean(atr_p95)
  ) |> 
  pivot_wider(names_from = zero_tax, values_from = c(mean_prop_atr_pilots, mean_atr_p95)) |> 
    rename (
      `Prop. Pilots (zero tax)` = `mean_prop_atr_pilots_TRUE`,
      `Prop. Pilots (pos. tax)` = `mean_prop_atr_pilots_FALSE`,
      `ATR (zero tax)` = `mean_atr_p95_TRUE`,
      `ATR (pos. tax)` = `mean_atr_p95_FALSE`
    ) |> 
  as.data.frame()

print(xtable(sum_stat_zero_vs_pos_tax, type = "latex", caption = "Proportion of pilots and ATR by year, postive tax states versus zero tax states", digits=c(0,0,3,3,2,2)), file = "output_tables/sum_stat_zero_vs_pos_tax.tex")

xtable(sum_stat_zero_vs_pos_tax)

# Scatterplot: relationship between proportion of pilots and average state tax rate, all years pooled
m |> 
  filter(state != "AK") |> 
  ggplot(aes(x = astr_p95, y = prop_atr_pilots)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(
    x = "Average state tax rate, 95th percentile income (%)",
    y = "Prop. of total working pop. that is ATR pilots (%)",
    fill = ""  ) +
  theme_bw()+
  theme(
    legend.position = c(0.9, .03),
    axis.title  = element_text(size = 20),
    axis.text = element_text(size = 17),
    strip.text.x = element_text(size = 20),
    plot.title = element_text(size = 18)
  ) 

ggsave("output_graphs/pit_prop_astr_pilots_all_years_noAK.png", width = 10, height = 8)


# Scatterplot: relationship between proportion of pilots and average tax rate, all years pooled
m |> 
  filter(state != "AK") |> 
  ggplot(aes(x = atr_p95, y = prop_atr_pilots)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(15, 30)) +
  labs(
    x = "Average tax rate, 95th percentile income (%)",
    y = "Prop. of total working pop. that is ATR pilots (%)",
    fill = ""  ) +
  theme_bw()+
  theme(
    legend.position = c(0.9, .03),
    axis.title  = element_text(size = 20),
    axis.text = element_text(size = 17),
    strip.text.x = element_text(size = 20),
    plot.title = element_text(size = 18)
  ) 
ggsave("output_graphs/pit_prop_atr_pilots_all_years_noAK.png", width = 10, height = 8)


reg_astr_p95 <- lm(prop_atr_pilots ~ astr_p95, data = m |> filter(state != "AK")) 
summary(reg_astr_p95)


reg_astr_p95 <- lm(prop_atr_pilots ~ atr_p95, data = m |> filter(state != "AK")) 
summary(reg_astr_p95)


# Binned scatterplot: relationship between proportion of pilots and average state tax rate, all years pooled
binscatter_m <- m |> 
  as.data.frame() 

p <- binsreg(prop_atr_pilots, astr_p95,
             data = binscatter_m,
             polyreg = 3)

p$bins_plot + 
  labs(
    x = "Average state tax rate, 95th percentile income (%)",
    y = "Share of pilots in tot. working pop. (%)") +
  theme(
    axis.title  = element_text(size = 24),
    axis.text = element_text(size = 20),
    strip.text.x = element_text(size = 28)
  ) 

ggsave("output_graphs/binscatter_astr_pilots.png", width = 10, height = 8)

# robustness: no AK
p_noAK <- binsreg(prop_atr_pilots, astr_p95,
                  data = binscatter_m |> filter(state != "AK"),
                  polyreg = 3)

p_noAK$bins_plot + 
  labs(
    x = "Average state tax rate, 95th percentile income (%)",
    y = "Share of pilots in tot. working pop. (%)") +
  theme(
    axis.title  = element_text(size = 24),
    axis.text = element_text(size = 20),
    strip.text.x = element_text(size = 28)
  )  

ggsave("output_graphs/binscatter_astr_pilots_noAK.png", width = 10, height = 8)

