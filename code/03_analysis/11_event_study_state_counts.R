# Purpose: estimate a simple event-study specification for state-year pilot counts.
# Inputs: `data/derived/aviationdb/event_study_state_counts_panel.csv`
# Outputs: one figure and one regression table in `output/aviationdb/`

# Setup ----

source("code/00_setup/00_packages_paths.R")

dir.create(paths$figures_aviationdb, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$tables_aviationdb, recursive = TRUE, showWarnings = FALSE)

figure_path <- file.path(paths$figures_aviationdb, "event_study_state_counts.png")
table_path <- file.path(paths$tables_aviationdb, "event_study_state_counts.tex")

# Input Loading ----

event_study_panel <- read_csv(
  file.path(paths$derived_aviationdb, "event_study_state_counts_panel.csv"),
  show_col_types = FALSE
) |>
  mutate(
    year = as.integer(year),
    atr_group = as.integer(atr_group)
  )

# 1. Event-Study Regression ----

event_study_model <- feols(
  log_n_pilots ~ i(year, atr_group, ref = 2017) | state + year,
  data = event_study_panel,
  cluster = ~state
)

# 2. Figure ----

event_study_terms <- tibble(
  term = names(coef(event_study_model)),
  estimate = unname(coef(event_study_model))
) |>
  left_join(
    tibble(
      term = rownames(confint(event_study_model)),
      conf.low = confint(event_study_model)[, 1],
      conf.high = confint(event_study_model)[, 2]
    ),
    by = "term"
  ) |>
  mutate(
    year = as.integer(str_match(term, "year::(\\d+):")[, 2]),
    event_time = year - 2017L
  ) |>
  filter(!is.na(year)) |>
  arrange(year)

event_study_plot <- event_study_terms |>
  ggplot(aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 1, linetype = "dotted", color = "gray50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = event_study_terms$event_time) +
  labs(
    x = "Event time relative to 2017",
    y = "ATP vs other coefficient on log state-year pilot counts",
    title = "Event-study estimates for log state-year pilot counts",
    subtitle = "Control group excludes commercial pilots; event time 1 corresponds to 2018"
  ) +
  theme_bw()

ggsave(figure_path, plot = event_study_plot, width = 9, height = 6)

# 3. Table ----

etable(
  event_study_model,
  tex = TRUE,
  replace = TRUE,
  digits = 4,
  depvar = FALSE,
  title = "Event-study specification for log state-year pilot counts",
  file = table_path
)

# Reporting ----

message("Wrote event-study figure to ", figure_path)
message("Wrote event-study table to ", table_path)
