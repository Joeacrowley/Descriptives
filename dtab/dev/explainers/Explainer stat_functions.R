# Explainer: the stat functions (stat_functions_draft.R)
# Plain maths: given grouped data and an outcome name, return one row per group. one_table() does the grouping.
# Paths use here::here(), so it runs from anywhere in the Descriptives project.

suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
source(here::here("dtab", "dev", "calc_stat_engine_draft.R"))

bsa <- readRDS(here::here("data", "synthetic_bsa2019.rds"))


# The registry: every stat's function, type and whether it's weighted.
names(stat_registry)
stat_registry$mean


# ---- stat_mean() (numeric) ----
# Input: data grouped by the predictor(s), NA rows already removed.
grouped <- bsa %>%
  filter(!is.na(leftrigh), !is.na(RSex)) %>%
  group_by(RSex)

stat_mean(grouped, outcome = "leftrigh", conf = NULL)
# One row per group. Columns keep their raw names (RSex) - one_table() standardises them later.
# conf is ignored here: SE and CI are always calculated, and the engine keeps what was asked for.


# ---- stat_perc() (categorical) ----
# Input: grouped by the predictor(s) AND the outcome.
grouped_cat <- bsa %>%
  filter(!is.na(MuchPov), !is.na(RSex)) %>%
  group_by(RSex, MuchPov)

stat_perc(grouped_cat, outcome = "MuchPov", conf = NULL)
# summarise(.groups = "drop_last") leaves it grouped by RSex, so each % is out of that sex's total.
# Categories nobody picked aren't here yet - one_table() adds them as 0 rows.


# ---- stat_w_mean() (weighted) ----
# Same idea, but the input is a survey design and conf goes to the survey package.
grouped_svy <- srvyr::as_survey_design(bsa, weights = WtFactor) %>%
  filter(!is.na(leftrigh), !is.na(RSex)) %>%
  group_by(RSex)

stat_w_mean(grouped_svy, outcome = "leftrigh", conf = "se")
stat_w_mean(grouped_svy, outcome = "leftrigh", conf = "ci")
# Only the columns asked for come back: estimate_se, or estimate_low/estimate_upp.
