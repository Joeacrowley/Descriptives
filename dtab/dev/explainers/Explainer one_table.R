# Explainer: one_table()
# Builds one table (one outcome x one predictor set); calc_stat_engine() stacks them.
# Paths use here::here(), so it runs from anywhere in the Descriptives project.

suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
source(here::here("dtab", "dev", "calc_stat_engine_draft.R"))

bsa <- readRDS(here::here("data", "synthetic_bsa2019.rds"))

# The stat's registry entry: its maths function and type.
spec <- stat_registry$mean
spec


# Total only example...
one_table(bsa, outcome = "leftrigh", predictor = NULL,
          stat = "mean", spec = spec, conf = NULL)
# No predictor columns yet - calc_stat_engine() fills them with "Total" when stacking.


# One predictor...
tbl_flat <- one_table(bsa, outcome = "leftrigh", predictor = "RAgeCat",
                      stat = "mean", spec = spec, conf = NULL)
tbl_flat
# Rows missing leftrigh or RAgeCat are dropped, so these bases sum to less than the Total's.
sum(tbl_flat$base)


# Nested predictors (RSex x RAgeCat)...
levels(bsa$RSex)
one_table(bsa, outcome = "leftrigh", predictor = c("RSex", "RAgeCat"),
          stat = "mean", spec = spec, conf = NULL) %>% 
  print(n=1000)
# RSex has unused levels ("Don`t know", "Refusal"...) - empty groups get no row.


# With conf = "se"...
# SE and CI columns all come back; 
# Other functions, e.g. weighted_mean, will return only SE or CI
# Later, calc_stat_engine() keeps only the ones asked for so result is consistent
one_table(bsa, outcome = "leftrigh", predictor = "RSex",
          stat = "mean", spec = spec, conf = "ci") %>% glimpse

