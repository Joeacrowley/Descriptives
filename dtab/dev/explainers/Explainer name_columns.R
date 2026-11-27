# Explainer: standardise_names() 
# Sits inside `one_table` inside `calc_stat_engine`
# Paths use here::here(), so it runs from anywhere in the Descriptives project.

suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
source(here::here("dtab", "dev", "calc_stat_engine_draft.R"))

bsa <- readRDS(here::here("data", "synthetic_bsa2019.rds"))


# Example tibble, before standardising. 
tbl <- tibble(
  RSex     = c("Male", "Male", "Female", "Female"),
  RAgeCat  = c("18-24", "25-34", "18-24", "25-34"),
  estimate = c(2.9, 3.1, 2.7, 3.0),
  base     = c(120, 210, 140, 230),
  o_cat    = "mean"
)
tbl

# Standardised...
standardise_names(
  tbl, 
  data = bsa, 
  outcome = "leftrigh", 
  predictor = c("RSex", "RAgeCat")
  )
# Variable order is set later, so it looks a mess.  
# Order set by `select()` in `calc_stats_engine` that sets the columns to keep.

# Total only example...
tbl_total <- tibble(estimate = 2.95, base = 2567, o_cat = "mean")
tbl_total

standardise_names(tbl_total, data = bsa, outcome = "leftrigh", predictor = NULL)
# `cross_break` variable's value here is simply 'total'. 

# Categorical outcome
# Here the outcome's own column exists, holding its categories - it becomes o_cat.
tbl_cat <- tibble(
  RSex     = c("Male", "Male", "Female", "Female"),
  MuchPov  = c("Very little poverty", "Quite a lot", "Very little poverty", "Quite a lot"),
  estimate = c(0.35, 0.65, 0.30, 0.70)
)
tbl_cat

standardise_names(tbl_cat, data = bsa, outcome = "MuchPov", predictor = "RSex")


# Complex example: 4 nested predictors (RSex x RAgeCat x Married x HEdQual3)
# Each predictor in turn becomes p_catN, with its own predictorN and p_labN.
tbl_complex <- tibble(
  RSex     = c("Male", "Male", "Female", "Female"),
  RAgeCat  = c("18-24", "65+", "18-24", "65+"),
  Married  = c("Never married", "Married/living as married", "Never married", "Widowed"),
  HEdQual3 = c("Degree", "No qualification", "Degree", "O level or equiv/CSE"),
  estimate = c(2.6, 3.4, 2.5, 3.2),
  base     = c(18, 95, 22, 61),
  o_cat    = "mean"
)
tbl_complex

result <- standardise_names(
  tbl_complex,
  data = bsa,
  outcome = "leftrigh",
  predictor = c("RSex", "RAgeCat", "Married", "HEdQual3")
  )

result
# cross_break joins all four: "RSex_X_RAgeCat_X_Married_X_HEdQual3".

result %>% glimpse
