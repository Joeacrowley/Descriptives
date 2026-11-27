# Explainer: calc_stat_engine()

# It deals with one outcome and one statistic at a time. 
# For a count and percentage, then `calc_stats` passes the requests separately.
# Likewise, for two outcomes, they are passed separately. 
# But, it loops over as many predictor sets as required. 
# The different predictor's results are bound together into a data frame. 
# The data frame has consistent variable names so different predictor variables
# can be stacked together. 

# Paths use here::here(), so it runs from anywhere in the Descriptives project.
suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
source(here::here("dtab", "dev", "calc_stat_engine_draft.R"))

bsa <- readRDS(here::here("data", "synthetic_bsa2019.rds"))


# The tables it will build: NULL (the Total) first, then each predictor set.
predictors <- list("RSex", c("RSex", "RAgeCat"))
c(list(NULL), predictors)


# Total + flat + nested, stacked...
result <- calc_stat_engine(bsa, outcome = "leftrigh", predictors = predictors, stat = "mean")
result
# Total rows have no predictor columns of their own, so those are filled with "Total".

result %>% count(cross_break)
# One block of rows per table: 1 Total, 2 RSex, 14 RSex x RAgeCat.


# Column order is fixed here, by select()...
names(result)
# unweighted_n = base for numeric stats (one row per group).


# conf decides which uncertainty columns are kept...
calc_stat_engine(bsa, "leftrigh", stat = "mean", conf = NULL) %>% names()
calc_stat_engine(bsa, "leftrigh", stat = "mean", conf = "se") %>% names()
calc_stat_engine(bsa, "leftrigh", stat = "mean", conf = "ci") %>% names()


# Stats with no SE/CI (min, iqr, sd...) get NA columns, so conf = "ci" still works.
calc_stat_engine(bsa, "leftrigh", predictors = list("RSex"), stat = "min", conf = "ci")


# Categorical stat (perc)...
calc_stat_engine(bsa, "MuchPov", predictors = list("RSex"), stat = "perc") %>%
  select(cross_break, p_cat1, o_cat, estimate, unweighted_n, base)
# o_cat holds the outcome's categories; outcome_type is "categorical".


# Weighted stat: same call, but data is a survey design.
svy <- srvyr::as_survey_design(bsa, weights = WtFactor)
calc_stat_engine(svy, "leftrigh", predictors = list("RSex"), stat = "w_mean", conf = "se")


# An unknown stat name stops straight away.
try(calc_stat_engine(bsa, "leftrigh", stat = "mode"))
try(calc_stat_engine(bsa, "leftrigh", stat = "mean")) # Whereas, this is fine.
