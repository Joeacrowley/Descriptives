# =============================================================================
# Exploratory script, NOT a testthat test - nothing here is an assertion.
#
# Builds a small synthetic survey dataset with a predictor level
# (region == "East") that has ZERO real observations, then reproduces
# calc_stats()'s own internals up to (but not including) add_pairwise_sig()
# - the exact point add_empty_predictor_levels() is meant to be called from
# (see that function's own header note in R/add_empty_predictor_levels.R) -
# and prints the three objects it's designed to take: merged_tables, data,
# predictors.
#
# Run with:
#   Rscript dev/explore_empty_predictor_levels.R
# from the package root (dtab/), or source() it from an R session already
# sitting in that directory.
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(forcats)
library(rlang)
library(srvyr)
library(survey)
library(haven)

source("R/calc_stats.R")
source("R/pairwise_testing.R")
source("R/add_empty_predictor_levels.R")

# ---- synthetic data ---------------------------------------------------------
# "region" is declared with THREE levels (North/South/East), but only North
# and South ever appear in a row - East is the empty level this whole
# exercise is about. wt = 1, ids = 1 (unweighted, unclustered) so the
# weighted/svyby() path runs (same shape as calc_stats(pairwise = TRUE)'s
# real use case, where a `covmat` list-column is present).
survey_data <- tibble(
  response = factor(c(rep("Yes", 8), rep("No", 2),     # North: 8/10 Yes
                       rep("Yes", 3), rep("No", 7)),    # South: 3/10 Yes
                     levels = c("No", "Yes")),
  region   = factor(c(rep("North", 10), rep("South", 10)),
                     levels = c("North", "South", "East")),   # <- East never appears in a row
  wt       = rep(1, 20)
)

design <- survey_data %>% as_survey_design(ids = 1, weights = wt)

# ---- reproduce calc_stats()'s own internals up to just before ------------
# ---- add_pairwise_sig() - see calc_stats.R's dispatcher for the original --
# ---- of this (the individual_tables / merged_tables / p_method block) -----
outcomes   <- "response"
predictors <- list("region")   # calc_stats() normalises a bare character
                                 # vector to this shape too - see its own
                                 # `if (is.character(predictors))` line.

# weighted_perc_svyby(), not weighted_perc() - this is the function
# calc_stats(pairwise = TRUE) actually swaps "w_perc" to (see
# active_stat_registry in calc_stats()), and it's the one that produces the
# `covmat` list-column add_empty_predictor_levels() has to cope with.
merged_tables <- weighted_perc_svyby(data = design, outcomes = outcomes,
                                      predictors = predictors, conf = "se") %>%
  mutate(p_method = NA, p_value = NA)   # calc_stats()'s own "pval not requested" branch

cat("\n================ INPUT 1: merged_tables ================\n")
print(merged_tables, n = Inf, width = Inf)

cat("\n---- column names ----\n")
print(names(merged_tables))

cat("\n---- East (the empty level) - confirm it's genuinely absent ----\n")
print(merged_tables %>% filter(p_cat1 == "East"), width = Inf)

cat("\n================ INPUT 2: data ================\n")
cat("class(design):", paste(class(design), collapse = ", "), "\n")
cat("levels(underlying region column):", paste(levels(design$variables$region), collapse = ", "), "\n")

cat("\n================ INPUT 3: predictors ================\n")
print(predictors)

# ---- now actually call the helper and show what it does -------------------
filled <- add_empty_predictor_levels(merged_tables, design, predictors)

cat("\n================ add_empty_predictor_levels() OUTPUT ================\n")
cat("nrow before:", nrow(merged_tables), " / nrow after:", nrow(filled), "\n\n")
print(filled %>% filter(p_cat1 == "East"), width = Inf)
