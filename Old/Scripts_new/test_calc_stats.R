# =============================================================================
# KNOWN-ANSWER TESTS for calc_stats.R
#
# I have NOT run these myself — I don't have R available in this environment
# (no root/sudo access to install it), so these are unverified. Run them on
# your machine with:
#
#   testthat::test_file(here::here("Scripts_new", "test_calc_stats.R"))
#
# Where hand-calculation is practical, each test builds a tiny synthetic
# dataset small enough that the "correct" answer is worked out by hand in a
# comment above it, rather than trusting either the old or new code. Where
# hand-calculation isn't practical (bootstrap CIs, chi-square/Kruskal-Wallis
# p-values), the test checks structure and plausibility instead (right method
# chosen, p in [0,1], NA where NA is expected) rather than an exact number.
#
# 44 tests, organised by what they cover:
#   1-3   unweighted_mean      (total, grouped, conf = NULL behaviour)
#   4-5   weighted_mean        (total, grouped)
#   6-7   unweighted_median    (small-subgroup NA-CI fix, conf = "se" sanity)
#   8     weighted_median      (the easy case — survey_median has vartype built in)
#   9-10  unweighted_sum       (total, grouped)
#   11    weighted_sum
#   12-13 unweighted_perc      (total, grouped)
#   14-15 weighted_perc        (known proportion, discrepancy #1 fix)
#   16-17 unweighted_count     (discrepancy #2 fix, total case)
#   18    weighted_count
#   19-23 calc_stats()         (statistic-type filtering, Total labelling,
#                                multiple predictor sets, nested predictors,
#                                validation errors)
#   24-27 significance tests   (all 4 test functions via return_pvalues(),
#                                nested-predictor p-value skip)
#   28    list_depth() / pluck_depth() convention
#   29    input validation helpers + common_prefix()
#   30    base_information() / create_bases()
#   31    weighted_median()/weighted_sum() correctly zap_labels() on a real
#         haven_labelled column (discrepancy #6) — added after the fact, on
#         top of the 30-test cap, specifically because tests 8 and 11 use
#         plain numeric columns and wouldn't have caught this
#   32-35 outcome_type stamping (numeric / categorical / multicoded)
#   36-42 min/max/range/iqr/sd  (unweighted_min/max/range/iqr/sd,
#                                weighted_iqr/weighted_sd — point estimates
#                                only, per the "point estimates only" decision
#                                for this first pass)
#   43    calc_stats() only computes the new numeric-only stats for numeric
#         outcomes, same filtering logic as test 19 but for the new names
#   44    calc_stats() computes shared p-values for mean+sd together,
#         confirming stat_labels/pval-trigger were actually extended to
#         cover the new stats, not just calc_stats()'s registry
#   45    calc_stats() doesn't crash when pval = TRUE and every predictor
#         set is nested (regression test for a real crash caught by
#         pivot_nested_crosstab.R's own test suite)
# =============================================================================
# file.create(here::here(".here"))
# setwd('/Users/joecrowley/5 - R/Descriptives')
#
library(testthat)
source(here::here("Scripts_new", "calc_stats.R"))


# =============================================================================
# unweighted_mean
# =============================================================================

# x = 10, 20, 10, 20 -> mean = 60/4 = 15
# deviations from 15: -5, 5, -5, 5 -> sum of squares = 100 -> var = 100/3 = 33.3333
# sd = 5.773503 -> se = sd/sqrt(4) = 2.886751

test_that("1. unweighted_mean matches a hand-calculated mean and SE (total only)", {
  data <- tibble(x = c(10, 20, 10, 20))
  result <- unweighted_mean(data, outcomes = "x", conf = "se")

  expect_equal(result$estimate, 15)
  expect_equal(result$base, 4)
  expect_equal(result$estimate_se, 2.886751, tolerance = 1e-5)
  expect_equal(result$o_lab, "x")   # no label attribute on x -> falls back to the variable name
})


# Group A: x = 10, 20, 10, 20 -> same as test 1: mean = 15
# Group B: x = 30, 30         -> mean = 30, sd = 0 -> se = 0

test_that("2. unweighted_mean matches hand-calculated group means", {
  data <- tibble(
    x = c(10, 20, 10, 20, 30, 30),
    g = factor(c("A", "A", "A", "A", "B", "B"))
  )
  result <- unweighted_mean(data, outcomes = "x", predictors = list("g"), conf = "se")

  group_a <- result %>% filter(p_cat1 == "A")
  group_b <- result %>% filter(p_cat1 == "B")

  expect_equal(group_a$estimate, 15)
  expect_equal(group_b$estimate, 30)
  expect_equal(group_b$estimate_se, 0)
})


test_that("3. conf = NULL blanks the SE and drops the CI-bound columns", {
  data <- tibble(x = c(1, 2, 3, 4, 5))
  result <- unweighted_mean(data, outcomes = "x")   # conf defaults to NULL

  expect_equal(result$estimate_se, "-")
  expect_false(any(c("estimate_low", "estimate_upp") %in% names(result)))
})


# =============================================================================
# weighted_mean
# =============================================================================

# weighted mean of x = 10,20,10,20 with weights 1,1,2,2
#   = (10*1 + 20*1 + 10*2 + 20*2) / (1+1+2+2) = 90/6 = 15
# (same example used earlier to explain "known answer synthetic data")

test_that("4. weighted_mean matches a hand-calculated weighted average", {
  svy <- tibble(x = c(10, 20, 10, 20), w = c(1, 1, 2, 2)) %>%
    as_survey_design(weight = w)
  result <- weighted_mean(svy, outcomes = "x")

  expect_equal(result$estimate, 15)
})


# Group A: x = 10, 20, weights 1, 1 -> weighted mean = 15
# Group B: x = 30, 40, weights 1, 1 -> weighted mean = 35

test_that("5. weighted_mean matches hand-calculated weighted group means", {
  svy <- tibble(
    x = c(10, 20, 30, 40),
    w = c(1, 1, 1, 1),
    g = factor(c("A", "A", "B", "B"))
  ) %>% as_survey_design(weight = w)

  result <- weighted_mean(svy, outcomes = "x", predictors = list("g"))

  expect_equal(result %>% filter(p_cat1 == "A") %>% pull(estimate), 15)
  expect_equal(result %>% filter(p_cat1 == "B") %>% pull(estimate), 35)
})


# =============================================================================
# unweighted_median (discrepancy #3) / weighted_median
# =============================================================================

# Group "small" has 3 observations -> grouped_medianci() drops it (n <= 5),
# so its CI should come through as a genuine NA, not the string "NA - NA".
# Group "big" has 8 observations -> CI should be computed (some non-NA string).
# Medians are exact regardless of the CI: small = median(1,2,3) = 2,
# big = median(1..8) = 4.5.

test_that("6. unweighted_median renders a dropped small-subgroup CI as real NA", {
  set.seed(123)
  median_data <- tibble(
    g = factor(c(rep("small", 3), rep("big", 8))),
    x = c(1, 2, 3, 1, 2, 3, 4, 5, 6, 7, 8)
  )
  result <- unweighted_median(median_data, outcomes = "x", predictors = list("g"), conf = "ci")

  small_row <- result %>% filter(p_cat1 == "small")
  big_row   <- result %>% filter(p_cat1 == "big")

  expect_equal(small_row$estimate, 2)
  expect_equal(big_row$estimate, 4.5)
  expect_true(is.na(small_row$estimate_ci))
  expect_false(is.na(big_row$estimate_ci))
})


# x = 1,2,3,4,5,100 (even n) -> sorted median = (3rd + 4th)/2 = (3+4)/2 = 3.5
# wrappedtools::medianse()'s exact algorithm isn't hand-verifiable here, so
# this only checks the point estimate exactly and the SE for being a sane
# positive number, not medianse()'s specific formula.

test_that("7. unweighted_median total case: exact median, sane SE", {
  data <- tibble(x = c(1, 2, 3, 4, 5, 100))
  result <- unweighted_median(data, outcomes = "x", conf = "se")

  expect_equal(result$estimate, 3.5)
  expect_true(is.numeric(result$estimate_se))
  expect_true(result$estimate_se > 0)
})


# x = 10,20,30,40,50 with weights 1,1,20,1,1 (total weight 24, half = 12).
# Cumulative weight sorted by x: 1, 2, 22, 23, 24 -> the 12-point falls deep
# inside the x=30 block (cumulative 2 to 22), not on a boundary between two
# values, so the weighted median is unambiguously 30 regardless of exactly
# how survey_median() interpolates.

test_that("8. weighted_median matches a hand-calculated weighted median", {
  svy <- tibble(x = c(10, 20, 30, 40, 50), w = c(1, 1, 20, 1, 1)) %>%
    as_survey_design(weight = w)
  result <- weighted_median(svy, outcomes = "x")

  expect_equal(result$estimate, 30)
})


# =============================================================================
# unweighted_sum / weighted_sum
# =============================================================================

# x = 5,10,15,20 -> sum = 50, n = 4, mean = 12.5
# deviations: -7.5,-2.5,2.5,7.5 -> sum of squares = 125 -> var = 125/3 = 41.6667
# sd = 6.454972 -> se = sd * sqrt(n) = 6.454972 * 2 = 12.909944

test_that("9. unweighted_sum matches a hand-calculated total and SE (total only)", {
  data <- tibble(x = c(5, 10, 15, 20))
  result <- unweighted_sum(data, outcomes = "x", conf = "se")

  expect_equal(result$estimate, 50)
  expect_equal(result$base, 4)
  expect_equal(result$estimate_se, 12.909944, tolerance = 1e-5)
})


# Group A: x = 5,10,15,20 -> sum = 50 (same as test 9, se = 12.909944)
# Group B: x = 100,200    -> sum = 300, sd = sqrt(5000) = 70.710678
#          se = sd * sqrt(2) = sqrt(5000)*sqrt(2) = sqrt(10000) = 100 exactly

test_that("10. unweighted_sum matches hand-calculated group totals", {
  data <- tibble(
    x = c(5, 10, 15, 20, 100, 200),
    g = factor(c("A", "A", "A", "A", "B", "B"))
  )
  result <- unweighted_sum(data, outcomes = "x", predictors = list("g"), conf = "se")

  group_a <- result %>% filter(p_cat1 == "A")
  group_b <- result %>% filter(p_cat1 == "B")

  expect_equal(group_a$estimate, 50)
  expect_equal(group_a$estimate_se, 12.909944, tolerance = 1e-5)
  expect_equal(group_b$estimate, 300)
  expect_equal(group_b$estimate_se, 100, tolerance = 1e-6)
})


# x = 10,20,30 with weight 1 each -> weighted total = 10+20+30 = 60
# (equal weight-1 makes this equivalent to a plain sum, avoiding any
# ambiguity about survey_total()'s scaling)

test_that("11. weighted_sum matches a hand-calculated weighted total", {
  svy <- tibble(x = c(10, 20, 30), w = c(1, 1, 1)) %>% as_survey_design(weight = w)
  result <- weighted_sum(svy, outcomes = "x")

  expect_equal(result$estimate, 60)
})


# =============================================================================
# unweighted_perc / weighted_perc
# =============================================================================

# 3 "Yes" out of 10 rows -> estimate = 0.3
# se = sqrt(0.3 * 0.7 / 10) = sqrt(0.021) = 0.1449138

test_that("12. unweighted_perc matches a hand-calculated proportion and SE", {
  data <- tibble(y = factor(c(rep("Yes", 3), rep("No", 7))))
  result <- unweighted_perc(data, outcomes = "y", conf = "se")

  yes_row <- result %>% filter(o_cat == "Yes")

  expect_equal(yes_row$estimate, 0.3)
  expect_equal(yes_row$estimate_se, 0.1449138, tolerance = 1e-5)
})


# Group A: 4 Yes out of 10 -> 0.4
# Group B: 8 Yes out of 10 -> 0.8

test_that("13. unweighted_perc matches hand-calculated group proportions", {
  data <- tibble(
    g = factor(c(rep("A", 10), rep("B", 10))),
    y = factor(c(rep("Yes", 4), rep("No", 6), rep("Yes", 8), rep("No", 2)))
  )
  result <- unweighted_perc(data, outcomes = "y", predictors = list("g"), conf = "se")

  expect_equal(result %>% filter(p_cat1 == "A", o_cat == "Yes") %>% pull(estimate), 0.4)
  expect_equal(result %>% filter(p_cat1 == "B", o_cat == "Yes") %>% pull(estimate), 0.8)
})


# y = Yes,Yes,No,No,No with weights 2,2,1,1,1 -> total weight = 7, Yes weight = 4
# proportion = 4/7 = 0.5714286

test_that("14. weighted_perc matches a hand-calculated weighted proportion", {
  svy <- tibble(y = factor(c("Yes", "Yes", "No", "No", "No")), w = c(2, 2, 1, 1, 1)) %>%
    as_survey_design(weight = w)
  result <- weighted_perc(svy, outcomes = "y")

  expect_equal(result %>% filter(o_cat == "Yes") %>% pull(estimate), 4 / 7, tolerance = 1e-6)
})


# Discrepancy #1 check: group A has 6 rows (2 Yes, 4 No), all weight 1.
# unweighted_n for the Yes/A cell should be the real category count, 2 — not
# overwritten with the group's total base (6), which is what the original
# weighted_perc.R did before this fix.

test_that("15. weighted_perc keeps the real per-category unweighted_n (discrepancy #1)", {
  svy <- tibble(
    g = factor(c(rep("A", 6), rep("B", 6))),
    y = factor(c(rep("Yes", 2), rep("No", 4), rep("Yes", 3), rep("No", 3))),
    w = rep(1, 12)
  ) %>% as_survey_design(weight = w)

  result <- weighted_perc(svy, outcomes = "y", predictors = list("g"))
  row <- result %>% filter(p_cat1 == "A", o_cat == "Yes")

  expect_equal(row$unweighted_n, 2)   # would have been 6 under the old buggy behaviour
  expect_equal(row$base, 6)
})


# =============================================================================
# unweighted_count (discrepancy #2) / weighted_count
# =============================================================================

# Group A has 10 rows across 3 categories: cat1=2, cat2=3, cat3=5
# For group A / cat1: estimate = 2, base (group total) = 10
#   CORRECT:  prop = estimate / base         = 2/10   = 0.2
#             se   = sqrt(base*prop*(1-prop)) = sqrt(10*0.2*0.8) = sqrt(1.6) = 1.264911
#   ORIGINAL (buggy): prop = estimate / sum(base), where base was already a
#             length-3 repeated column within this group, so sum(base) = 30
#             prop = 2/30 = 0.06667 -> se = sqrt(10*0.06667*0.93333) = 0.7888
# This test checks the corrected value; the original would have failed it.

test_that("16. unweighted_count uses base, not sum(base), for the proportion (discrepancy #2)", {
  count_data <- tibble(
    g = factor(c(rep("A", 10), rep("B", 10))),
    y = factor(c(rep("cat1", 2), rep("cat2", 3), rep("cat3", 5),
                 rep("cat1", 4), rep("cat2", 4), rep("cat3", 2)))
  )
  result <- unweighted_count(count_data, outcomes = "y", predictors = list("g"), conf = "se")

  row <- result %>% filter(p_cat1 == "A", o_cat == "cat1")

  expect_equal(row$estimate, 2)
  expect_equal(row$base, 10)
  expect_equal(row$estimate_se, sqrt(10 * 0.2 * 0.8), tolerance = 1e-6)
})


# y = Yes(4), No(6) -> Yes: estimate = 4, base = 10, prop = 0.4
# se = sqrt(base*prop*(1-prop)) = sqrt(10*0.4*0.6) = sqrt(2.4) = 1.549193

test_that("17. unweighted_count matches a hand-calculated total-case count and SE", {
  data <- tibble(y = factor(c(rep("Yes", 4), rep("No", 6))))
  result <- unweighted_count(data, outcomes = "y", conf = "se")

  yes_row <- result %>% filter(o_cat == "Yes")

  expect_equal(yes_row$estimate, 4)
  expect_equal(yes_row$base, 10)
  expect_equal(yes_row$estimate_se, sqrt(2.4), tolerance = 1e-6)
})


# y = Yes,Yes,No with weights 2,3,5 -> weighted count of "Yes" = 2+3 = 5

test_that("18. weighted_count matches a hand-calculated weighted total", {
  svy <- tibble(y = factor(c("Yes", "Yes", "No")), w = c(2, 3, 5)) %>%
    as_survey_design(weight = w)
  result <- weighted_count(svy, outcomes = "y")

  expect_equal(result %>% filter(o_cat == "Yes") %>% pull(estimate), 5)
})


# =============================================================================
# calc_stats() dispatch
# =============================================================================

# statistics = c("mean", "perc") requested for both a numeric and a factor
# outcome. The numeric outcome should only get "mean" rows (its "perc" gets
# filtered out), the factor outcome only "perc" rows — this is the
# str_remove_all-based filtering inside calc_stats(), unchanged from the
# original but now feeding the registry loop instead of 10 if-blocks.

test_that("19. calc_stats() only computes statistics valid for each outcome's type", {
  data <- tibble(
    outcome_num = c(10, 20, 30, 40, 50, 60),
    outcome_cat = factor(c("Yes", "Yes", "Yes", "No", "No", "No")),
    group       = factor(c("A", "A", "B", "B", "A", "B"))
  )
  result <- calc_stats(data, outcomes = c("outcome_num", "outcome_cat"),
                        predictors = "group", statistics = c("mean", "perc"),
                        multicode = FALSE)

  expect_equal(result %>% filter(outcome == "outcome_num") %>% pull(stat) %>% unique(), "mean")
  expect_equal(result %>% filter(outcome == "outcome_cat") %>% pull(stat) %>% unique(), "perc")
})


test_that("20. calc_stats() labels cross_break as 'Total' when no predictors are given", {
  data <- tibble(outcome_num = c(1, 2, 3, 4, 5))
  result <- calc_stats(data, outcomes = "outcome_num", statistics = "mean", multicode = FALSE)

  expect_equal(unique(result$cross_break), "Total")
})


test_that("21. calc_stats() breaks down by each predictor set separately when given a list of single variables", {
  data <- tibble(
    outcome_num = c(10, 20, 30, 40, 50, 60, 70, 80),
    group1 = factor(c("A", "A", "A", "A", "B", "B", "B", "B")),
    group2 = factor(c("X", "X", "Y", "Y", "X", "X", "Y", "Y"))
  )
  result <- calc_stats(data, outcomes = "outcome_num", predictors = list("group1", "group2"),
                        statistics = "mean", multicode = FALSE)

  # An overall "Total" row is always included alongside whatever predictor
  # breakdowns are requested (same source as test 20's Total-only case,
  # bind_rows(total_table, predictor_tables) runs regardless of how many
  # predictor sets are given) — not a bug, just this test's assertion
  # forgetting it the first time round.
  expect_setequal(unique(result$cross_break), c("Total", "group1", "group2"))
})


test_that("22. calc_stats() builds a combined nested breakdown when a predictor set has 2 variables", {
  data <- tibble(
    outcome_num = c(10, 20, 30, 40, 50, 60, 70, 80),
    group1 = factor(c("A", "A", "A", "A", "B", "B", "B", "B")),
    group2 = factor(c("X", "X", "Y", "Y", "X", "X", "Y", "Y"))
  )
  result <- calc_stats(data, outcomes = "outcome_num", predictors = list(c("group1", "group2")),
                        statistics = "mean", multicode = FALSE)

  expect_true("group1_X_group2" %in% result$cross_break)
  expect_true(all(c("predictor1", "predictor2", "p_cat1", "p_cat2") %in% names(result)))
})


test_that("23. calc_stats() rejects invalid inputs with the expected messages", {
  data <- tibble(
    outcome_num = c(1, 2, 3, 4),
    group       = factor(c("A", "A", "B", "B")),
    numeric_var = c(1.1, 2.2, 3.3, 4.4)
  )

  expect_error(
    calc_stats(data, outcomes = "group", predictors = list("group"), statistics = "perc"),
    "Outcome appears in predictor list"
  )
  expect_error(
    calc_stats(data, outcomes = "outcome_num", predictors = list("numeric_var"), statistics = "mean"),
    "Not all predictors are factor variables"
  )
  expect_error(
    calc_stats(data, outcomes = "not_a_real_column", predictors = list("group"), statistics = "mean"),
    "do not exist in the data frame"
  )
  expect_error(
    calc_stats(data, outcomes = "outcome_num", predictors = list(list("group")), statistics = "mean"),
    "Predictor list should not contained further lists"
  )
})


# =============================================================================
# significance tests
# =============================================================================

test_that("24. return_pvalues() dispatches to the chi-square test for an unweighted factor outcome", {
  data <- tibble(
    g = factor(rep(c("A", "B"), each = 10)),
    y = factor(c(rep("Yes", 8), rep("No", 2), rep("Yes", 2), rep("No", 8)))
  )
  result <- return_pvalues(data, outcome = "y", predictor = "g")

  expect_equal(unique(result$p_method), "Chi-Square test")
  expect_true(all(result$p_value >= 0 & result$p_value <= 1))
})


test_that("25. return_pvalues() dispatches to a numeric-outcome test for an unweighted numeric outcome", {
  data <- tibble(
    g = factor(rep(c("A", "B"), each = 6)),
    x = c(1, 2, 3, 4, 5, 6, 20, 21, 22, 23, 24, 25)
  )
  result <- return_pvalues(data, outcome = "x", predictor = "g")

  expect_true(unique(result$p_method) %in% c("Kruskal–Wallis", "Welch's ANOVA"))
  expect_true(all(result$p_value >= 0 & result$p_value <= 1))
})


test_that("26. return_pvalues() runs the weighted tests without error and returns valid p-values", {
  svy_cat <- tibble(
    g = factor(rep(c("A", "B"), each = 6)),
    y = factor(c(rep("Yes", 5), "No", rep("No", 5), "Yes")),
    w = rep(1, 12)
  ) %>% as_survey_design(weight = w)
  cat_result <- return_pvalues(svy_cat, outcome = "y", predictor = "g")

  expect_equal(unique(cat_result$p_method), "Chi-Square test")
  expect_true(all(cat_result$p_value >= 0 & cat_result$p_value <= 1))

  svy_num <- tibble(
    g = factor(rep(c("A", "B"), each = 6)),
    x = c(1, 2, 3, 4, 5, 6, 20, 21, 22, 23, 24, 25),
    w = rep(1, 12)
  ) %>% as_survey_design(weight = w)
  num_result <- return_pvalues(svy_num, outcome = "x", predictor = "g")

  expect_true(unique(num_result$p_method) %in% c("Kruskal–Wallis", "Wald Test"))
  expect_true(all(num_result$p_value >= 0 & num_result$p_value <= 1))
})


# Originally documented behaviour: p-values were only computed for
# single-variable predictors, with nested (multi-variable) predictor sets
# silently skipped. That changed once nested_pvalues() was added (Joe's
# design call: one p-value per outer level, testing the inner variable
# against the outcome within it) - a call mixing a flat set ("group1") and
# a nested one (c("group1","group2")) now gets BOTH kinds of p-value, each
# computed independently by its own branch in calc_stats()'s pval block.
# Kept the mixed-set setup from the original version of this test, since
# it's still a useful check that the two branches don't interfere with each
# other - just checking a different outcome now.

test_that("27. calc_stats() computes p-values for both a single-variable predictor and a nested one in the same call", {
  data <- tibble(
    outcome_num = c(10, 20, 30, 40, 50, 60, 70, 80),
    group1 = factor(c("A", "A", "A", "A", "B", "B", "B", "B")),
    group2 = factor(c("X", "X", "Y", "Y", "X", "X", "Y", "Y"))
  )
  result <- calc_stats(data, outcomes = "outcome_num",
                        predictors = list("group1", c("group1", "group2")),
                        statistics = "mean", pval = TRUE, multicode = FALSE)

  single_var_rows <- result %>% filter(cross_break == "group1")
  nested_rows     <- result %>% filter(cross_break == "group1_X_group2")

  expect_true(all(!is.na(single_var_rows$p_value)))
  expect_true(all(!is.na(nested_rows$p_value)))
  expect_true(all(nested_rows$p_value >= 0 & nested_rows$p_value <= 1))
})


# =============================================================================
# utility functions
# =============================================================================

test_that("28. list_depth() still returns 1 for a flat predictor list, not for a nested one", {
  flat_predictors   <- list(c("g1", "g2"), "g3")
  nested_predictors <- list(list("g1", "g2"), list(c("g3", "g4")))

  expect_equal(list_depth(flat_predictors), 1)
  expect_false(list_depth(nested_predictors) == 1)
})


test_that("29. input validation helpers return the correct TRUE/FALSE, and common_prefix() finds the shared stem", {
  data <- tibble(x = 1:5, g = factor(c("A", "B", "A", "B", "A")))

  expect_true(vars_exist(variable_list = list("x", "g"), data = data))
  expect_false(vars_exist(variable_list = list("x", "not_a_column"), data = data))

  expect_true(all(check_all_factors(data = data, variable_list = list("g"))))
  expect_false(all(check_all_factors(data = data, variable_list = list("x"))))

  expect_true(outcomes_not_in_predictors(outcomes = "x", predictors = list("g")))
  expect_false(outcomes_not_in_predictors(outcomes = "x", predictors = list("x")))

  expect_equal(common_prefix(c("Q1_OptionA", "Q1_OptionB", "Q1_OptionC")), "Q1_Option")
})


# base_information()/create_bases(): "x" has a specific base description,
# "g" falls back to the general one. The exact label text appended after
# ":-" depends on labelled::var_label()'s null_action = "fill" behaviour
# (falls back to the variable name), which isn't this file's logic to
# re-verify — so this checks the dispatch (which base applies to which
# variable), via the part that IS this file's logic, with str_starts()
# rather than an exact string match.

test_that("30. create_bases() picks the specific base for one variable and the general one for another", {
  data <- tibble(x = c(1, 2, 3), g = c(4, 5, 6))
  base_info <- base_information(
    data = data,
    general_base = "Asked of all respondents",
    specific_bases = c(x = "Self-completion questionnaire")
  )

  expect_true(str_starts(create_bases(base_info = base_info, variables = "x"), "Self-completion questionnaire:-"))
  expect_true(str_starts(create_bases(base_info = base_info, variables = "g"), "Asked of all respondents:-"))
})


# =============================================================================
# discrepancy #6 regression test
# =============================================================================

# x = 10,20,30,40,50 with equal weights -> weighted median = 30, weighted
# total = 150. The point isn't the arithmetic (already covered by tests 8/11)
# — it's that x here is a genuine haven_labelled column, which tests 8/11
# don't use. Before the fix, zap_labels() was applied to the string "x"
# (a no-op) instead of x's actual values, leaving the haven_labelled class on
# the column when it reached survey_median()/survey_total().

test_that("31. weighted_median() and weighted_sum() zap_labels() on the outcome's values, not its name (discrepancy #6)", {
  labelled_x <- haven::labelled(c(10, 20, 30, 40, 50), label = "A haven-labelled continuous variable")
  data <- tibble(x = labelled_x, w = c(1, 1, 1, 1, 1))

  expect_true(haven::is.labelled(data$x))   # confirms this test actually exercises a labelled column

  svy <- data %>% as_survey_design(weight = w)

  expect_equal(weighted_median(svy, outcomes = "x")$estimate, 30)
  expect_equal(weighted_sum(svy, outcomes = "x")$estimate, 150)
})


# =============================================================================
# outcome_type: numeric / categorical / multicoded
# =============================================================================

test_that("32. calc_stats() stamps outcome_type = 'numeric' for a numeric outcome", {
  data <- tibble(age = c(20, 25, 30, 35, 40))
  result <- calc_stats(data, outcomes = "age", statistics = "mean", multicode = FALSE)

  expect_equal(unique(result$outcome_type), "numeric")
})

test_that("33. calc_stats() stamps outcome_type = 'categorical' for an ordinary categorical outcome", {
  data <- tibble(gender = factor(c("Male", "Female", "Male", "Female", "Male")))
  result <- calc_stats(data, outcomes = "gender", statistics = "perc", multicode = FALSE)

  expect_equal(unique(result$outcome_type), "categorical")
})

# Q1_OptionA/B/C: a genuine multi-select ("choose all that apply") set,
# built the way convert_multicodes() actually expects to find one - three
# separate Yes/No columns, each labelled "<shared stem>: <own option>", same
# base (no missing data differences between them). Designed so each column
# independently has exactly 2 Yes out of 4 -> 0.5 - the arithmetic isn't the
# point here (that's not new logic), the point is that they collapse into
# one variable's levels and get stamped outcome_type = "multicoded", not
# left as "categorical".
test_that("34. convert_multicodes() stamps outcome_type = 'multicoded' for a real multi-select set", {
  data <- tibble(
    Q1_OptionA = factor(c("Yes", "No",  "Yes", "No")),
    Q1_OptionB = factor(c("No",  "Yes", "No",  "Yes")),
    Q1_OptionC = factor(c("No",  "No",  "Yes", "Yes"))
  )
  attr(data$Q1_OptionA, "label") <- "Q1: Option A"
  attr(data$Q1_OptionB, "label") <- "Q1: Option B"
  attr(data$Q1_OptionC, "label") <- "Q1: Option C"

  result <- calc_stats(data, outcomes = c("Q1_OptionA", "Q1_OptionB", "Q1_OptionC"),
                        statistics = "perc", multicode = TRUE)

  # Collapsed into one variable ("Q1_Option", the common prefix - same
  # helper checked directly in test 29) with one row per original option,
  # not three separate single-code variables.
  expect_equal(nrow(result), 3)
  expect_equal(unique(result$outcome), "Q1_Option")
  expect_equal(unique(result$o_lab), "Q1")
  expect_setequal(result$o_cat, c("Option A", "Option B", "Option C"))

  expect_equal(unique(result$outcome_type), "multicoded")
  expect_true(all(result$estimate == 0.5))
})

# Same multi-select set as test 34, but with a predictor (sex) added, so
# calc_stats() also produces a "Total" cross_break AND a "sex" cross_break
# (Male/Female) for each option, not just one overall row each.
#
# Regression coverage for a fixed typo: convert_multicodes()'s final
# group_by(), just above this test's target code, used to group by
# `contains(c("crossbreak", "p_cat"))` - "crossbreak", no underscore, which
# did NOT match the actual "cross_break" column name (contains() does
# substring matching, and "crossbreak" isn't a substring of "cross_break").
# That silently turned the grouping into just group_by(p_cat1, left_stem)
# instead of group_by(cross_break, p_cat1, left_stem). Fixed in calc_stats.R.
# This test's own numbers wouldn't have caught the old bug (Total and sex
# never share a p_cat1 value, so the missing cross_break grouping was
# harmless here by accident) - it's still worth keeping this test in place
# as a marker that convert_multicodes() correctly groups by predictor set
# now, ahead of leaning on it with more complex predictor structures where
# cross_break grouping would actually matter.
test_that("35. outcome_type stays 'multicoded' when a predictor breakdown is also requested", {
  data <- tibble(
    Q1_OptionA = factor(c("Yes", "No",  "Yes", "No",  "Yes", "No")),
    Q1_OptionB = factor(c("No",  "Yes", "No",  "Yes", "No",  "Yes")),
    Q1_OptionC = factor(c("No",  "No",  "Yes", "Yes", "No",  "Yes")),
    sex        = factor(c("Male", "Male", "Male", "Female", "Female", "Female"))
  )
  attr(data$Q1_OptionA, "label") <- "Q1: Option A"
  attr(data$Q1_OptionB, "label") <- "Q1: Option B"
  attr(data$Q1_OptionC, "label") <- "Q1: Option C"

  result <- calc_stats(data, outcomes = c("Q1_OptionA", "Q1_OptionB", "Q1_OptionC"),
                        predictors = "sex", statistics = "perc", multicode = TRUE)

  # Still collapsed to one variable, across both the Total rows and the
  # Male/Female breakdown rows - 3 options x (Total + Male + Female) = 9 rows.
  expect_equal(nrow(result), 9)
  expect_equal(unique(result$outcome), "Q1_Option")
  expect_equal(unique(result$o_lab), "Q1")
  expect_setequal(result$o_cat, c("Option A", "Option B", "Option C"))
  expect_true(all(result$outcome_type == "multicoded"))
})


# =============================================================================
# min / max / range / iqr / sd (numeric-only additions)
# =============================================================================

# Group A: x = 3, 7   -> min = 3, max = 7, range = 4
# Group B: x = 1, 9, 5 -> min = 1, max = 9, range = 8
# Same dataset reused across tests 36-38 (one function each), same shape as
# the mean/sum grouped tests above.

test_that("36. unweighted_min matches hand-calculated group minimums", {
  data <- tibble(
    x = c(3, 7, 1, 9, 5),
    g = factor(c("A", "A", "B", "B", "B"))
  )
  result <- unweighted_min(data, outcomes = "x", predictors = list("g"))

  expect_equal(result %>% filter(p_cat1 == "A") %>% pull(estimate), 3)
  expect_equal(result %>% filter(p_cat1 == "B") %>% pull(estimate), 1)
})

test_that("37. unweighted_max matches hand-calculated group maximums", {
  data <- tibble(
    x = c(3, 7, 1, 9, 5),
    g = factor(c("A", "A", "B", "B", "B"))
  )
  result <- unweighted_max(data, outcomes = "x", predictors = list("g"))

  expect_equal(result %>% filter(p_cat1 == "A") %>% pull(estimate), 7)
  expect_equal(result %>% filter(p_cat1 == "B") %>% pull(estimate), 9)
})

test_that("38. unweighted_range matches hand-calculated group ranges (max - min)", {
  data <- tibble(
    x = c(3, 7, 1, 9, 5),
    g = factor(c("A", "A", "B", "B", "B"))
  )
  result <- unweighted_range(data, outcomes = "x", predictors = list("g"))

  expect_equal(result %>% filter(p_cat1 == "A") %>% pull(estimate), 4)
  expect_equal(result %>% filter(p_cat1 == "B") %>% pull(estimate), 8)
})


# x = 1..8 -> R's default (type 7) quantile: Q1 sits at position 2.75
# (2 + 0.75*(3-2) = 2.75), Q3 at position 6.25 (6 + 0.25*(7-6) = 6.25)
# -> IQR = 6.25 - 2.75 = 3.5, matching base R's IQR().

test_that("39. unweighted_iqr matches a hand-calculated IQR (base R's type-7 quantile)", {
  data <- tibble(x = c(1, 2, 3, 4, 5, 6, 7, 8))
  result <- unweighted_iqr(data, outcomes = "x")

  expect_equal(result$estimate, 3.5)
})

# survey_quantile()'s interpolation isn't guaranteed to match base R's type-7
# IQR exactly even with equal weights (different default quantile type) - per
# the "point estimates only" scope decision, this checks plausibility (a
# finite positive number, no SE/CI columns produced) rather than an exact
# figure, same treatment test 25/26 give the significance-test p-values.
#
# This test also caught a real bug on its first real run (not something
# hand-tracing found): weighted_iqr() referenced "q25"/"q75" columns that
# survey_quantile() never actually created under those names - it suffixes
# whatever name you give it with "_q<probability*100>" regardless of how
# many quantiles you ask for, so `q25 = survey_quantile(x, 0.25)` produced a
# column called "q25_q25", not "q25". Fixed in calc_stats.R by requesting
# both quantiles from one call under a single stem, whose two output columns
# are then predictably named "quantiles_q25"/"quantiles_q75" by that same
# rule. Left this test as a plausibility check rather than tightening it to
# an exact figure now that it runs cleanly - the underlying uncertainty
# about survey_quantile()'s interpolation matching base R's IQR() hasn't
# changed.

test_that("40. weighted_iqr runs and returns a plausible point estimate, with no SE/CI columns", {
  svy <- tibble(x = c(1, 2, 3, 4, 5, 6, 7, 8), w = rep(1, 8)) %>% as_survey_design(weight = w)
  result <- weighted_iqr(svy, outcomes = "x")

  expect_true(is.numeric(result$estimate))
  expect_true(result$estimate > 0 && is.finite(result$estimate))
  expect_false(any(c("estimate_low", "estimate_upp") %in% names(result)))
})


# x = 2,4,4,4,5,5,7,9 -> mean = 5; deviations -3,-1,-1,-1,0,0,2,4
# sum of squares = 9+1+1+1+0+0+4+16 = 32 -> sample var = 32/7 = 4.571429
# sample sd = sqrt(4.571429) = 2.138090 (R's sd() uses the n-1 divisor)

test_that("41. unweighted_sd matches a hand-calculated sample standard deviation", {
  data <- tibble(x = c(2, 4, 4, 4, 5, 5, 7, 9))
  result <- unweighted_sd(data, outcomes = "x")

  expect_equal(result$estimate, 2.138090, tolerance = 1e-5)
})

# Same x as test 41, weight 1 for every row. With an SRS design and equal
# weights, survey_var() is expected to reduce to the same sample variance
# base R's var() gives, so sqrt(survey_var()) should land on the same 2.138090
# - not independently re-derived here, just checked against test 41's figure.

test_that("42. weighted_sd matches unweighted_sd's figure under equal weights", {
  svy <- tibble(x = c(2, 4, 4, 4, 5, 5, 7, 9), w = rep(1, 8)) %>% as_survey_design(weight = w)
  result <- weighted_sd(svy, outcomes = "x")

  expect_equal(result$estimate, 2.138090, tolerance = 1e-4)
})


# =============================================================================
# calc_stats() dispatch for the new numeric-only stats
# =============================================================================

# Same filtering mechanism as test 19 (str_remove_all-based type filtering),
# now covering the 5 new names: a categorical outcome should lose all of
# them and keep only "perc"; a numeric outcome should keep them and lose
# "perc".

test_that("43. calc_stats() only computes min/max/sd for the numeric outcome, not the categorical one", {
  data <- tibble(
    outcome_num = c(10, 20, 30, 40, 50),
    outcome_cat = factor(c("Yes", "Yes", "No", "No", "Yes")),
    group       = factor(c("A", "A", "B", "B", "A"))
  )
  result <- calc_stats(data, outcomes = c("outcome_num", "outcome_cat"),
                        predictors = "group", statistics = c("min", "max", "sd", "perc"),
                        multicode = FALSE)

  expect_setequal(result %>% filter(outcome == "outcome_num") %>% pull(stat) %>% unique(),
                   c("min", "max", "sd"))
  expect_equal(result %>% filter(outcome == "outcome_cat") %>% pull(stat) %>% unique(), "perc")
})


# Integration check for the stat_labels/pval-trigger extension (not just the
# stat_registry entry): mean and sd both belong to run_assoc_test()'s
# "numeric" stat_labels set now, so they should come from the SAME
# significance test per predictor level and carry an identical, non-NA
# p-value - the same "shared test" pattern already covered for mean+median
# in pivot_crosstab's tests, checked here one layer down at calc_stats()
# directly.
#
# Scoped to cross_break == "group" throughout, same as test 27's
# single_var_rows: calc_stats() always adds a cross_break == "Total" row
# alongside the predictor breakdown (see test 21's comment on bind_rows()
# running regardless of what's requested), and Total has no predictor to
# test against, so its p_value is a genuine, expected NA - not scoping to
# "group" first was this test's own bug on its first real run, not a
# calc_stats() bug.

test_that("44. calc_stats() computes a shared, non-NA p-value for mean and sd together", {
  data <- tibble(
    outcome_num = c(1, 2, 3, 4, 5, 6, 7, 8, 20, 21, 22, 23, 24, 25, 26, 27),
    group       = factor(rep(c("A", "B"), each = 8))
  )
  result <- calc_stats(data, outcomes = "outcome_num", predictors = "group",
                        statistics = c("mean", "sd"), pval = TRUE, multicode = FALSE)
  group_rows <- result %>% filter(cross_break == "group")

  expect_true(all(!is.na(group_rows$p_value)))

  mean_pvals <- group_rows %>% filter(stat == "mean") %>% arrange(p_cat1) %>% pull(p_value)
  sd_pvals   <- group_rows %>% filter(stat == "sd")   %>% arrange(p_cat1) %>% pull(p_value)
  expect_equal(mean_pvals, sd_pvals)
})


# Regression test for a real crash caught by pivot_nested_crosstab.R's own
# test suite (test 9, test_pivot_nested_crosstab.R): requesting pval = TRUE
# when EVERY predictor set is nested (2+ variables) - not just some of them,
# unlike test 27 above - used to leave pvalue_preds (the single-variable
# predictor sets p-values can actually be computed against) completely
# empty, and map_return_p_values()'s map_df(character(0), ...) then
# returned a tibble with zero rows AND zero columns, crashing the
# left_join() just after it ("x and y have no common variables"). Fixed by
# short-circuiting to NA once pvalue_preds is empty (still checked below -
# the Total row has no predictor to test at all, so it stays NA regardless).
#
# This no longer means p_value is NA everywhere though - nested_pvalues()
# (added after this test was first written, once Joe specified the actual
# intended design: one p-value per outer level, testing the inner variable
# against the outcome within it) now genuinely computes one for group1_X_
# group2's rows. Within A: group2 = X gives 10,20, group2 = Y gives 30,40 -
# a real difference, so "within A, is group2 associated with outcome_num"
# should return a real (non-NA) p-value, not hand-calculable exactly with
# only 2 observations per group, so checked structurally.

test_that("45. calc_stats() computes a nested p-value per outer level (and doesn't error) when every predictor set is nested", {
  data <- tibble(
    outcome_num = c(10, 20, 30, 40, 50, 60, 70, 80),
    group1      = factor(c("A", "A", "A", "A", "B", "B", "B", "B")),
    group2      = factor(c("X", "X", "Y", "Y", "X", "X", "Y", "Y"))
  )

  expect_no_error(
    result <- calc_stats(data, outcomes = "outcome_num", predictors = list(c("group1", "group2")),
                          statistics = "mean", pval = TRUE, multicode = FALSE)
  )

  total_row   <- result %>% filter(cross_break == "Total")
  nested_rows <- result %>% filter(cross_break == "group1_X_group2")

  expect_true(is.na(total_row$p_value))
  expect_true(all(!is.na(nested_rows$p_value)))
  expect_true(all(nested_rows$p_value >= 0 & nested_rows$p_value <= 1))

  # Broadcast per outer level - both of A's rows (p_cat1 == "A", one each
  # for p_cat2 == "X"/"Y") share the SAME p-value, since the test is "is
  # group2 associated with outcome_num within A" (one answer, not one per
  # inner level) - and likewise for B, with its own (probably different)
  # p-value.
  a_pvals <- nested_rows %>% filter(p_cat1 == "A") %>% pull(p_value)
  b_pvals <- nested_rows %>% filter(p_cat1 == "B") %>% pull(p_value)
  expect_equal(length(unique(a_pvals)), 1)
  expect_equal(length(unique(b_pvals)), 1)
})
