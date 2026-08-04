# =============================================================================
# KNOWN-ANSWER TESTS for simplified_functions.R
#
# I have NOT run these myself — I don't have R available in this environment
# (no root/sudo access to install it), so these are unverified. Run them on
# your machine with:
#
#   testthat::test_file(here::here("Scripts", "test_simplified_functions.R"))
#
# Where hand-calculation is practical, each test builds a tiny synthetic
# dataset small enough that the "correct" answer is worked out by hand in a
# comment above it, rather than trusting either the old or new code. Where
# hand-calculation isn't practical (bootstrap CIs, chi-square/Kruskal-Wallis
# p-values), the test checks structure and plausibility instead (right method
# chosen, p in [0,1], NA where NA is expected) rather than an exact number.
#
# 30 tests, organised by what they cover:
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
# =============================================================================
# file.create(here::here(".here"))
# setwd('/Users/joecrowley/5 - R/Descriptives')
#
library(testthat)
source(here::here("Scripts", "simplified_functions.R"))


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


# Documented behaviour: p-values are only computed for single-variable
# predictors; nested (multi-variable) predictor sets are silently skipped.
# Mixing a single-variable and a nested predictor set in one call lets this
# be checked precisely, rather than relying on what happens when
# map_return_p_values() is given nothing to do at all.

test_that("27. calc_stats() computes p-values for single-variable predictors but skips nested ones", {
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
  expect_true(all(is.na(nested_rows$p_value)))
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
