# =============================================================================
# Tests for pivot_numeric_summary.R
# Same caveat as the other test files in this project: I haven't run these
# myself (no R in this environment). Run with:
#   testthat::test_file("tests/testthat/test-pivot_numeric_summary.R"), or devtools::test()
# Tests 1-3 cover the three validation errors (categorical outcome,
# categorical-only statistic, weighted = TRUE without a survey object).
# Test 4 is the main hand-calculated case: two numeric variables with
# DIFFERENT bases (one has missing data the other doesn't), confirming Base
# is computed per-variable, not shared across the whole table. Test 5 covers
# weighted = TRUE - a hand-calculated weighted mean alongside min, which has
# no weighted equivalent and should silently stay unweighted even though
# weighted = TRUE was requested (per the "you get one or the other, except
# min/max/range which only have one version" design). Test 6 checks column
# order follows the `statistics` argument's own order, not alphabetical or
# registry order - genuinely not obvious it would, since pivot_wider()'s
# column order depends on row first-appearance order in calc_stats()'s
# output. Test 7 covers format_numeric_summary()'s digit formatting and Base
# rendering.
# =============================================================================


test_that("1. pivot_numeric_summary() rejects a categorical outcome", {
  data <- tibble(age = c(1, 2, 3), gender = factor(c("M", "F", "M")))

  expect_error(
    pivot_numeric_summary(data, outcomes = c("age", "gender"), statistics = "mean"),
    "must be numeric"
  )
})


test_that("2. pivot_numeric_summary() rejects a categorical-only statistic", {
  data <- tibble(age = c(1, 2, 3))

  expect_error(
    pivot_numeric_summary(data, outcomes = "age", statistics = c("mean", "perc")),
    "statistics must be"
  )
})


test_that("3. pivot_numeric_summary() rejects weighted = TRUE without a survey design object", {
  data <- tibble(age = c(1, 2, 3))

  expect_error(
    pivot_numeric_summary(data, outcomes = "age", statistics = "mean", weighted = TRUE),
    "survey design object"
  )
})


# age = 10,20,30,40,50,60 (no missing) -> mean = 35
#   deviations -25,-15,-5,5,15,25 -> sum of squares = 1750 -> var = 1750/5 = 350
#   sd = sqrt(350) = 18.708287
#   min = 10, max = 60
# income = 100,200,300,NA,NA,NA (half missing) -> mean = 200
#   deviations -100,0,100 -> sum of squares = 20000 -> var = 20000/2 = 10000
#   sd = 100, min = 100, max = 300
# Base: age's complete cases = 6, income's = 3 - deliberately different, to
# confirm Base is computed per-variable (via calc_stat_engine()'s per-outcome
# `filtered`) rather than accidentally shared/overwritten across variables.

test_that("4. pivot_numeric_summary() matches hand-calculated stats and per-variable bases", {
  data <- tibble(
    age    = c(10, 20, 30, 40, 50, 60),
    income = c(100, 200, 300, NA, NA, NA)
  )
  result <- pivot_numeric_summary(data, outcomes = c("age", "income"),
                                   statistics = c("mean", "sd", "min", "max"),
                                   weighted = FALSE)[[1]]

  expect_equal(nrow(result), 2)

  age_row    <- result %>% filter(Variable == "age")
  income_row <- result %>% filter(Variable == "income")

  expect_equal(age_row$Mean, 35)
  expect_equal(age_row$SD, 18.708287, tolerance = 1e-5)
  expect_equal(age_row$Min, 10)
  expect_equal(age_row$Max, 60)
  expect_equal(age_row$Base, 6)

  expect_equal(income_row$Mean, 200)
  expect_equal(income_row$SD, 100)
  expect_equal(income_row$Min, 100)
  expect_equal(income_row$Max, 300)
  expect_equal(income_row$Base, 3)
})


# x = 10,20,30,40 with weights 1,1,2,2 -> weighted mean = (10+20+60+80)/6 = 28.333333
# (same weighting shape as test_calc_stats.R's test 4/5) - min is requested
# alongside it, and should stay the plain unweighted minimum (10) even though
# weighted = TRUE, since min has no weighted equivalent to switch to.

test_that("5. pivot_numeric_summary(weighted = TRUE) requests w_mean but keeps min unweighted", {
  svy <- tibble(x = c(10, 20, 30, 40), w = c(1, 1, 2, 2)) %>% as_survey_design(weight = w)
  result <- pivot_numeric_summary(svy, outcomes = "x", statistics = c("mean", "min"),
                                   weighted = TRUE)[[1]]

  # "Mean (w)" not "Mean" - tidy_statistic_description() labels w_mean that
  # way (see pivot_summary.R's `labels` vector), and pivot_wider() uses
  # whatever tidy_statistic_description() produced as its column names.
  expect_true("Mean (w)" %in% names(result))
  expect_true("Min" %in% names(result))
  expect_false("Min (w)" %in% names(result))   # min has no weighted form to label that way

  expect_equal(result[["Mean (w)"]][1], 170 / 6, tolerance = 1e-6)
  expect_equal(result[["Min"]][1], 10)
  expect_equal(result$Base[1], 4)
})


test_that("6. pivot_numeric_summary() orders columns by the requested statistics order, not alphabetically", {
  data <- tibble(x = c(1, 2, 3, 4, 5))
  result <- pivot_numeric_summary(data, outcomes = "x", statistics = c("sd", "mean", "min"))[[1]]

  # Alphabetical would be Mean, Min, SD - this checks the actual request
  # order (sd, mean, min) survived through calc_stats() and pivot_wider().
  expect_equal(names(result), c("Variable", "SD", "Mean", "Min", "Base"))
})


# age = 10..60 as in test 4: Mean = 35.0, Min = 10.0, Max = 60.0 (all stats
# format to 1dp per format_statistic()'s rule for these stat codes), Base as
# a plain unformatted count ("6", no decimal).

test_that("7. format_numeric_summary() formats each column to 1dp and Base as a plain count", {
  data <- tibble(age = c(10, 20, 30, 40, 50, 60))
  pivoted <- pivot_numeric_summary(data, outcomes = "age", statistics = c("mean", "min", "max"))
  result <- format_numeric_summary(pivoted)

  # Row 1 is the header text itself (as_hux(add_colnames = TRUE) puts the
  # column names into the huxtable's own row 1, not just a display header) -
  # result[["Mean"]][1] is literally the string "Mean", not a data value.
  # The one real data row (age) is row 2 - same +1 offset the other two
  # format_*() files' tests already account for elsewhere, missed here on
  # this test's first pass.
  expect_equal(result[["Mean"]][2], "35.0")
  expect_equal(result[["Min"]][2], "10.0")
  expect_equal(result[["Max"]][2], "60.0")
  expect_equal(result[["Base"]][2], "6")

  variable_col <- which(names(result) == "Variable")
  mean_col     <- which(names(result) == "Mean")
  expect_equal(huxtable::align(result)[2, variable_col], "left")
  expect_equal(huxtable::align(result)[2, mean_col], "right")
})
