# =============================================================================
# Tests for make_numeric_summary_table.R
#
# I haven't run these myself - same caveat as every other test file in this
# project. Run with:
#
#   testthat::test_file(here::here("Scripts_new", "test_make_numeric_summary_table.R"))
#
# Same identical()-against-the-hand-chained-call strategy as
# test_make_table.R, and for the same reason: make_numeric_summary_table()
# doesn't recompute any statistics or formatting logic of its own, it's just
# pivot_numeric_summary() -> format_numeric_summary() (or, if
# formatted = FALSE, just pivot_numeric_summary() on its own) - so there's
# nothing here worth re-verifying with hand-calculated values that
# test_pivot_numeric_summary.R doesn't already cover.
#
# Test 1 covers the default (formatted = TRUE) path. Test 2 covers
# formatted = FALSE. Test 3 covers weighted = TRUE actually reaching
# pivot_numeric_summary() unchanged - a plain TRUE/FALSE toggle here, unlike
# anything make_table() passes through, so worth its own explicit check
# rather than assuming it rides along with the others. Uses equal weights
# (w = 1 for every row) deliberately - the point is confirming weighted = TRUE
# is honoured at all (requests w_mean/w_sd, not mean/sd, and requires a
# survey design object - see pivot_numeric_summary()'s own validation), not
# re-deriving weighted arithmetic, which is already covered elsewhere
# (test_calc_stats.R). Equal weights make weighted and unweighted estimates
# numerically identical, so identical() still isolates exactly the thing
# this test is checking - that the right calc_stats() call happened - without
# needing to hand-calculate a genuinely different weighted result.
# =============================================================================

library(testthat)
library(tidyverse)
library(srvyr)
library(huxtable)
source(here::here("Scripts_new", "calc_stats.R"))
source(here::here("Scripts_new", "pivot_summary.R"))
source(here::here("Scripts_new", "pivot_numeric_summary.R"))
source(here::here("Scripts_new", "make_numeric_summary_table.R"))

test_that("1. make_numeric_summary_table() dispatches to pivot_numeric_summary()/format_numeric_summary()", {
  data <- tibble(
    age    = c(10, 20, 30, 40, 50, 60),
    income = c(100, 200, 300, 400, 500, 600)
  )

  result   <- make_numeric_summary_table(data, outcomes = c("age", "income"),
                                          statistics = c("mean", "sd"))
  expected <- format_numeric_summary(
    pivot_numeric_summary(data, outcomes = c("age", "income"), statistics = c("mean", "sd"))
  )

  expect_s3_class(result, "huxtable")
  expect_identical(result, expected)
})

test_that("2. make_numeric_summary_table(formatted = FALSE) returns pivot_numeric_summary()'s own output, untouched", {
  data <- tibble(
    age    = c(10, 20, 30, 40, 50, 60),
    income = c(100, 200, 300, 400, 500, 600)
  )

  result   <- make_numeric_summary_table(data, outcomes = c("age", "income"),
                                          statistics = c("mean", "sd"), formatted = FALSE)
  expected <- pivot_numeric_summary(data, outcomes = c("age", "income"), statistics = c("mean", "sd"))

  expect_type(result, "list")
  expect_false(inherits(result, "huxtable"))
  expect_identical(result, expected)
})

test_that("3. make_numeric_summary_table(weighted = TRUE) passes weighted through to pivot_numeric_summary() unchanged", {
  # Equal weights (w = 1 throughout) - see header note on why this isolates
  # "was weighted = TRUE honoured at all" from "is the weighted arithmetic
  # correct" (already covered elsewhere).
  data <- tibble(
    age = c(10, 20, 30, 40, 50, 60),
    w   = rep(1, 6)
  ) %>% as_survey_design(weight = w)

  result   <- make_numeric_summary_table(data, outcomes = "age", statistics = "mean",
                                          weighted = TRUE)
  expected <- format_numeric_summary(
    pivot_numeric_summary(data, outcomes = "age", statistics = "mean", weighted = TRUE)
  )

  expect_s3_class(result, "huxtable")
  expect_identical(result, expected)
})
