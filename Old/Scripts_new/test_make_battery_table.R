# =============================================================================
# Tests for make_battery_table.R
#
# I haven't run these myself - same caveat as every other test file in this
# project. Run with:
#
#   testthat::test_file(here::here("Scripts_new", "test_make_battery_table.R"))
#
# Same identical()-against-the-hand-chained-call strategy as
# test_make_table.R/test_make_numeric_summary_table.R, and for the same
# reason: make_battery_table() doesn't recompute any statistics or
# formatting logic of its own, it's just calc_stats() -> pivot_battery() ->
# format_battery() (or, if formatted = FALSE, stopping after pivot_battery())
# - so there's nothing here worth re-verifying with hand-calculated values
# that test_pivot_battery.R doesn't already cover.
#
# Test 1 covers the default (formatted = TRUE) path. Test 2 covers
# formatted = FALSE. Test 3 covers conf = "se" actually reaching
# pivot_battery()/format_battery() and producing a real inserted SE row -
# worth its own explicit check since this is the one argument
# make_numeric_summary_table() (the closest sibling wrapper) does NOT have
# at all (pivot_numeric_summary() never supported SE/CI), so it's the one
# genuinely new thing about this wrapper that's worth confirming end to end,
# not just assumed to ride along.
# =============================================================================

library(testthat)
library(tidyverse)
source(here::here("Scripts_new", "calc_stats.R"))
source(here::here("Scripts_new", "pivot_summary.R"))
source(here::here("Scripts_new", "pivot_battery.R"))
source(here::here("Scripts_new", "make_battery_table.R"))


test_that("1. make_battery_table() dispatches to calc_stats() -> pivot_battery() -> format_battery()", {
  data <- tibble(
    item1 = factor(c("Agree", "Agree", "Neutral", "Disagree"),
                    levels = c("Disagree", "Neutral", "Agree")),
    item2 = factor(c("Agree", "Agree", "Agree", NA),
                    levels = c("Disagree", "Neutral", "Agree"))
  )

  result <- make_battery_table(data, outcomes = c("item1", "item2"), statistics = "perc")

  expected_stats_table <- calc_stats(data, outcomes = c("item1", "item2"), predictors = NULL,
                                      statistics = "perc", conf = NULL, base = NULL,
                                      multicode = FALSE)
  expected <- format_battery(pivot_battery(expected_stats_table))

  expect_s3_class(result, "huxtable")
  expect_identical(result, expected)
})


test_that("2. make_battery_table(formatted = FALSE) returns pivot_battery()'s own list, untouched", {
  data <- tibble(
    item1 = factor(c("Agree", "Agree", "Neutral", "Disagree"),
                    levels = c("Disagree", "Neutral", "Agree"))
  )

  result <- make_battery_table(data, outcomes = "item1", statistics = "perc", formatted = FALSE)

  expected_stats_table <- calc_stats(data, outcomes = "item1", predictors = NULL,
                                      statistics = "perc", conf = NULL, base = NULL,
                                      multicode = FALSE)
  expected <- pivot_battery(expected_stats_table)

  expect_type(result, "list")
  expect_false(inherits(result, "huxtable"))
  expect_identical(result, expected)

  # 4 elements now (pivoted, stat_code, category_order, conf_type) - see
  # pivot_battery()'s own header note on the SE/CI addition. conf wasn't
  # requested here, so conf_type should be NA, not absent - the list is
  # always length 4 regardless of whether conf was used.
  expect_equal(length(result), 4)
  expect_true(is.na(result[[4]]))
})


test_that("3. make_battery_table(conf = \"se\") reaches pivot_battery()/format_battery() and produces a real inserted SE row", {
  data <- tibble(
    item1 = factor(c("Agree", "Agree", "Neutral", "Disagree"),
                    levels = c("Disagree", "Neutral", "Agree")),
    item2 = factor(c("Agree", "Agree", "Agree", NA),
                    levels = c("Disagree", "Neutral", "Agree"))
  )

  result <- make_battery_table(data, outcomes = c("item1", "item2"),
                                statistics = "perc", conf = "se")

  expected_stats_table <- calc_stats(data, outcomes = c("item1", "item2"), predictors = NULL,
                                      statistics = "perc", conf = "se", base = NULL,
                                      multicode = FALSE)
  expected <- format_battery(pivot_battery(expected_stats_table))

  expect_s3_class(result, "huxtable")
  expect_identical(result, expected)

  # Not just identical() to the hand-chained call - confirms conf = "se"
  # genuinely produced a real SE row through the wrapper, not just that the
  # wrapper mechanically reproduces whatever pivot_battery()/format_battery()
  # would have done regardless of conf.
  expect_true("SE" %in% result[[2]])
})
