# =============================================================================
# Tests for make_table.R
#
# I haven't run these myself - same caveat as every other test file in this
# project. Run with:
#
#   testthat::test_file(here::here("Scripts_new", "test_make_table.R"))
#
# make_table() is a pure dispatcher - it doesn't recompute any statistics or
# formatting logic of its own, just decides which pivot_*()/format_*() pair
# to call and calls calc_stats() once on the way there. So these tests don't
# re-verify any arithmetic (that's already covered by test_calc_stats.R/
# test_pivot_summary.R/test_pivot_crosstab.R/test_pivot_nested_crosstab.R) -
# each dispatch test instead checks identical() against calling the same
# calc_stats() -> pivot_*() -> format_*() chain by hand with the exact same
# arguments. If make_table() ever silently drops an argument, calls the
# wrong pivot_*()/format_*() pair, or double-transforms something, identical()
# catches it without needing separately hand-calculated expected values.
#
# Tests 1-3 cover the three successful dispatch paths (summary, crosstab -
# both single- and multi-set, nested crosstab). Tests 4-6 cover the three
# rejected shapes (more than one nested set, a nested set mixed with a flat
# set, a nested set that isn't exactly 2 variables) - see make_table()'s own
# header note for why each of these has no pivot_*() function that could
# render it, rather than being an arbitrary restriction invented here.
#
# Tests 7-9 cover formatted = FALSE, one per dispatch path (summary,
# crosstab, nested crosstab) - same identical() strategy as tests 1-3, just
# checking against the bare pivot_*() output instead of format_*(pivot_*()).
# The point of each is really just "the format_*() step didn't run" - not
# re-testing dispatch itself (tests 1-3 already do that for formatted = TRUE,
# and the dispatch logic runs identically either way, before `formatted` is
# ever consulted).
# =============================================================================

library(testthat)
library(tidyverse)
library(huxtable)
source(here::here("Scripts_new", "calc_stats.R"))
source(here::here("Scripts_new", "pivot_summary.R"))
source(here::here("Scripts_new", "pivot_crosstab.R"))
source(here::here("Scripts_new", "pivot_nested_crosstab.R"))
source(here::here("Scripts_new", "make_table.R"))

test_that("1. make_table() with predictors = NULL dispatches to pivot_summary()/format_summary()", {
  data <- tibble(age = c(10, 20, 30, 40, 50, 60))

  result   <- make_table(data, outcomes = "age", statistics = "mean")
  expected <- format_summary(pivot_summary(calc_stats(data, outcomes = "age", statistics = "mean")))

  expect_s3_class(result, "huxtable")
  expect_identical(result, expected)
})

test_that("2. make_table() with a flat predictor dispatches to pivot_crosstab()/format_crosstab(), single and multiple sets alike", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60),
    sex       = factor(c("Male", "Male", "Male", "Female", "Female", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Old"))
  )

  # Single flat set, predictors passed as a bare character string - the
  # shape calc_stats() itself coerces via `if (is.character(predictors))
  # predictors <- list(predictors)`, so make_table() needs to recognise it
  # BEFORE that coercion happens (it inspects the raw argument, same as
  # calc_stats() would).
  result_single   <- make_table(data, outcomes = "age", predictors = "sex", statistics = "mean")
  expected_single <- format_crosstab(pivot_crosstab(
    calc_stats(data, outcomes = "age", predictors = "sex", statistics = "mean")
  ))
  expect_s3_class(result_single, "huxtable")
  expect_identical(result_single, expected_single)

  # Multiple flat sets side by side - predictors = list("sex", "age_group"),
  # every element length 1, so n_nested == 0 and this still routes to
  # pivot_crosstab() (which already handles more than one set on its own).
  result_multi   <- make_table(data, outcomes = "age", predictors = list("sex", "age_group"),
                                statistics = "mean")
  expected_multi <- format_crosstab(pivot_crosstab(
    calc_stats(data, outcomes = "age", predictors = list("sex", "age_group"), statistics = "mean")
  ))
  expect_s3_class(result_multi, "huxtable")
  expect_identical(result_multi, expected_multi)
})

test_that("3. make_table() with a nested predictor (list(c(outer, inner))) dispatches to pivot_nested_crosstab()/format_nested_crosstab()", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60, 70, 80),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )

  result <- make_table(data, outcomes = "age", predictors = list(c("sex", "age_group")),
                        statistics = "mean")
  expected <- format_nested_crosstab(pivot_nested_crosstab(
    calc_stats(data, outcomes = "age", predictors = list(c("sex", "age_group")), statistics = "mean")
  ))

  expect_s3_class(result, "huxtable")
  expect_identical(result, expected)
})

test_that("4. make_table() errors on more than one nested predictor set", {
  data <- tibble(
    age = 1:8,
    a   = factor(rep(c("A1", "A2"), 4)),
    b   = factor(rep(c("B1", "B2"), each = 4)),
    c   = factor(rep(c("C1", "C2"), times = c(3, 5))),
    d   = factor(rep(c("D1", "D2"), times = c(5, 3)))
  )

  expect_error(
    make_table(data, outcomes = "age",
               predictors = list(c("a", "b"), c("c", "d")), statistics = "mean"),
    "at most one nested predictor set"
  )
})

test_that("5. make_table() errors when a nested set is combined with an additional flat set", {
  data <- tibble(
    age = 1:8,
    sex       = factor(rep(c("Male", "Female"), 4)),
    age_group = factor(rep(c("Young", "Old"), each = 4)),
    region    = factor(rep(c("North", "South"), times = c(3, 5)))
  )

  expect_error(
    make_table(data, outcomes = "age",
               predictors = list(c("sex", "age_group"), "region"), statistics = "mean"),
    "can't combine a nested predictor set"
  )
})

test_that("6. make_table() errors when a nested set doesn't have exactly 2 variables", {
  data <- tibble(
    age = 1:8,
    a = factor(rep(c("A1", "A2"), 4)),
    b = factor(rep(c("B1", "B2"), each = 4)),
    c = factor(rep(c("C1", "C2"), times = c(3, 5)))
  )

  expect_error(
    make_table(data, outcomes = "age", predictors = list(c("a", "b", "c")), statistics = "mean"),
    "exactly 2 variables"
  )
})

test_that("7. make_table(formatted = FALSE) with predictors = NULL returns pivot_summary()'s own output, untouched", {
  data <- tibble(age = c(10, 20, 30, 40, 50, 60))

  result   <- make_table(data, outcomes = "age", statistics = "mean", formatted = FALSE)
  expected <- pivot_summary(calc_stats(data, outcomes = "age", statistics = "mean"))

  # A list, not a huxtable - format_summary() never ran.
  expect_type(result, "list")
  expect_false(inherits(result, "huxtable"))
  expect_identical(result, expected)
})

test_that("8. make_table(formatted = FALSE) with a flat predictor returns pivot_crosstab()'s own output, untouched", {
  data <- tibble(
    age = c(10, 20, 30, 40, 50, 60),
    sex = factor(c("Male", "Male", "Male", "Female", "Female", "Female"))
  )

  result   <- make_table(data, outcomes = "age", predictors = "sex", statistics = "mean",
                          formatted = FALSE)
  expected <- pivot_crosstab(calc_stats(data, outcomes = "age", predictors = "sex", statistics = "mean"))

  expect_type(result, "list")
  expect_false(inherits(result, "huxtable"))
  expect_identical(result, expected)
})

test_that("9. make_table(formatted = FALSE) with a nested predictor returns pivot_nested_crosstab()'s own output, untouched", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60, 70, 80),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )

  result <- make_table(data, outcomes = "age", predictors = list(c("sex", "age_group")),
                        statistics = "mean", formatted = FALSE)
  expected <- pivot_nested_crosstab(
    calc_stats(data, outcomes = "age", predictors = list(c("sex", "age_group")), statistics = "mean")
  )

  expect_type(result, "list")
  expect_false(inherits(result, "huxtable"))
  expect_identical(result, expected)
})
