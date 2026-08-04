# =============================================================================
# Tests for pivot_summary.R (pivot_summary() / format_summary() and their
# helpers)
#
# I haven't run these myself - same caveat as the other test files in this
# project. Run with:
#
#   testthat::test_file(here::here("Scripts_new", "test_pivot_summary.R"))
#
# Tests 1, 2, 4, 5, 8, 9, 10, 13, 14 call calc_stats() for real (genuine
# integration coverage of the pipeline you were testing by hand). Tests 3, 6,
# 7 use hand-built calc_stats()-shaped input instead, to isolate
# pivot_summary()'s own column handling from calc_stats() - in particular to
# construct the conf = "se"/"ci" column shapes directly without needing a
# working survey design object.
#
# Tests 10, 13, 14 cover format_summary() end to end and are the ones in this
# file that touch a real huxtable object - I'm less confident in the exact
# indexing/rendering behaviour there than everywhere else (flagged inline at
# the na_string print check in test 10). Test 13 extends 10's single
# numeric + single categorical case to two numeric variables plus one
# categorical, and test 14 checks that a variable shown under two statistics
# gets its repeated Variable text rowspan-merged back down to one cell.
# Tests 11-12 cover format_statistic()/format_ci_string() directly instead,
# which is exactly why those were pulled out as their own functions - plain
# R, known input/output pairs, nothing huxtable-specific to get wrong.
# =============================================================================

library(testthat)
library(tidyverse)
library(huxtable)
source(here::here("Scripts_new", "calc_stats.R"))
source(here::here("Scripts_new", "pivot_summary.R"))

test_that("1. a numeric-only variable gets one row, no label row, label directly on it", {
  data <- tibble(age = c(20, 25, 30, 35, 40))
  stats_table <- calc_stats(data, outcomes = "age", statistics = "mean", multicode = FALSE)
  result <- pivot_summary(stats_table)[[1]]

  expect_equal(nrow(result), 1)
  expect_equal(result$Variable, "age")
  expect_equal(result$row_type, "data")
})

test_that("2. a categorical variable gets a label row above its levels, blanked correctly", {
  data <- tibble(gender = factor(c("Male", "Female", "Male", "Female", "Male")))
  stats_table <- calc_stats(data, outcomes = "gender", statistics = "perc", multicode = FALSE)
  result <- pivot_summary(stats_table)[[1]]

  expect_equal(nrow(result), 3)   # 1 label row + 2 levels
  label_row <- result %>% filter(row_type == "label")
  level_rows <- result %>% filter(row_type == "data")

  expect_equal(label_row$Variable, "gender")
  expect_true(is.na(label_row$Estimate))
  expect_true(is.na(label_row$Base))
  expect_setequal(level_rows$Variable, c("Male", "Female"))
  expect_false(any(is.na(level_rows$Estimate)))
})

test_that("3. base_description is not carried through as a per-row column", {
  data <- tibble(age = c(20, 25, 30, 35, 40))
  stats_table <- calc_stats(data, outcomes = "age", statistics = "mean", multicode = FALSE)
  result <- pivot_summary(stats_table)[[1]]

  expect_false("base_description" %in% names(result))
})

test_that("4. only the Total row is kept when predictors were requested", {
  data <- tibble(
    age   = c(10, 20, 30, 40, 50, 60),
    group = factor(c("A", "A", "B", "B", "A", "B"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = "group",
                             statistics = "mean", multicode = FALSE)
  result <- pivot_summary(stats_table)[[1]]

  expect_equal(nrow(result), 1)   # the two "group" breakdown rows are dropped
  expect_equal(result$Estimate, 35)
})

test_that("5. Statistics uses short statistic labels end to end", {
  data <- tibble(
    age    = c(20, 25, 30, 35, 40),
    gender = factor(c("Male", "Female", "Male", "Female", "Male"))
  )
  stats_table <- calc_stats(data, outcomes = c("age", "gender"),
                             statistics = c("mean", "perc"), multicode = FALSE)
  result <- pivot_summary(stats_table)[[1]]

  expect_true("Statistics" %in% names(result))
  expect_true(all(na.omit(result$Statistics) %in% c("Mean", "%")))
})

test_that("6. the SE placeholder ('-') is dropped, but a real SE column is kept", {
  base_row <- tibble(
    cross_break = "Total", outcome = "age", o_lab = "Age", o_cat = "mean",
    stat = "mean", outcome_type = "numeric", estimate = 45, base = 100,
    base_description = NA_character_
  )

  no_conf   <- base_row %>% mutate(estimate_se = "-")
  result_no_conf <- pivot_summary(no_conf)[[1]]
  expect_false("SE" %in% names(result_no_conf))

  with_se   <- base_row %>% mutate(estimate_se = 2.1)
  result_se <- pivot_summary(with_se)[[1]]
  expect_true("SE" %in% names(result_se))
  expect_equal(result_se$SE, 2.1)
})

test_that("7. a 95% CI column is built and renamed correctly", {
  ci_stats <- tibble(
    cross_break = "Total", outcome = "age", o_lab = "Age", o_cat = "mean",
    stat = "mean", outcome_type = "numeric", estimate = 45, estimate_low = 40,
    estimate_upp = 50, estimate_ci = "40 - 50", base = 100, base_description = NA_character_
  )
  result <- pivot_summary(ci_stats)[[1]]

  expect_true("95% CI" %in% names(result))
  expect_equal(result[["95% CI"]], "40 - 50")
})

test_that("8. a variable shown with two statistics gets a label row per statistic", {
  data <- tibble(x = c(10, 20, 30, 40, 50))
  stats_table <- calc_stats(data, outcomes = "x", statistics = c("mean", "median"), multicode = FALSE)
  result <- pivot_summary(stats_table)[[1]]

  expect_equal(sum(result$Variable == "x"), 2)
})

test_that("9. a more realistic table: two numeric variables (two statistics each) + one categorical variable", {

  # age/income: mean and median hand-calculable (n = 6, so median is the
  # average of the 3rd/4th sorted values). region: 3 North, 2 South, 1 East.
  data <- tibble(
    age    = c(10, 20, 30, 40, 50, 100),
    income = c(1000, 2000, 3000, 4000, 5000, 100000),
    region = factor(c("North", "South", "North", "East", "South", "North"))
  )

  stats_table <- calc_stats(data, outcomes = c("age", "income", "region"),
                             statistics = c("mean", "median", "perc"), multicode = FALSE)
  result <- pivot_summary(stats_table)[[1]]

  # 2 numeric vars x 2 stats each (no label row) + 1 categorical var
  # (1 label row + 3 levels) = 2 + 2 + 4
  expect_equal(nrow(result), 8)

  # calc_stats() loops outcomes as the outer loop and statistics as the inner
  # loop, so each outcome's rows stay together in outcomes order: age, then
  # income, then region. fct_inorder() inside pivot_summary() is what
  # preserves this - group_split() on a plain column would otherwise
  # re-sort blocks alphabetically (which would put age, income, region in
  # the same order here by coincidence, but not in general).
  expect_equal(match("age", result$Variable), 1)
  expect_true(match("income", result$Variable) > max(which(result$Variable == "age")))

  # age and income: no label row, one row per statistic, correct values
  age_rows <- result %>% filter(Variable == "age")
  expect_equal(nrow(age_rows), 2)
  expect_false(any(age_rows$row_type == "label"))
  expect_equal(age_rows$Estimate[age_rows$stat_code == "mean"],   250 / 6)
  expect_equal(age_rows$Estimate[age_rows$stat_code == "median"], 35)

  income_rows <- result %>% filter(Variable == "income")
  expect_equal(nrow(income_rows), 2)
  expect_equal(income_rows$Estimate[income_rows$stat_code == "mean"],   115000 / 6)
  expect_equal(income_rows$Estimate[income_rows$stat_code == "median"], 3500)

  # region: exactly one label row, blanked, above its three levels
  region_label <- result %>% filter(row_type == "label")
  expect_equal(nrow(region_label), 1)
  expect_equal(region_label$Variable, "region")
  expect_true(is.na(region_label$Estimate))
  expect_true(is.na(region_label$Base))

  region_levels <- result %>% filter(Variable %in% c("North", "South", "East"))
  expect_equal(nrow(region_levels), 3)
  expect_equal(region_levels$Estimate[region_levels$Variable == "North"], 0.5)
  expect_equal(region_levels$Estimate[region_levels$Variable == "South"], 2 / 6)
  expect_equal(region_levels$Estimate[region_levels$Variable == "East"],  1 / 6)

  # base (unweighted N) is 6 everywhere it isn't blanked
  expect_true(all(result$Base[result$row_type == "data"] == 6))

  # no leftover base_description column, and stat_code carries the raw codes
  expect_false("base_description" %in% names(result))
  expect_setequal(unique(na.omit(result$stat_code)), c("mean", "median", "perc"))

  # Statistics is always this column name, always the short form
  expect_true("Statistics" %in% names(result))
  expect_setequal(na.omit(result$Statistics), c("Mean", "Median", "%"))
})

test_that("10. format_summary() produces a correctly formatted huxtable for a semi-realistic mixed table", {

  data <- tibble(
    age    = c(10, 20, 30, 40, 50, 100),
    region = factor(c("North", "South", "North", "East", "South", "North"))
  )
  stats_table <- calc_stats(data, outcomes = c("age", "region"),
                             statistics = c("mean", "perc"), multicode = FALSE)
  result <- format_summary(pivot_summary(stats_table))

  expect_s3_class(result, "huxtable")
  expect_setequal(names(result), c("Variable", "Statistics", "Estimate", "Base"))
  expect_equal(nrow(result), 6)   # 1 header row + age + region label + 3 levels

  # Underlying stored values - [[ extraction on a huxtable gives back what's
  # actually stored, not the printed/na_string-substituted text, so these are
  # safe to check directly regardless of how na_string rendering works.
  age_row    <- which(result[["Variable"]] == "age")
  north_row  <- which(result[["Variable"]] == "North")
  south_row  <- which(result[["Variable"]] == "South")
  east_row   <- which(result[["Variable"]] == "East")
  region_row <- which(result[["Variable"]] == "region")

  expect_equal(result[["Estimate"]][age_row],   "41.7")
  expect_equal(result[["Estimate"]][north_row], "50")   # no "%" sign, rounded to 0dp
  expect_equal(result[["Estimate"]][south_row], "33")   # 33.333 -> 33
  expect_equal(result[["Estimate"]][east_row],  "17")   # 16.667 -> 17
  expect_true(is.na(result[["Estimate"]][region_row]))   # label row - structural, not missing data

  expect_equal(result[["Base"]][age_row],   "6")
  expect_equal(result[["Base"]][north_row], "6")

  # Alignment: Variable left, Estimate right, checked on a real body row.
  # align() is a huxtable accessor matrix, same access pattern as rowspan()
  # used in the merge_rows tests in test_summary_table.R.
  variable_col <- which(names(result) == "Variable")
  estimate_col <- which(names(result) == "Estimate")
  expect_equal(huxtable::align(result)[age_row, variable_col], "left")
  expect_equal(huxtable::align(result)[age_row, estimate_col], "right")

  # Bold: the variable name text ("age", "region") is bold; the category
  # level text underneath region ("North" etc) is not - checked via
  # huxtable's bold() accessor, same access pattern as align()/rowspan().
  expect_true(huxtable::bold(result)[age_row, variable_col])
  expect_true(huxtable::bold(result)[region_row, variable_col])
  expect_false(huxtable::bold(result)[north_row, variable_col])
  expect_false(huxtable::bold(result)[south_row, variable_col])
  expect_false(huxtable::bold(result)[east_row, variable_col])

  # Top padding: extra padding (6) marks the start of each variable's block -
  # age's own row and region's label row - baseline padding (1, from
  # set_all_padding(1)) everywhere else, including region's level rows.
  expect_equal(huxtable::top_padding(result)[age_row, variable_col], 6)
  expect_equal(huxtable::top_padding(result)[region_row, variable_col], 6)
  expect_equal(huxtable::top_padding(result)[north_row, variable_col], 1)
  expect_equal(huxtable::top_padding(result)[south_row, variable_col], 1)
  expect_equal(huxtable::top_padding(result)[east_row, variable_col], 1)

  # Rendered text: the region label row should print blank, not "NA" -
  # this depends on huxtable's na_string substitution at print time, not
  # just the stored value, so it's the one check here I'm least confident
  # in without being able to run it myself.
  printed_lines <- str_split(huxtable::to_screen(result), "\n")[[1]]
  region_label_line <- printed_lines[str_detect(printed_lines, "region")]
  expect_false(any(str_detect(region_label_line, "\\bNA\\b")))

  # Stripe: plain alternating rows - each row's colour should differ from
  # the row directly below it, and match the row two below it (checked as a
  # pattern rather than against hardcoded colours/positions, since it isn't
  # certain from the docs alone whether stripe_rows() counts the header row
  # as row 1 or starts from the first body row - this pattern holds either
  # way). Checked directly via background_color() rather than by eye, since
  # #f5f7fa vs #ffffff is a genuinely subtle difference that a screenshot or
  # a quick look can easily fail to show either way.
  bg <- huxtable::background_color(result)
  body_colours <- unname(bg[2:nrow(result), 1])
  expect_true(all(body_colours[-length(body_colours)] != body_colours[-1]))
  expect_equal(body_colours[1], body_colours[3])
})

test_that("11. format_statistic() formats each statistic type correctly and preserves NA", {
  expect_equal(format_statistic(0.5,     "perc"),   "50")    # no "%" sign, 0dp
  expect_equal(format_statistic(0.16667, "w_perc"), "17")    # 16.667 -> 17
  expect_equal(format_statistic(1234,    "count"),  "1,234")
  expect_equal(format_statistic(41.6667, "mean"),   "41.7")
  expect_equal(format_statistic(35,      "median"), "35.0")
  expect_equal(format_statistic(115000,  "w_sum"),  "115,000.0")
  expect_true(is.na(format_statistic(NA_real_, "mean")))
})

test_that("12. format_ci_string() reformats both CI bounds to match Estimate's precision, preserving NA", {
  expect_equal(format_ci_string("40.234 - 52.891", "mean"), "40.2 - 52.9")
  expect_equal(format_ci_string("0.1234 - 0.5678", "perc"), "12 - 57")   # 12.34 -> 12, 56.78 -> 57
  expect_true(is.na(format_ci_string(NA_character_, "mean")))
})

test_that("13. format_summary() handles multiple variables (two numeric + one categorical) together", {
  data <- tibble(
    age    = c(10, 20, 30, 40, 50, 100),
    income = c(1000, 2000, 3000, 4000, 5000, 100000),
    region = factor(c("North", "South", "North", "East", "South", "North"))
  )
  stats_table <- calc_stats(data, outcomes = c("age", "income", "region"),
                             statistics = c("mean", "perc"), multicode = FALSE)
  result <- format_summary(pivot_summary(stats_table))

  expect_s3_class(result, "huxtable")
  expect_equal(nrow(result), 7)   # 1 header + age + income + region label + 3 levels

  age_row    <- which(result[["Variable"]] == "age")
  income_row <- which(result[["Variable"]] == "income")
  region_row <- which(result[["Variable"]] == "region")
  north_row  <- which(result[["Variable"]] == "North")
  south_row  <- which(result[["Variable"]] == "South")
  east_row   <- which(result[["Variable"]] == "East")

  # age and income both come before region, in the order requested
  expect_true(age_row < income_row)
  expect_true(income_row < region_row)

  # income's mean has no big.mark (that rule only applies to count/sum, not
  # mean/median - see format_statistic()), so it's "19166.7", not "19,166.7"
  expect_equal(result[["Estimate"]][age_row],    "41.7")
  expect_equal(result[["Estimate"]][income_row], "19166.7")
  expect_equal(result[["Estimate"]][north_row],  "50")   # no "%" sign, rounded to 0dp
  expect_equal(result[["Estimate"]][south_row],  "33")
  expect_equal(result[["Estimate"]][east_row],   "17")
  expect_true(is.na(result[["Estimate"]][region_row]))

  expect_true(all(result[["Base"]][c(age_row, income_row, north_row, south_row, east_row)] == "6"))

  # Bold: variable names bold, category levels not - same check as test 10,
  # extended to the two numeric variables shown alongside the categorical one.
  variable_col <- which(names(result) == "Variable")
  expect_true(huxtable::bold(result)[age_row, variable_col])
  expect_true(huxtable::bold(result)[income_row, variable_col])
  expect_true(huxtable::bold(result)[region_row, variable_col])
  expect_false(huxtable::bold(result)[north_row, variable_col])
  expect_false(huxtable::bold(result)[south_row, variable_col])
  expect_false(huxtable::bold(result)[east_row, variable_col])

  # Stripe still alternates correctly over the larger row set - same
  # consecutive-differ / two-apart-match pattern as test 10.
  bg <- huxtable::background_color(result)
  body_colours <- unname(bg[2:nrow(result), 1])
  expect_true(all(body_colours[-length(body_colours)] != body_colours[-1]))
  expect_equal(body_colours[1], body_colours[3])
})

test_that("14. format_summary() merges a variable's repeated Variable text across its two statistics", {
  data <- tibble(x = c(10, 20, 30, 40, 50, 100))
  stats_table <- calc_stats(data, outcomes = "x", statistics = c("mean", "median"), multicode = FALSE)
  result <- format_summary(pivot_summary(stats_table))

  expect_equal(nrow(result), 3)   # 1 header + mean row + median row

  mean_pos   <- which(result[["Statistics"]] == "Mean")
  median_pos <- which(result[["Statistics"]] == "Median")
  expect_equal(result[["Estimate"]][mean_pos],   "41.7")
  expect_equal(result[["Estimate"]][median_pos], "35.0")

  # Variable text merges down to one cell rather than repeating "x" on both
  # rows - checked directly via the stored cell text and rowspan(), not by
  # eye, same reasoning as the stripe checks above.
  #
  # NOT "" on the median row's own Variable cell - a real test run against
  # pivot_crosstab.R's copy of this same merge pattern caught this: per
  # huxtable's own docs (spans.Rd), setting rowspan() COPIES the anchor
  # cell's content into every cell it covers, so extracting a covered cell
  # (via `[[`, same as printing) always resolves back to "x" regardless of
  # what format_summary() tries to assign there afterwards. This function
  # used to have a follow-up loop attempting to blank the covered cell -
  # removed as dead code (it never had any observable effect - the covered
  # row was always DISPLAYED as blank anyway, since rowspan collapses it
  # visually either way, independent of the underlying stored value).
  variable_col <- which(names(result) == "Variable")
  expect_equal(result[["Variable"]][mean_pos],   "x")
  expect_equal(result[["Variable"]][median_pos], "x")
  expect_equal(huxtable::rowspan(result)[mean_pos, variable_col], 2)

  # Top padding: only the first (mean) row of this numeric variable's block
  # gets the extra top padding - the second (median) row, though merged into
  # the same visual cell for Variable, stays at the tight baseline.
  expect_equal(huxtable::top_padding(result)[mean_pos,   variable_col], 6)
  expect_equal(huxtable::top_padding(result)[median_pos, variable_col], 1)
})

test_that("15. a real multicode variable is pivoted the same way as an ordinary categorical one", {

  # Same multi-select construction as test_calc_stats.R's test 34/35 - three
  # Yes/No columns sharing the "Q1: " label stem, collapsed by
  # convert_multicodes() into one variable (outcome_type = "multicoded")
  # with three levels. pivot_summary()'s is_categorical check now reads
  # outcome_type directly (outcome_type %in% c("categorical", "multicoded")),
  # so this is the test that actually exercises the "multicoded" branch of
  # that check with real data, rather than just the numeric/plain-categorical
  # cases everything else in this file already covers.
  data <- tibble(
    Q1_OptionA = factor(c("Yes", "No",  "Yes", "No")),
    Q1_OptionB = factor(c("No",  "Yes", "No",  "Yes")),
    Q1_OptionC = factor(c("No",  "No",  "Yes", "Yes"))
  )
  attr(data$Q1_OptionA, "label") <- "Q1: Option A"
  attr(data$Q1_OptionB, "label") <- "Q1: Option B"
  attr(data$Q1_OptionC, "label") <- "Q1: Option C"

  stats_table <- calc_stats(data, outcomes = c("Q1_OptionA", "Q1_OptionB", "Q1_OptionC"),
                             statistics = "perc", multicode = TRUE)
  result <- pivot_summary(stats_table)[[1]]

  expect_equal(nrow(result), 4)   # 1 label row + 3 levels, same shape as a plain categorical variable

  label_row  <- result %>% filter(row_type == "label")
  level_rows <- result %>% filter(row_type == "data")

  expect_equal(label_row$Variable, "Q1")
  expect_true(is.na(label_row$Estimate))
  expect_true(is.na(label_row$Base))

  expect_setequal(level_rows$Variable, c("Option A", "Option B", "Option C"))
  expect_true(all(level_rows$Estimate == 0.5))
  expect_false(any(is.na(level_rows$Estimate)))
})
