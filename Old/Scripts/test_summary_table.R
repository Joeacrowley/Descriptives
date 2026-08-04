# =============================================================================
# TESTS for summary_table.R
#
# I have NOT run these myself — same caveat as the other test files in this
# project, run them with:
#
#   testthat::test_file(here::here("Scripts", "test_summary_table.R"))
#
# Confidence varies more in this file than in test_simplified_functions.R.
# tidy_statistic_description(), identify_natural_breaks_pivot2(), and pivot1()
# are plain data-frame functions — the same kind of known-answer test used
# throughout the rest of this project, and I'm confident in these.
#
# merge_rows() and pivot1h() operate on huxtable objects, and I can't verify
# huxtable's exact indexing/extraction behaviour (e.g. whether `ht[[col]]`
# includes the header row, whether `ht[row, col]` returns a bare scalar or a
# 1x1 object) without running R. Where that matters, the test checks
# something robust to that uncertainty (e.g. "how many cells in this column
# are blank" rather than "is cell [2,1] blank") rather than a single fragile
# assumption — but treat this section as the most likely to need small
# adjustments.
#
# 17 tests:
#   1-4   tidy_statistic_description()   (all 4 labelling branches)
#   5-6   identify_natural_breaks_pivot2()
#   7-8   merge_rows()
#   9-12  pivot1()
#   13    pivot1h() — change #1: Statistics column now merges like Variable
#   14    pivot1h() — drops the Levels column for numeric-only tables
#   15-17 summary_table()
# =============================================================================
# file.create(here::here(".here"))
# setwd('/Users/joecrowley/5 - R/Descriptives')
#
library(testthat)
source(here::here("Scripts", "simplified_functions.R"))
source(here::here("Scripts", "summary_table.R"))


# =============================================================================
# tidy_statistic_description()
# =============================================================================

test_that("1. concise = TRUE produces short labels", {
  data <- tibble(stat = c("mean", "w_perc", "count"))
  result <- tidy_statistic_description(data, concise = TRUE)

  expect_true("Statistics" %in% names(result))
  expect_equal(result$Statistics, c("Mean", "% (w)", "Count"))
})


test_that("2. an all-weighted table gets 'Statistics (weighted)' without repeating 'weighted' per row", {
  data <- tibble(stat = c("w_mean", "w_perc"))
  result <- tidy_statistic_description(data)

  expect_true("Statistics (weighted)" %in% names(result))
  expect_equal(result[["Statistics (weighted)"]], c("Mean", "Percentage"))
})


test_that("3. an all-unweighted table gets 'Statistics (unweighted)' without repeating 'unweighted' per row", {
  data <- tibble(stat = c("mean", "perc"))
  result <- tidy_statistic_description(data)

  expect_true("Statistics (unweighted)" %in% names(result))
  expect_equal(result[["Statistics (unweighted)"]], c("Mean", "Percentage"))
})


test_that("4. a mixed weighted/unweighted table spells out 'Weighted'/'Unweighted' per row", {
  data <- tibble(stat = c("mean", "w_perc"))
  result <- tidy_statistic_description(data)

  expect_true("Statistics" %in% names(result))
  expect_equal(result$Statistics, c("Unweighted mean", "Weighted percentage"))
})


# =============================================================================
# identify_natural_breaks_pivot2()
# =============================================================================

# a,a,b,b,b,c grouped on column 1 -> runs of length 2,3,1 -> ends at 2,5,6

test_that("5. finds the correct break points for a single grouping column", {
  data <- tibble(a = c("x", "x", "y", "y", "y", "z"), b = 1:6)
  expect_equal(identify_natural_breaks_pivot2(data, vars = 1), c(2, 5, 6))
})


# Grouping on both columns together, where column b is unique on every row,
# means every row is its own run -> a break after every row.

test_that("6. finds a break after every row when the combined grouping key is unique per row", {
  data <- tibble(a = c("x", "x", "y", "y", "y", "z"), b = 1:6)
  expect_equal(identify_natural_breaks_pivot2(data, vars = c(1, 2)), 1:6)
})


# =============================================================================
# merge_rows()
# =============================================================================

# add_colnames = FALSE so row 1 of the resulting huxtable is the first real
# data row, not a header — avoids any ambiguity about header-row offsets for
# this test, which is about merge_rows() in isolation.

test_that("7. merges consecutive identical rows and blanks the cells beneath them", {
  ht <- tibble(g = c("A", "A", "A", "B", "B"), val = 1:5) %>% huxtable::as_hux(add_colnames = FALSE)
  result <- merge_rows(ht, col_nums = 1, cols_to_merge = 1)

  merged_col <- result[[1]]
  expect_equal(huxtable::rowspan(result)[1, 1], 3)
  expect_equal(sum(merged_col == ""), 2)   # the two blanked-out rows within the "A" run
  expect_equal(huxtable::rowspan(result)[4, 1], 2)
})


test_that("8. rows_to_exclude prevents a run from being merged even when repeated", {
  ht <- tibble(g = c("A", "A", "A", "B", "B"), val = 1:5) %>% huxtable::as_hux(add_colnames = FALSE)
  result <- merge_rows(ht, col_nums = 1, cols_to_merge = 1, rows_to_exclude = 1)

  # The "A" run starts at row 1, which is excluded -> should NOT be merged.
  expect_equal(huxtable::rowspan(result)[1, 1], 1)
  expect_equal(result[[1]][2], "A")   # not blanked

  # The "B" run starts at row 4, not excluded -> should still merge normally.
  expect_equal(huxtable::rowspan(result)[4, 1], 2)
})


# =============================================================================
# pivot1()
# =============================================================================

# Hand-built calc_stats()-shaped input rather than a real calc_stats() call,
# to test pivot1()'s own transformation logic in isolation. Row 4 has
# cross_break = "group1" specifically to check it gets filtered out.

fake_stats <- tibble(
  cross_break       = c("Total", "Total", "Total", "group1"),
  outcome           = c("age_num", "gender_cat", "gender_cat", "age_num"),
  o_lab             = c("Age", "Gender", "Gender", "Age"),
  o_cat             = c("mean", "Male", "Female", "mean"),
  stat              = c("mean", "perc", "perc", "mean"),
  estimate          = c(45.2, 0.48, 0.52, 50.1),
  estimate_se       = c("-", "-", "-", "-"),
  base              = c(100, 100, 100, 40),
  base_description  = rep(NA_character_, 4),
  p_value           = rep(NA_real_, 4),
  p_method          = rep(NA_character_, 4)
)

test_that("9. pivot1() keeps only the Total rows", {
  result <- pivot1(fake_stats)[[1]]
  expect_equal(nrow(result), 3)
})

test_that("10. pivot1() renames columns for display", {
  result <- pivot1(fake_stats)[[1]]
  expect_true(all(c("Variable", "Levels", "Estimate", "Base") %in% names(result)))
  expect_false("o_lab" %in% names(result))
})

test_that("11. pivot1() blanks Levels for numeric-statistic rows but keeps real categories", {
  result <- pivot1(fake_stats)[[1]]
  age_row     <- result %>% filter(Variable == "Age")
  gender_rows <- result %>% filter(Variable == "Gender")

  expect_equal(age_row$Levels, "")
  expect_setequal(gender_rows$Levels, c("Male", "Female"))
})

test_that("12. pivot1() drops the SE column entirely when no SE was requested", {
  result <- pivot1(fake_stats)[[1]]
  expect_false("SE" %in% names(result))
})


# =============================================================================
# pivot1h()
# =============================================================================

test_that("13. pivot1h() merges repeated Statistics cells, not just Variable (change #1)", {
  gender_stats <- tibble(
    cross_break       = rep("Total", 3),
    outcome           = rep("gender_cat", 3),
    o_lab             = rep("Gender", 3),
    o_cat             = c("Male", "Female", "Other"),
    stat              = rep("perc", 3),
    estimate          = c(0.45, 0.45, 0.10),
    estimate_se       = rep("-", 3),
    base              = rep(100, 3),
    base_description  = rep(NA_character_, 3),
    p_value           = rep(NA_real_, 3),
    p_method          = rep(NA_character_, 3)
  )

  formatted <- pivot1(gender_stats, concise = TRUE) %>% pivot1h()
  statistics_col <- formatted[["Statistics"]]

  expect_true("%" %in% statistics_col)
  expect_equal(sum(statistics_col == ""), 2)   # 2 of the 3 repeated "%" cells merged away
})


test_that("14. pivot1h() drops the Levels column when every row is a numeric statistic", {
  age_only <- tibble(
    cross_break = "Total", outcome = "age_num", o_lab = "Age", o_cat = "mean",
    stat = "mean", estimate = 45, estimate_se = "-", base = 100,
    base_description = NA_character_, p_value = NA_real_, p_method = NA_character_
  )

  formatted <- pivot1(age_only, concise = TRUE) %>% pivot1h()
  expect_false("Levels" %in% names(formatted))
})


# =============================================================================
# summary_table()
# =============================================================================

summary_data <- tibble(
  age    = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
  gender = factor(c("Male", "Female", "Male", "Female", "Male",
                     "Female", "Male", "Female", "Male", "Female"))
)

test_that("15. summary_table() runs end to end and returns a flextable", {
  result <- summary_table(summary_data, outcomes = c("age", "gender"))
  expect_s3_class(result, "flextable")
})

test_that("16. summary_table(raw = TRUE) returns pivot1()'s list structure instead", {
  result <- summary_table(summary_data, outcomes = c("age", "gender"), raw = TRUE)

  expect_type(result, "list")
  expect_equal(length(result), 2)
  expect_s3_class(result[[1]], "data.frame")
})

test_that("17. summary_table() defaults to concise statistic labels", {
  result <- summary_table(summary_data, outcomes = c("age", "gender"), raw = TRUE)
  labels <- result[[1]]$Statistics

  expect_true(all(labels %in% c("Mean", "%")))
  expect_false(any(str_detect(labels, "Weighted|Unweighted")))
})
