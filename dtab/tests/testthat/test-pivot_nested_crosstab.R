# =============================================================================
# Tests for pivot_nested_crosstab.R
# Same caveat as every other test file in this project: unverified, no R in
# this environment. Run with:
#   testthat::test_file("tests/testthat/test-pivot_nested_crosstab.R"), or devtools::test()
# Tests 1-2 cover the core reshape with a numeric variable: test 1 is the
# clean case (hand-calculated means for all 4 outer x inner combinations),
# test 2 removes one combination's respondents entirely to check
# rectangularity (tidyr::complete() filling it in as a genuinely blank
# column rather than the table silently losing a column). Tests 3-4 are the
# two validation errors. Test 5 covers a categorical variable (label + level
# rows, hand-calculated proportions). Test 6 is format_nested_crosstab()'s
# 4-row header - the part I'd check most carefully by hand, both the
# insert_row() bookkeeping and huxtable::colspan()/merge_cells() behaving
# the way format_crosstab()'s tests already assumed they do (same lower-
# confidence flag on colspan() as an accessor name, unconfirmed without R).
# Tests 7-11 fill gaps flagged after the fact (Joe asked directly whether
# SE/CI/p-value/multiple-outcome cases were covered - they weren't). Test 7
# is conf = "se" (hand-calculated, deliberately using the same clean data as
# test 1/6 so the SE arithmetic works out to a round number). Test 8 is
# conf = "ci" - checked structurally (a "<low> - <high>" string, non-NA)
# rather than an exact figure, since a 2-observation group's t-based CI
# multiplier isn't a clean hand-calculable number the way the SE is.
# Test 9 covers p-values properly, per Joe's explicit design call: one
# p-value per OUTER level (e.g. "among Male respondents, is age_group
# associated with age"), not one for the whole nested set - this replaced
# an earlier version of this test that (correctly, at the time) asserted no
# p-value ever attached; that assumption changed once calc_stats.R grew
# nested_pvalues(). Test 10 is two outcomes requested together, checking
# base_wide's distinct(outcome, o_lab) crossing actually works with more
# than one outcome present, not just the single-outcome case every other
# test here uses. Test 11 is format_nested_crosstab()'s side of the
# per-outer-level p-value columns - each one's span (rows 1-3) has to widen
# to fold it in, the same way a flat crosstab's span already folds in its
# own p-value column.
# Tests 12-13 cover the Base-relocation feature (Joe's request: every
# variable's Base row pulled into one consolidated "Sample sizes" section at
# the bottom, rather than sitting inline right under each variable's own
# data; individual base rows show no "Base" text in Statistics since the
# section header already conveys it once, and Variable+Statistics are
# merged into a single wide cell in that section for extra label room).
# Test 12 is pivot_nested_crosstab()'s side - deliberately uses two outcomes
# of DIFFERENT types (numeric and categorical) requested together, since
# that's the case most likely to expose an outcome-type-specific assumption
# in the relocation logic (there isn't one - relocation keys off row_type,
# not outcome_type - but this is the test that would catch it if there were
# one). Test 13 is format_nested_crosstab()'s side - the padding refinement,
# checked via huxtable::top_padding() (same lower-confidence accessor-name
# flag as colspan() elsewhere in this file): the section header should get
# the same top-padding gap any other block start gets, and every individual
# base row after it should NOT get its own gap, so the section reads as one
# tight block under a single header rather than one gap per variable. It
# also covers a real regression relocation introduced and I caught while
# implementing it, not something Joe asked for directly: block-boundary
# detection used to infer "a new numeric variable just started" from a data
# row landing right after the PREVIOUS variable's base row - always true
# under the old inline layout, never true anymore now that every base row
# moves to the bottom, which would have silently stopped separating two
# adjacent numeric variables (e.g. age directly followed by score) with any
# padding at all. Re-derived from o_lab instead (kept around through
# format_nested_crosstab() now for exactly this - see its block-boundary
# comment for why o_lab, not the Variable column, which a conf/SE row
# deliberately blanks and would have broken the same check a different way).
# Tests 14-16 port the pairwise-significance mechanics (legend row, Sig.
# diff row, SE/CI suppression) from pivot_crosstab.R's own tests 18-21 -
# straightforward here because each composite (outer, inner) column IS
# already exactly the unit assign_sig_letters() keys a letter to
# (cross_break, p_cat1, p_cat2) - no new keying logic needed, just reshaping
# sig_letter/sig_diff the same way estimate/conf already get reshaped in
# reshape_nested_predictor_set(). All 4 groups in the synthetic data below
# are deliberately given very different Yes-rates (0.8/0.2/0.2/0.8) so real
# pairwise differences are close to guaranteed without needing to hand-solve
# the exact test statistic - these tests check structure (legend content
# matches pivot_nested_crosstab()'s own legend output, not hardcoded
# letters; a Sig. diff row appears; SE rows are suppressed), not exact
# letter assignment, the same restraint test 20 in test_pivot_crosstab.R
# uses for the same reason (which letter lands on which column depends on
# row order inside assign_sig_letters(), not something worth pinning to a
# specific value here). Test 14 is pivot_nested_crosstab()'s side (legend/
# __sigdiff present only when pairwise = TRUE, absent otherwise - backward
# compatibility). Test 15 is format_nested_crosstab()'s side (legend row
# position/content, Sig. diff row, SE suppression). Test 16 is a fuller,
# weighted, pval = TRUE example printed via huxtable::as_flextable() for
# visual review, same purpose as test 8's CI-range print and test 21 in
# test_pivot_crosstab.R.
# =============================================================================


# Male/Young: age 10,20 -> mean 15   Male/Old: age 30,40 -> mean 35
# Female/Young: age 50,60 -> mean 55   Female/Old: age 70,80 -> mean 75
# Explicit factor levels (sex: Male,Female; age_group: Young,Old) so
# group_by()/summarise() - which orders its output by FACTOR LEVEL, not row-
# appearance - produces a deterministic, known column order to assert on.

test_that("1. pivot_nested_crosstab() builds one composite column per (outer, inner) combination with hand-calculated means", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60, 70, 80),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = list(c("sex", "age_group")),
                             statistics = "mean", multicode = FALSE)
  result <- pivot_nested_crosstab(stats_table)
  pivoted     <- result[[1]]
  col_order   <- result[[3]]
  header_info <- result[[4]]

  expect_equal(col_order, c("Male: Young", "Male: Old", "Female: Young", "Female: Old"))

  mean_row <- pivoted %>% filter(Statistics == "Mean")
  expect_equal(mean_row[["Male: Young"]], 15)
  expect_equal(mean_row[["Male: Old"]], 35)
  expect_equal(mean_row[["Female: Young"]], 55)
  expect_equal(mean_row[["Female: Old"]], 75)

  expect_equal(header_info$outer_level, c("Male", "Male", "Female", "Female"))
  expect_equal(header_info$inner_level, c("Young", "Old", "Young", "Old"))
  expect_equal(unique(header_info$outer_label), "sex")      # no label attribute set -> falls back to the variable name
  expect_equal(unique(header_info$inner_label), "age_group")
})


test_that("2. pivot_nested_crosstab() keeps a rectangular column set even when one combination has zero respondents", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female"), levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young"), levels = c("Young", "Old"))
    # No Female/Old rows at all - that combination has zero respondents.
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = list(c("sex", "age_group")),
                             statistics = "mean", multicode = FALSE)
  result <- pivot_nested_crosstab(stats_table)
  pivoted   <- result[[1]]
  col_order <- result[[3]]

  # The column still exists - calc_stats() itself would never have produced
  # a "Female: Old" row at all (group_by()/summarise() don't invent empty
  # groups); tidyr::complete() inside reshape_nested_predictor_set() is what
  # puts the column back in as genuinely NA rather than missing.
  expect_equal(col_order, c("Male: Young", "Male: Old", "Female: Young", "Female: Old"))

  mean_row <- pivoted %>% filter(Statistics == "Mean")
  expect_true(is.na(mean_row[["Female: Old"]]))
  expect_equal(mean_row[["Female: Young"]], 55)
})


test_that("3. pivot_nested_crosstab() rejects data without a nested predictor set", {
  data <- tibble(age = c(1, 2, 3, 4), sex = factor(c("Male", "Male", "Female", "Female")))
  stats_table <- calc_stats(data, outcomes = "age", predictors = "sex", statistics = "mean", multicode = FALSE)

  expect_error(pivot_nested_crosstab(stats_table), "nested predictor set")
})


test_that("4. pivot_nested_crosstab() rejects more than one statistic on a categorical variable", {
  data <- tibble(
    y         = factor(c("Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No")),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "y", predictors = list(c("sex", "age_group")),
                             statistics = c("perc", "count"), multicode = FALSE)

  expect_error(pivot_nested_crosstab(stats_table), "only supports one statistic")
})


# Male/Young: Yes,Yes,No,No -> 2/4 = 0.5     Male/Old: Yes,No,No,No -> 1/4 = 0.25
# Female/Young: Yes,Yes,Yes,No -> 3/4 = 0.75
# Female/Old: No,No,No,No -> zero "Yes" respondents in that group. calc_stats()
# never produces a row for a zero-occurrence o_cat level at all (see the
# header note on this in pivot_nested_crosstab.R - group_by() on two factor
# columns together doesn't invent unobserved combinations the way grouping
# by one factor does), so pivot_nested_crosstab() currently fills that cell
# as NA (renders as "-" downstream in format_nested_crosstab()), not a
# computed 0 - this is a real, still-open design question (flagged to Joe,
# not yet answered): should a genuine zero-occurrence category display "0"
# instead of a dash? If that gets implemented later, this assertion is the
# one that would need to flip from NA to 0.

test_that("5. pivot_nested_crosstab() handles a categorical variable with label + level rows", {
  data <- tibble(
    y = factor(c(
      "Yes", "Yes", "No",  "No",
      "Yes", "No",  "No",  "No",
      "Yes", "Yes", "Yes", "No",
      "No",  "No",  "No",  "No"
    )),
    sex       = factor(rep(c("Male", "Male", "Female", "Female"), each = 4), levels = c("Male", "Female")),
    age_group = factor(rep(c("Young", "Old", "Young", "Old"), each = 4), levels = c("Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "y", predictors = list(c("sex", "age_group")),
                             statistics = "perc", multicode = FALSE)
  result <- pivot_nested_crosstab(stats_table)
  pivoted <- result[[1]]

  expect_equal(pivoted$Variable[1], "y")     # label row
  expect_true(is.na(pivoted$Statistics[1]))

  yes_row <- pivoted %>% filter(Variable == "Yes")
  expect_equal(yes_row[["Male: Young"]], 0.5)
  expect_equal(yes_row[["Male: Old"]], 0.25)
  expect_equal(yes_row[["Female: Young"]], 0.75)
  expect_true(is.na(yes_row[["Female: Old"]]))
})


test_that("6. format_nested_crosstab() builds the 4-row nested header with correct spans", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60, 70, 80),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = list(c("sex", "age_group")),
                             statistics = "mean", multicode = FALSE)
  result <- format_nested_crosstab(pivot_nested_crosstab(stats_table))

  male_young_col   <- which(names(result) == "Male: Young")
  male_old_col     <- which(names(result) == "Male: Old")
  female_young_col <- which(names(result) == "Female: Young")
  female_old_col   <- which(names(result) == "Female: Old")

  # Row 1: outer label ("sex"), spanning all 4 nested columns. merge_cells()
  # only stores text/colspan at the ANCHOR cell (the lowest column index in
  # the merged range) - same behaviour format_crosstab()'s tests already
  # rely on, computed dynamically here rather than assumed to be any
  # particular named column.
  anchor <- min(male_young_col, male_old_col, female_young_col, female_old_col)
  expect_equal(result[[anchor]][1], "sex")
  expect_equal(huxtable::colspan(result)[1, anchor], 4)

  # Row 2: outer levels - "Male" spanning its 2 columns, "Female" its own 2.
  male_anchor   <- min(male_young_col, male_old_col)
  female_anchor <- min(female_young_col, female_old_col)
  expect_equal(result[[male_anchor]][2], "Male")
  expect_equal(huxtable::colspan(result)[2, male_anchor], 2)
  expect_equal(result[[female_anchor]][2], "Female")
  expect_equal(huxtable::colspan(result)[2, female_anchor], 2)

  # Row 3: inner label ("age_group"), repeated once per outer level - same
  # spans as row 2, different text.
  expect_equal(result[[male_anchor]][3], "age_group")
  expect_equal(huxtable::colspan(result)[3, male_anchor], 2)
  expect_equal(result[[female_anchor]][3], "age_group")
  expect_equal(huxtable::colspan(result)[3, female_anchor], 2)

  # Row 4: bare inner levels, one per column - no "sex: "/"age_group: "
  # prefix repeated from the composite column name that names(result) still
  # carries internally.
  expect_equal(result[["Male: Young"]][4], "Young")
  expect_equal(result[["Male: Old"]][4], "Old")
  expect_equal(result[["Female: Young"]][4], "Young")
  expect_equal(result[["Female: Old"]][4], "Old")

  # Row 5 is the first real data row (Mean), header_offset (4) + 1 - values
  # formatted to 1dp same as format_crosstab()'s mean rule.
  expect_equal(result[["Male: Young"]][5], "15.0")
  expect_equal(result[["Female: Old"]][5], "75.0")

  # Rows 1:3 (the outer/inner group header rows - "sex" / "Male"+"Female" /
  # "age_group") are centred, not right-aligned - right-alignment is meant
  # for data, and was previously clobbering this center setting because it
  # was applied "everywhere" (all rows) rather than scoped to header_offset
  # onward. Row 4 (bare "Young"/"Old" levels) and row 5 (real data) stay
  # right-aligned, unaffected by Joe's ask, which was specifically about
  # rows 1:3. align() as a huxtable accessor - same lower-confidence flag as
  # colspan()/top_padding() elsewhere in this file, unconfirmed without R.
  expect_true(all(huxtable::align(result)[1:3, male_anchor] == "center"))
  expect_true(all(huxtable::align(result)[1:3, female_anchor] == "center"))
  expect_equal(huxtable::align(result)[4, male_anchor], "right")
  expect_equal(huxtable::align(result)[5, male_anchor], "right")
})


# Same data as test 1/6 (age 10..80 in pairs) - convenient here too, since
# each pair's spread (10 apart) gives a clean SE: for x = c(a, a+10), mean =
# a+5, deviations -5/+5, sum of squares = 50, sample var = 50/1 = 50,
# sample sd = sqrt(50) = 7.071068, se = sd/sqrt(2) = 5.0 exactly - the same
# figure for all 4 groups, since every group has this identical shape.

test_that("7. format_nested_crosstab() inserts an SE row under each Mean row, with a hand-calculated value", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60, 70, 80),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = list(c("sex", "age_group")),
                             statistics = "mean", conf = "se", multicode = FALSE)
  result <- format_nested_crosstab(pivot_nested_crosstab(stats_table))

  mean_row <- which(result[["Statistics"]] == "Mean")
  se_row   <- which(result[["Statistics"]] == "SE")

  expect_equal(se_row, mean_row + 1)   # SE sits directly under its own Mean row
  expect_equal(result[["Male: Young"]][mean_row], "15.0")
  expect_equal(result[["Male: Young"]][se_row], "5.0")
  expect_equal(result[["Female: Old"]][mean_row], "75.0")
  expect_equal(result[["Female: Old"]][se_row], "5.0")
})


# CI not hand-calculated exactly - a 2-observation group's CI depends on a
# t-distribution multiplier (df = 1, a very wide interval) that isn't a
# clean number to verify by hand the way the SE above is. Checked
# structurally instead: a real "<low> - <high>" range string, non-NA,
# positioned right after its Mean row - same treatment test 25/26 in
# test_calc_stats.R give a significance-test p-value they can't hand-verify
# exactly either.

test_that("8. format_nested_crosstab() inserts a 95% CI row, formatted as a range string", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60, 70, 80),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = list(c("sex", "age_group")),
                             statistics = "mean", conf = "ci", multicode = FALSE)
  result <- format_nested_crosstab(pivot_nested_crosstab(stats_table))
  print(result %>% huxtable::as_flextable())

  mean_row <- which(result[["Statistics"]] == "Mean")
  ci_row   <- which(result[["Statistics"]] == "95% CI")

  expect_equal(ci_row, mean_row + 1)
  expect_false(is.na(result[["Male: Young"]][ci_row]))
  expect_true(stringr::str_detect(result[["Male: Young"]][ci_row], "^-?[0-9.]+ - -?[0-9.]+$"))
})


# calc_stats() now computes ONE p-value per (outcome, outer level) for a
# nested predictor set - "among Male respondents, is age_group associated
# with age", and the same question again for Female - not one shared value
# for the whole nested set (an explicit design call from Joe, not a
# default). See nested_pvalues()'s header note in calc_stats.R, and test 45
# there for the earlier crash this replaced (pval = TRUE erroring when every
# predictor set was nested). Not hand-calculated here - a 2-observation-per-
# group ANOVA/Kruskal-Wallis figure isn't a clean number to verify by hand -
# checked structurally (column exists, right position, in [0,1], shown once).

test_that("9. pivot_nested_crosstab() attaches one p-value column per outer level, positioned after that level's own columns", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60, 70, 80),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = list(c("sex", "age_group")),
                             statistics = "mean", pval = TRUE, multicode = FALSE)

  expect_true(any(!is.na(stats_table$p_value)))

  pivoted <- pivot_nested_crosstab(stats_table)[[1]]

  male_p_col   <- "p_value (age_group | Male)"
  female_p_col <- "p_value (age_group | Female)"
  expect_true(all(c(male_p_col, female_p_col) %in% names(pivoted)))

  # Each sits right after its OWN outer level's last inner column, not
  # clustered together at the very end - same "next to the data it tests"
  # placement flat crosstabs already use for their per-set p-value column.
  expect_equal(names(pivoted)[which(names(pivoted) == "Male: Old") + 1], male_p_col)
  expect_equal(names(pivoted)[which(names(pivoted) == "Female: Old") + 1], female_p_col)

  mean_row <- which(pivoted$Statistics == "Mean")
  # Statistics is blanked (NA) on individual base rows now - showing "Base"
  # there was redundant once the relocated block got its own "Sample sizes"
  # section header - so row_type is what actually identifies the base row.
  base_row <- which(pivoted$row_type == "base")

  expect_false(is.na(pivoted[[male_p_col]][mean_row]))
  expect_true(pivoted[[male_p_col]][mean_row] >= 0 && pivoted[[male_p_col]][mean_row] <= 1)
  expect_false(is.na(pivoted[[female_p_col]][mean_row]))
  expect_true(is.na(pivoted[[male_p_col]][base_row]))
  expect_true(is.na(pivoted[[female_p_col]][base_row]))
})


# Two outcomes together: age (mean 1.5/3.5/5.5/7.5 wouldn't be right - see
# below) and score, sharing the same sex x age_group breakdown. score is
# deliberately a second, independent numeric column (1..8) so its own means
# (Male/Young = 1.5, Male/Old = 3.5, Female/Young = 5.5, Female/Old = 7.5)
# are distinguishable from age's (15/35/55/75) - if base_wide's
# distinct(outcome, o_lab) crossing (added when fixing the base-duplication
# bug) mixed the two outcomes up, this would catch it.

test_that("10. pivot_nested_crosstab() handles multiple outcomes together, each keeping its own correct values and base", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60, 70, 80),
    score     = c(1, 2, 3, 4, 5, 6, 7, 8),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = c("age", "score"), predictors = list(c("sex", "age_group")),
                             statistics = "mean", multicode = FALSE)
  result    <- pivot_nested_crosstab(stats_table)
  pivoted   <- result[[1]]
  col_order <- result[[3]]

  expect_equal(col_order, c("Male: Young", "Male: Old", "Female: Young", "Female: Old"))

  # row_type == "data" matters here, not just Variable == "age" - a numeric
  # outcome's relocated Base row now deliberately carries that SAME Variable
  # text (see pivot_nested_crosstab()'s Base-relocation note: Variable is
  # set to the variable's own label there too, on purpose), so filtering by
  # Variable alone would catch both rows instead of just the data row.
  age_row   <- pivoted %>% filter(Variable == "age", row_type == "data")
  score_row <- pivoted %>% filter(Variable == "score", row_type == "data")

  expect_equal(age_row[["Male: Young"]], 15)
  expect_equal(age_row[["Female: Old"]], 75)
  expect_equal(score_row[["Male: Young"]], 1.5)
  expect_equal(score_row[["Female: Old"]], 7.5)

  # One Base row per outcome, both correctly showing 2 respondents per cell
  # (no missing-data difference between age and score in this dataset).
  # row_type, not Statistics == "Base" - Statistics is blanked (NA) on
  # individual base rows now that the relocated block has its own
  # "Sample sizes" section header instead.
  base_rows <- pivoted %>% filter(row_type == "base")
  expect_equal(nrow(base_rows), 2)
  expect_true(all(base_rows[["Male: Young"]] == 2))
  expect_true(all(base_rows[["Female: Old"]] == 2))
})


test_that("11. format_nested_crosstab() folds each outer level's p-value column into its own header span", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60, 70, 80),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = list(c("sex", "age_group")),
                             statistics = "mean", pval = TRUE, multicode = FALSE)
  result <- format_nested_crosstab(pivot_nested_crosstab(stats_table))
  print(result %>% huxtable::as_flextable())

  male_p_col     <- which(names(result) == "p_value (age_group | Male)")
  female_p_col   <- which(names(result) == "p_value (age_group | Female)")
  male_young_col <- which(names(result) == "Male: Young")
  male_old_col   <- which(names(result) == "Male: Old")
  female_old_col <- which(names(result) == "Female: Old")

  # Row 2's "Male" span now covers 3 columns (Young/Old + its own p-value
  # column), not 2 - same widening flat crosstabs already do for their own
  # p-value column.
  male_anchor <- min(male_young_col, male_old_col, male_p_col)
  expect_equal(result[[male_anchor]][2], "Male")
  expect_equal(huxtable::colspan(result)[2, male_anchor], 3)

  # Row 1's overall span now covers all 6 columns (4 levels + 2 p-value
  # columns), not just the 4 level columns.
  all_anchor <- min(male_young_col, female_old_col, male_p_col, female_p_col)
  expect_equal(huxtable::colspan(result)[1, all_anchor], 6)

  # p-value text itself, formatted via format_pvalue() - 3dp or "<0.001",
  # shown on the Mean row.
  mean_row <- which(result[["Statistics"]] == "Mean")
  expect_true(stringr::str_detect(result[[male_p_col]][mean_row], "^(<0\\.001|[01]\\.\\d{3})$"))
})


test_that("12. pivot_nested_crosstab() relocates every variable's Base row into one consolidated section at the bottom, correctly labelled, with two outcomes of DIFFERENT types requested together", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60, 70, 80),
    result    = factor(c("Pass", "Pass", "Fail", "Fail", "Pass", "Pass", "Fail", "Fail")),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )
  # "mean" only applies to the numeric outcome (age), "perc" only to the
  # categorical one (result) - calc_stats() strips whichever doesn't apply
  # to each outcome's type internally (see its own header note on this),
  # so requesting both together is exactly how a real mixed-type call would
  # look, not a special case this test has to work around.
  stats_table <- calc_stats(data, outcomes = c("age", "result"), predictors = list(c("sex", "age_group")),
                             statistics = c("mean", "perc"), multicode = FALSE)
  result  <- pivot_nested_crosstab(stats_table)
  pivoted <- result[[1]]

  # age: 1 data row (numeric, no label row). result: 1 label row + 2 data
  # rows (Pass/Fail). Plus the relocated section: 1 "Sample sizes" header +
  # 2 base rows (one per outcome) = 7 rows total.
  expect_equal(nrow(pivoted), 7)

  row_types      <- pivoted$row_type
  base_positions <- which(row_types == "base")
  last_non_base  <- max(which(row_types != "base"))

  # every base row sits after every non-base row - nothing base-related is
  # still interleaved with either variable's own data.
  expect_true(all(base_positions > last_non_base))

  # exactly one section header immediately precedes the relocated block.
  header_pos <- min(base_positions) - 1
  expect_equal(pivoted$row_type[header_pos], "label")
  expect_equal(pivoted$Variable[header_pos], "Sample sizes")

  # each base row carries its OWN variable's label (not blank, per the old
  # inline convention), in the order the outcomes were requested.
  base_rows <- pivoted %>% filter(row_type == "base")
  expect_equal(base_rows$Variable, c("age", "result"))

  # hand-calculated: 2 respondents in every (sex, age_group) cell, for both
  # outcomes - no missing data for either variable in this dataset.
  expect_true(all(base_rows[["Male: Young"]] == 2))
  expect_true(all(base_rows[["Female: Old"]] == 2))
})


test_that("13. format_nested_crosstab() still separates two adjacent NUMERIC variables, and gives the Base section header its own top-padding gap while keeping every base row after it tight", {
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60, 70, 80),
    score     = c(1, 2, 3, 4, 5, 6, 7, 8),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = c("age", "score"), predictors = list(c("sex", "age_group")),
                             statistics = "mean", multicode = FALSE)
  result <- format_nested_crosstab(pivot_nested_crosstab(stats_table))

  # top_padding() as a huxtable accessor - same lower-confidence flag as
  # colspan() elsewhere in this file, unconfirmed without R.
  #
  # Statistics == "Mean" matters here, not just Variable == "age" - the
  # relocated Base row for a numeric outcome deliberately carries that SAME
  # Variable text (see pivot_nested_crosstab()'s Base-relocation note), so
  # matching on Variable alone catches both rows instead of just the data
  # row - the exact ambiguity test 10 already had to work around, missed
  # here on this test's first pass.
  variable_col <- which(names(result) == "Variable")
  age_row      <- which(result[[variable_col]] == "age" & result[["Statistics"]] == "Mean")
  score_row    <- which(result[[variable_col]] == "score" & result[["Statistics"]] == "Mean")

  # Relocating Base rows to the bottom means score's data row no longer
  # lands right after age's base row the way it would have under the old
  # inline layout - nothing structural marks the boundary between them
  # unless format_nested_crosstab() detects it some other way (it now does,
  # via o_lab - see that function's block-boundary comment). Both age's row
  # (the table's very first data row) and score's row (a new variable
  # starting) should get the same 6pt block-start gap.
  expect_equal(huxtable::top_padding(result)[age_row, variable_col], 6)
  expect_equal(huxtable::top_padding(result)[score_row, variable_col], 6)

  base_header_row  <- which(result[[variable_col]] == "Sample sizes")
  expect_equal(length(base_header_row), 1)
  # NOT is.na() here - confirmed against huxtable's own docs (not hand-
  # traced) that merge_cells() COPIES the anchor (Variable) cell's content
  # into the other cell in the merged range, rather than blanking it; this
  # is what huxtable actually does to survive row/column reordering after a
  # merge, and it's why an earlier version of this test (checking for NA)
  # failed against real output even though pivot_nested_crosstab() sets
  # Statistics to NA beforehand - that NA gets overwritten by the merge
  # itself. Harmless for display (only the anchor's position is ever shown
  # once merged) - this assertion exists to pin down that documented
  # behaviour, not to re-litigate whether it's desirable.
  expect_equal(result[["Statistics"]][base_header_row], "Sample sizes")

  # The section header is a block start (row_type == "label" upstream), so
  # it should get the same 6pt top-padding gap any other block start gets
  # (see format_nested_crosstab()'s block_start_rows/set_top_padding()).
  expect_equal(huxtable::top_padding(result)[base_header_row, variable_col], 6)

  # Every base row AFTER the header belongs to the same consolidated
  # section, not a block of its own - default padding (1), not the 6pt gap -
  # otherwise each variable's base row would read as its own loose block
  # rather than one tight section under a single header.
  base_data_rows <- (base_header_row + 1):nrow(result)
  expect_true(all(huxtable::top_padding(result)[base_data_rows, variable_col] == 1))

  # Variable and Statistics are merged into one wide cell across the whole
  # relocated section (header row included) - same colspan()-as-accessor
  # caveat as the nested-header tests above, unconfirmed without R.
  expect_equal(huxtable::colspan(result)[base_header_row, variable_col], 2)
  for (r in base_data_rows) {
    expect_equal(huxtable::colspan(result)[r, variable_col], 2)
    # Same copy-not-blank behaviour as the header row above - each
    # individual base row's Statistics ends up holding a copy of its OWN
    # row's Variable text (its variable's label), not NA and not "Sample
    # sizes" from the header row above it.
    expect_equal(result[["Statistics"]][r], result[[variable_col]][r])
  }
})


# Shared synthetic design for tests 14-16: 4 groups of 15, Yes-rates
# deliberately far apart (0.8 / 0.2 / 0.2 / 0.8) so real pairwise
# differences are all but guaranteed - not a hand-solvable exact figure
# (same reasoning test 7/8's SE/CI already lean on: structure over exact
# arithmetic once a real inferential test is involved). y's levels are
# fixed explicitly (Yes before No) so the "Yes" row is always the first
# data row under the label row, regardless of factor()'s default alphabetical
# ordering (which would otherwise put "No" first).
build_nested_pairwise_design <- function(seed = 42) {
  set.seed(seed)
  n_per_group <- 15
  tibble(
    sex       = factor(rep(c("Male", "Male", "Female", "Female"), each = n_per_group),
                        levels = c("Male", "Female")),
    age_group = factor(rep(c("Young", "Old", "Young", "Old"), each = n_per_group),
                        levels = c("Young", "Old")),
    y         = factor(c(
      sample(c("Yes", "No"), n_per_group, replace = TRUE, prob = c(0.8, 0.2)),  # Male/Young
      sample(c("Yes", "No"), n_per_group, replace = TRUE, prob = c(0.2, 0.8)),  # Male/Old
      sample(c("Yes", "No"), n_per_group, replace = TRUE, prob = c(0.2, 0.8)),  # Female/Young
      sample(c("Yes", "No"), n_per_group, replace = TRUE, prob = c(0.8, 0.2))   # Female/Old
    ), levels = c("Yes", "No")),
    wt        = rep(1, n_per_group * 4)   # unweighted, but explicit - see
                                            # test_add_pairwise_sig.R's own
                                            # convention (e.g. its test 18)
                                            # for why weights = wt is always
                                            # passed explicitly even for an
                                            # unweighted design in this project
  ) %>% srvyr::as_survey_design(ids = 1, weights = wt)
}


test_that("14. pivot_nested_crosstab() carries a legend and __sigdiff columns only when pairwise = TRUE was requested", {
  design <- build_nested_pairwise_design()

  with_pairwise <- calc_stats(design, outcomes = "y", predictors = list(c("sex", "age_group")),
                               statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)
  without_pairwise <- calc_stats(design, outcomes = "y", predictors = list(c("sex", "age_group")),
                                  statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = FALSE)

  result_with    <- pivot_nested_crosstab(with_pairwise)
  result_without <- pivot_nested_crosstab(without_pairwise)

  expect_true(any(stringr::str_detect(names(result_with[[1]]), "__sigdiff$")))
  legend_with <- result_with[[6]]
  expect_true(!is.null(legend_with) && nrow(legend_with) > 0)

  expect_false(any(stringr::str_detect(names(result_without[[1]]), "__sigdiff$")))
  expect_true(is.null(result_without[[6]]))
})


test_that("15. format_nested_crosstab() shows a legend row right under the 4-row header, a Sig. diff row, and suppresses SE rows when pairwise = TRUE", {
  design <- build_nested_pairwise_design()

  stats_table <- calc_stats(design, outcomes = "y", predictors = list(c("sex", "age_group")),
                             statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)
  pivot_result <- pivot_nested_crosstab(stats_table)
  legend       <- pivot_result[[6]]
  result       <- format_nested_crosstab(pivot_result)
  print(result %>% huxtable::as_flextable())

  variable_col <- which(names(result) == "Variable")

  # header_offset is 4 rows - the legend is the very next row (row 5),
  # exactly as pivot_crosstab()'s own legend row sits right after ITS
  # 2-row header.
  expect_equal(result[[variable_col]][5], "Column reference")

  # Letters match pivot_nested_crosstab()'s own legend lookup exactly - not
  # hardcoded, since which letter lands on which column is an implementation
  # detail of assign_sig_letters()'s row order, not asserted as a fixed fact
  # here (same restraint test 20 in test_pivot_crosstab.R uses).
  for (i in seq_len(nrow(legend))) {
    col_idx <- which(names(result) == legend$level_col[i])
    expect_equal(result[[col_idx]][5], legend$sig_letter[i])
  }

  # No SE row anywhere - pairwise = TRUE suppresses conf display entirely,
  # same rule pivot_crosstab() applies.
  expect_false("SE" %in% result[["Statistics"]])

  # At least one Sig. diff row exists, and every one sits directly under a
  # real data row ("Yes" or "No") - not asserting Yes specifically comes
  # first (a real test run showed "No" actually sorts first here despite
  # the explicit levels = c("Yes", "No") on the factor - some step upstream
  # of format_nested_crosstab() isn't preserving that declared order, not
  # something this test needs to pin down or assume either way). What
  # matters for this test is the POSITIONAL invariant - a sigdiff row is
  # always immediately below its own data row - true regardless of which
  # category happened to land first.
  data_row_positions <- which(result[["Variable"]] %in% c("Yes", "No"))
  sigdiff_rows        <- which(result[["Statistics"]] == "Sig. diff")
  expect_true(length(sigdiff_rows) >= 1)
  expect_true(all((sigdiff_rows - 1) %in% data_row_positions))
})


test_that("16. format_nested_crosstab() renders a fuller, weighted, pairwise + p-value nested table cleanly (printed for visual review)", {
  set.seed(7)
  n_per_group <- 15
  design <- tibble(
    sex       = factor(rep(c("Male", "Male", "Female", "Female"), each = n_per_group),
                        levels = c("Male", "Female")),
    age_group = factor(rep(c("Young", "Old", "Young", "Old"), each = n_per_group),
                        levels = c("Young", "Old")),
    approve   = factor(c(
      sample(c("Yes", "No"), n_per_group, replace = TRUE, prob = c(0.75, 0.25)),
      sample(c("Yes", "No"), n_per_group, replace = TRUE, prob = c(0.25, 0.75)),
      sample(c("Yes", "No"), n_per_group, replace = TRUE, prob = c(0.30, 0.70)),
      sample(c("Yes", "No"), n_per_group, replace = TRUE, prob = c(0.70, 0.30))
    ), levels = c("Yes", "No")),
    wt        = runif(n_per_group * 4, 0.7, 1.4)
  ) %>% srvyr::as_survey_design(ids = 1, weights = wt)

  stats_table <- calc_stats(design, outcomes = "approve", predictors = list(c("sex", "age_group")),
                             statistics = "w_perc", conf = "se", pval = TRUE, multicode = FALSE, pairwise = TRUE)
  result <- format_nested_crosstab(pivot_nested_crosstab(stats_table))
  print(result %>% huxtable::as_flextable())

  expect_true("Column reference" %in% result[["Variable"]])
  expect_false("SE" %in% result[["Statistics"]])
  expect_true(any(!is.na(result[["Statistics"]]) & result[["Statistics"]] == "Sig. diff"))
})


# Tests 17-19: the same nested pairwise mechanism (tests 14/15/16 above), but
# for a nested w_mean statistic instead of w_perc - added once
# weighted_mean_svyby() gave calc_stats(pairwise = TRUE) a real covariance
# for nested means too. Not expected to need any CODE changes in
# pivot_nested_crosstab.R - reshape_nested_predictor_set()'s sig_diff/
# sig_letter handling is explicitly "ported from pivot_crosstab.R... not
# re-derived here" (see that function's own comment) and is just as
# column-presence-driven as the flat version. These tests confirm that
# empirically, same reasoning as tests 22-24 in test_pivot_crosstab.R.
#
# Reuses the exact Male/Young=10,20 / Male/Old=30,40 / Female/Young=50,60 /
# Female/Old=70,80 shape from test 12 in test_weighted_mean_svyby.R - already
# confirmed there (against real R output) to give a real, non-trivial SE
# (~3.78) and clearly-separated group means (15/35/55/75), so pairwise
# differences here are genuinely significant, not just structurally present.
build_nested_mean_design <- function() {
  data <- tibble(
    score     = c(10, 20, 30, 40, 50, 60, 70, 80),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old")),
    wt        = rep(1, 8)
  )
  data %>% srvyr::as_survey_design(ids = 1, weights = wt)
}

test_that("17. pivot_nested_crosstab() carries a legend and __sigdiff columns for a nested w_mean statistic under calc_stats(pairwise = TRUE)", {
  design <- build_nested_mean_design()

  with_pairwise <- calc_stats(design, outcomes = "score", predictors = list(c("sex", "age_group")),
                               statistics = "w_mean", conf = "se", multicode = FALSE, pairwise = TRUE)
  without_pairwise <- calc_stats(design, outcomes = "score", predictors = list(c("sex", "age_group")),
                                  statistics = "w_mean", conf = "se", multicode = FALSE, pairwise = FALSE)

  result_with    <- pivot_nested_crosstab(with_pairwise)
  result_without <- pivot_nested_crosstab(without_pairwise)

  expect_true(any(stringr::str_detect(names(result_with[[1]]), "__sigdiff$")))
  legend_with <- result_with[[6]]
  expect_true(!is.null(legend_with) && nrow(legend_with) > 0)

  expect_false(any(stringr::str_detect(names(result_without[[1]]), "__sigdiff$")))
  expect_true(is.null(result_without[[6]]))
})


test_that("18. format_nested_crosstab() shows a legend row, a Sig. diff row, and suppresses SE for a nested w_mean statistic", {
  design <- build_nested_mean_design()

  stats_table <- calc_stats(design, outcomes = "score", predictors = list(c("sex", "age_group")),
                             statistics = "w_mean", conf = "se", multicode = FALSE, pairwise = TRUE)
  pivot_result <- pivot_nested_crosstab(stats_table)
  legend       <- pivot_result[[6]]
  result       <- format_nested_crosstab(pivot_result)
  print(result %>% huxtable::as_flextable())

  variable_col <- which(names(result) == "Variable")

  # header_offset is 4 rows - the legend is the very next row (row 5), same
  # as test 15's own check.
  expect_equal(result[[variable_col]][5], "Column reference")

  for (i in seq_len(nrow(legend))) {
    col_idx <- which(names(result) == legend$level_col[i])
    expect_equal(result[[col_idx]][5], legend$sig_letter[i])
  }

  expect_false("SE" %in% result[["Statistics"]])

  # Numeric variable: "score" is its own data row's Variable text (no label
  # row, unlike test 15's categorical "Yes"/"No") - a single data row here
  # (one statistic requested), so the Sig. diff row (if any pair differs)
  # sits directly under it. The relocated base row ALSO reads Variable ==
  # "score" (same reasoning as test 23's own fix in test_pivot_crosstab.R -
  # format_nested_crosstab()'s Variable+Statistics merge_cells() on the base
  # row copies that same text into Statistics too), so disambiguated the
  # same way: Statistics == "Mean (w)" is only ever true on the real data
  # row.
  score_row    <- which(result[["Variable"]] == "score" & result[["Statistics"]] == "Mean (w)")
  sigdiff_rows <- which(result[["Statistics"]] == "Sig. diff")
  expect_true(length(sigdiff_rows) >= 1)
  expect_true(all((sigdiff_rows - 1) %in% score_row))
})


test_that("19. format_nested_crosstab() renders a fuller, weighted, pairwise + p-value nested w_mean table cleanly (printed for visual review)", {
  set.seed(9)
  n_per_group <- 15
  design <- tibble(
    sex       = factor(rep(c("Male", "Male", "Female", "Female"), each = n_per_group),
                        levels = c("Male", "Female")),
    age_group = factor(rep(c("Young", "Old", "Young", "Old"), each = n_per_group),
                        levels = c("Young", "Old")),
    score     = c(rnorm(n_per_group, 80, 6), rnorm(n_per_group, 60, 6),
                  rnorm(n_per_group, 40, 6), rnorm(n_per_group, 20, 6)),
    wt        = runif(n_per_group * 4, 0.7, 1.4)
  ) %>% srvyr::as_survey_design(ids = 1, weights = wt)

  stats_table <- calc_stats(design, outcomes = "score", predictors = list(c("sex", "age_group")),
                             statistics = "w_mean", conf = "se", pval = TRUE, multicode = FALSE, pairwise = TRUE)
  result <- format_nested_crosstab(pivot_nested_crosstab(stats_table))
  print(result %>% huxtable::as_flextable())

  expect_true("Column reference" %in% result[["Variable"]])
  expect_false("SE" %in% result[["Statistics"]])
  expect_true(any(!is.na(result[["Statistics"]]) & result[["Statistics"]] == "Sig. diff"))
})


test_that("20. format_nested_crosstab() still inserts a (blank) Sig. diff row when a row WAS pairwise-tested but nothing came out significant - regression test for a real bug, mirroring test 25 in test-pivot_crosstab.R", {
  # All 4 (sex, age_group) combinations get an IDENTICAL 50/50 Yes/No split -
  # every comparison is genuinely tested (eligible stat, real predictor
  # levels) and genuinely finds nothing significant, so sig_diff should be
  # "" (tested, nothing found), never NA (never tested), for every real
  # level. Same underlying bug as pivot_crosstab.R's format_crosstab(): this
  # file's format_nested_crosstab() ported the identical "only insert the
  # row if at least one REAL, non-empty difference exists" gate, which made
  # a genuinely-tested-but-null result silently produce no Sig. diff row at
  # all - indistinguishable from "pairwise wasn't requested". Fixed the same
  # way: key off "was this row tested" (any non-NA), not "did it find
  # anything".
  # n_per_group even and y built as the SAME 16-row Yes/No/Yes/No.../No
  # block repeated identically for all 4 (sex, age_group) combinations
  # (rather than one continuous rep() spanning block boundaries, which -
  # caught while chasing this test's first failure below - doesn't actually
  # give each block an identical split once the block size is odd and the
  # pattern phase drifts across the boundary). Every group gets EXACTLY the
  # same 8 Yes / 8 No, in the same order, so estimate is exactly equal
  # across every pair of groups - z = (estimate_i - estimate_j)/se_diff is
  # exactly 0 regardless of what se_diff itself comes out to, so nothing
  # can cross the significance threshold by construction, not by luck.
  n_per_group <- 16
  design <- tibble(
    sex       = factor(rep(c("Male", "Male", "Female", "Female"), each = n_per_group),
                        levels = c("Male", "Female")),
    age_group = factor(rep(c("Young", "Old", "Young", "Old"), each = n_per_group),
                        levels = c("Young", "Old")),
    y         = factor(rep(rep(c("Yes", "No"), n_per_group / 2), 4), levels = c("Yes", "No")),
    wt        = rep(1, n_per_group * 4)
  ) %>% srvyr::as_survey_design(ids = 1, weights = wt)

  stats_table <- calc_stats(design, outcomes = "y", predictors = list(c("sex", "age_group")),
                             statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)

  # Confirm the premise: genuinely tested, genuinely null - restricted to
  # the 4 real nested combinations (cross_break != "Total"). calc_stats()
  # also always computes an unconditional overall Total row alongside any
  # predictor breakdown, nested or flat, and add_pairwise_sig() correctly
  # leaves ITS sig_diff as NA (nothing to compare a lone overall estimate
  # against) - including that row in this premise check isn't testing the
  # scenario this test is actually about, and was the first version's own
  # mistake, not a real problem with the fix itself.
  yes_rows <- stats_table %>% filter(o_cat == "Yes", cross_break != "Total")
  expect_true(all(!is.na(yes_rows$sig_diff)))
  expect_true(all(yes_rows$sig_diff == ""))

  result <- format_nested_crosstab(pivot_nested_crosstab(stats_table))
  print(result %>% huxtable::as_flextable())

  expect_true("Sig. diff" %in% result[["Statistics"]])

  # Same positional invariant test 15 already checks - every Sig. diff row
  # sits directly under its own data row - plus, new to this test, that
  # those rows are actually blank (not "-", not a real letter) on every
  # level column, confirming this is the "tested, nothing found" case
  # specifically, not some other path.
  data_row_positions <- which(result[["Variable"]] %in% c("Yes", "No"))
  sigdiff_rows        <- which(result[["Statistics"]] == "Sig. diff")
  expect_true(length(sigdiff_rows) >= 1)
  expect_true(all((sigdiff_rows - 1) %in% data_row_positions))

  level_cols <- setdiff(names(result), c("Variable", "Statistics"))
  level_cols <- level_cols[!grepl("^p_value", level_cols)]
  for (r in sigdiff_rows) {
    expect_true(all(result[level_cols][r, ] == "" | is.na(result[level_cols][r, ])))
  }
})

