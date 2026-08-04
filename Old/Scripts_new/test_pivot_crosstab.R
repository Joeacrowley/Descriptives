# =============================================================================
# Tests for pivot_crosstab.R
#
# I haven't run these myself - same caveat as the other test files in this
# project. Run with:
#
#   testthat::test_file(here::here("Scripts_new", "test_pivot_crosstab.R"))
#
# Tests 1-2 call calc_stats() for real (categorical, then numeric), single
# predictor set. Test 3 covers the numeric multi-statistic + p-value-shown-
# once case, the trickiest piece of grouping logic in this file (cumsum()
# across a block that includes a label row for categorical variables, or
# spans two statistic blocks for a multi-stat numeric variable). Test 4
# checks the (now only remaining) predictor-set validation - two predictor
# sets sharing the same label. Test 5 is the other validation-error test.
#
# Tests 7-8 cover multiple predictor sets side by side - the main addition
# that pass - for a categorical and a numeric variable respectively. These
# are the two I'd re-check most carefully: the join that combines each
# set's reshaped table back together is the part of this file I'm least
# able to verify without running it myself (see the header note in
# pivot_crosstab.R on why (outcome, o_lab, o_cat, stat) should be a safe
# join key, and check that reasoning against what actually comes out).
#
# Tests 6 and 9 cover multi-code p-values now actually working, rather than
# being rejected: each level of a multicoded variable keeps its own
# independent p_value (not blanked after the first row the way an ordinary
# categorical/numeric variable's shared p-value is) - see the header note in
# pivot_crosstab.R for why that falls straight out of how calc_stats()
# computes p-values pre-collapse. Test 6 uses a hand-built calc_stats()-
# shaped input with outcome_type = "multicoded" and two options carrying
# deliberately different p-values, so the test can assert on exact values.
# Test 9 backs that up with a real calc_stats(multicode=TRUE, pval=TRUE)
# call (the same Q1_OptionA/B/C + sex construction as test_calc_stats.R's
# test 35), confirming this works end-to-end on genuine convert_multicodes()
# output, not just a hand-set flag - checked structurally there (non-NA,
# in [0,1]) since three independent chi-square p-values aren't hand-
# calculable.
#
# Test 15 (at the end) is a small regression check that stat_type_of() was
# actually extended for the new numeric stats added to calc_stats.R (min/
# max/range/iqr/sd) - mean+sd now sharing one p-value, same mechanism as
# test 3's mean+median.
#
# Tests 10-14 cover format_crosstab(), same access patterns as format_
# summary()'s tests in test_pivot_summary.R (stored cell values via [["col"]]
# rather than printed/na_string text, huxtable accessor functions like
# rowspan()/bold()/background_color() rather than checking by eye). Test 10
# is format_pvalue() directly. Test 11 covers the spanning header (checked
# via colspan(), which I'm less sure of than rowspan() - I haven't been able
# to confirm colspan() is the right accessor name without R to check against)
# and the base-row count formatting bug self-checking caught (base row
# values were going through format_statistic()'s stat_code-keyed rule,
# which produced "6.000000" instead of "6" since a base row's stat_code is
# NA - fixed by branching base rows to the same digits=0/comma-separated
# rule pivot_summary() uses for its Base column). Test 12 covers the new
# "SE as an inserted row" mechanic and its interaction with the Variable-
# text merge (a numeric variable's Mean row and its own inserted SE row
# merge into one "age" cell, same as Mean+Median would without any conf rows
# involved - checked via rowspan()). Test 13 covers a multi-statistic
# numeric variable with p-values but no conf, to check that mechanic in
# isolation from the row-insertion one. Test 14 combines everything at once
# (two predictor sets, two statistics, SE rows, two p-value columns).
#
# Tests 16-17 (after test 15) cover the Base-relocation feature ported over
# from pivot_nested_crosstab()/format_nested_crosstab(): every variable's
# Base row moved into one consolidated "Sample sizes" section at the bottom,
# Statistics blanked on individual base rows (the section header conveys
# "Base" once for the whole section instead), and Variable+Statistics merged
# into one wide cell in that section. Ported together with a header-
# alignment fix for the same reason it mattered in the nested table: a
# spanning header row was being explicitly centered, then immediately
# overwritten back to right-aligned by a later "everything else is right-
# aligned" call scoped too broadly ("everywhere" instead of header_offset
# onward). Porting the relocation feature here needed two follow-on changes
# that weren't optional the way they might look on their own: o_lab is no
# longer dropped from pivot_crosstab()'s output (format_crosstab() needs it,
# since block-boundary detection can no longer rely on "a data/label row
# right after a base row" once every base row moves to the bottom - the
# proxy this file used before), and format_crosstab()'s new_block is
# rebuilt from o_lab accordingly, the same fix already made once for the
# nested table. Every pre-existing test in this file that touched Base rows
# needed updating alongside this - see individual test comments for what
# changed and why (row counts +1 for the new section header, Statistics ==
# "Base" lookups replaced with either row_type == "base" at the pivot level
# or the "Sample sizes" header text at the format level since row_type isn't
# a huxtable column, and Variable text on base rows now reads that
# variable's own label instead of "").
# =============================================================================

library(testthat)
library(tidyverse)
library(huxtable)
source(here::here("Scripts_new", "calc_stats.R"))
source(here::here("Scripts_new", "pivot_summary.R"))
source(here::here("Scripts_new", "pivot_crosstab.R"))

test_that("1. categorical variable x single predictor, no conf, no p-value requested", {

  # Male (rows 1-3): North, North, South -> North = 2/3, South = 1/3, base = 3
  # Female (rows 4-6): South, South, North -> South = 2/3, North = 1/3, base = 3
  # Total (all 6): North = 3/6 = 0.5, South = 3/6 = 0.5, base = 6
  data <- tibble(
    region = factor(c("North", "North", "South", "South", "South", "North")),
    sex    = factor(c("Male", "Male", "Male", "Female", "Female", "Female"))
  )
  stats_table <- calc_stats(data, outcomes = "region", predictors = "sex",
                             statistics = "perc", multicode = FALSE)
  pivot_result <- pivot_crosstab(stats_table)
  result <- pivot_result[[1]]
  print(result)

  # 5 rows, not 4 - Base is no longer inline right after South. It's
  # relocated into its own consolidated "Sample sizes" section at the
  # bottom (Joe's request, same feature already built for the nested
  # table): one section-header row (row_type == "label", Variable ==
  # "Sample sizes"), then one row per variable - here just "region" - each
  # carrying that variable's OWN label in Variable (not blank the way an
  # inline Base row used to read), since it's no longer sitting directly
  # under that variable's own data to make the association obvious.
  expect_equal(nrow(result), 5)
  expect_equal(result$Variable, c("region", "North", "South", "Sample sizes", "region"))
  # Statistics reads NA on the relocated base row too, not "Base" - the
  # section header conveys that once for the whole section, so repeating it
  # on every row was redundant.
  expect_equal(result$Statistics, c(NA, "%", "%", NA, NA))

  expect_equal(result$Total,  c(NA, 0.5, 0.5, NA, 6))
  expect_equal(result$Male,   c(NA, 2/3, 1/3, NA, 3))
  expect_equal(result$Female, c(NA, 1/3, 2/3, NA, 3))

  # outcome/o_cat are join scaffolding, not part of the returned table -
  # same as pivot_summary(), which never puts them in its own final
  # select() either. o_cat in particular used to leak a wrong value onto the
  # label row (a leftover copy of the first level's o_cat, "North", instead
  # of being blank or absent); dropping the column entirely removes that
  # whole bug category rather than just patching the one row type that
  # exposed it.
  #
  # o_lab, unlike outcome/o_cat, IS kept now (a change from this function's
  # earlier behaviour) - format_crosstab() needs it for block-boundary
  # detection once Base rows no longer reliably sit right under their own
  # variable's data (see pivot_crosstab()'s own comment on this).
  expect_false(any(c("outcome", "o_cat") %in% names(result)))
  expect_true("o_lab" %in% names(result))

  # stat_code had the same leftover-copy issue (a stray "perc" on the label
  # row) - this one IS kept in the returned table (format_crosstab() will
  # need it later for digit formatting, same as stat_code in pivot_summary()/
  # format_summary()), so it's fixed by blanking rather than dropping.
  expect_true(is.na(result$stat_code[1]))
  expect_equal(result$stat_code[2:3], c("perc", "perc"))

  # outcome_type is also kept, for format_crosstab()'s eventual bolding
  # logic (numeric variable row vs categorical level row - a distinction
  # row_type alone can't make) - and it's genuinely constant across the
  # whole block, label and base row included, unlike o_cat/stat_code.
  expect_true("outcome_type" %in% names(result))
  expect_true(all(result$outcome_type == "categorical"))

  # Metadata columns are grouped in one block at the very end of the table -
  # 4 now, not 3, with o_lab joining the group for the reason above.
  expect_equal(tail(names(result), 4), c("row_type", "stat_code", "outcome_type", "o_lab"))

  # No conf requested -> no "__conf" columns at all.
  expect_false(any(str_detect(names(result), "__conf")))
  # No p-value requested -> no "p_value (...)" column at all.
  expect_false(any(str_detect(names(result), "p_value")))

  # predictor_sets (element 4) is a named vector - cross_break as names,
  # p_lab as values - needed by format_crosstab() for spanning headers even
  # in this single-set case, where level columns stay unprefixed ("Male"/
  # "Female") and there'd otherwise be nowhere to recover "sex" from.
  expect_equal(pivot_result[[4]], c(sex = "sex"))
  # No conf requested -> conf_type (element 5) is NA.
  expect_true(is.na(pivot_result[[5]]))
})

test_that("2. numeric variable x single predictor, with SE", {

  # Male (10,20,30): mean = 20, sd = 10, se = 10/sqrt(3) = 5.773503
  # Female (40,50,60): mean = 50, sd = 10, se = 5.773503
  # Total (10..60): mean = 35, sd = 18.708287, se = 18.708287/sqrt(6) = 7.637626
  data <- tibble(
    age = c(10, 20, 30, 40, 50, 60),
    sex = factor(c("Male", "Male", "Male", "Female", "Female", "Female"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = "sex",
                             statistics = "mean", conf = "se", multicode = FALSE)
  pivot_result <- pivot_crosstab(stats_table)
  result <- pivot_result[[1]]
  print(result)

  # 3 rows, not 2 - no label row for a numeric variable, but the relocated
  # Base section still adds its own header row on top of the base row
  # itself (same relocation feature as test 1 - see its comment).
  expect_equal(nrow(result), 3)
  expect_equal(result$Variable, c("age", "Sample sizes", "age"))
  expect_equal(result$Statistics, c("Mean", NA, NA))

  expect_equal(result$Total,  c(35, NA, 6))
  expect_equal(result$Male,   c(20, NA, 3))
  expect_equal(result$Female, c(50, NA, 3))

  expect_equal(result$Total__conf[1],  7.637626, tolerance = 1e-5)
  expect_equal(result$Male__conf[1],   5.773503, tolerance = 1e-5)
  expect_equal(result$Female__conf[1], 5.773503, tolerance = 1e-5)

  # Neither the section header nor the actual base row has an SE of its
  # own - the header because it's explicitly blanked (see pivot_crosstab()'s
  # relocation comment), the base row because bind_rows() fills the missing
  # "__conf" columns with NA (base_wide never carried them to begin with).
  expect_true(is.na(result$Total__conf[2]))
  expect_true(is.na(result$Total__conf[3]))

  # conf_type (element 5) records which kind of value is in "__conf" -
  # format_crosstab() needs this to know whether to run it through
  # format_statistic() (a raw SE, like here) or format_ci_string() (a CI
  # range string) - the column name alone ("Total__conf") can't tell it
  # apart, unlike pivot_summary(), which names the column "SE" or "95% CI"
  # directly.
  expect_equal(pivot_result[[5]], "se")
})

test_that("3. numeric variable with two statistics: p-value shown once, on the first (Mean) row", {

  data <- tibble(
    age = c(10, 20, 30, 40, 50, 60),
    sex = factor(c("Male", "Male", "Male", "Female", "Female", "Female"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = "sex",
                             statistics = c("mean", "median"), pval = TRUE, multicode = FALSE)
  result <- pivot_crosstab(stats_table)[[1]]
  print(result)

  # 4 rows, not 3 - Mean + Median + the relocated section's own header row +
  # the actual Base row (see test 1's comment on the relocation feature).
  expect_equal(nrow(result), 4)
  expect_equal(result$Variable, c("age", "age", "Sample sizes", "age"))
  expect_equal(result$Statistics, c("Mean", "Median", NA, NA))

  pval_col <- "p_value (sex)"
  expect_true(pval_col %in% names(result))

  # Shown once, on the Mean row (the first data row of this variable's
  # num_u "test type" block, which mean and median both belong to since
  # they come from the same significance test) - not on Median, not on the
  # section header, not on Base.
  expect_false(is.na(result[[pval_col]][1]))
  expect_true(result[[pval_col]][1] >= 0 && result[[pval_col]][1] <= 1)
  expect_true(is.na(result[[pval_col]][2]))
  expect_true(is.na(result[[pval_col]][3]))
  expect_true(is.na(result[[pval_col]][4]))
})

test_that("4. errors when two predictor sets share the same label", {
  data <- tibble(
    age = c(10, 20, 30, 40),
    g1  = factor(c("A", "A", "B", "B")),
    g2  = factor(c("X", "X", "Y", "Y"))
  )
  attr(data$g1, "label") <- "Group"
  attr(data$g2, "label") <- "Group"   # same label text as g1, on purpose

  stats_table <- calc_stats(data, outcomes = "age", predictors = list("g1", "g2"),
                             statistics = "mean", multicode = FALSE)

  expect_error(pivot_crosstab(stats_table), "distinct labels")
})

test_that("5. errors when a categorical variable has more than one statistic requested", {
  data <- tibble(
    region = factor(c("North", "North", "South", "South", "South", "North")),
    sex    = factor(c("Male", "Male", "Male", "Female", "Female", "Female"))
  )
  stats_table <- calc_stats(data, outcomes = "region", predictors = "sex",
                             statistics = c("perc", "count"), multicode = FALSE)

  expect_error(pivot_crosstab(stats_table), "one statistic per categorical variable")
})

test_that("6. a multi-code variable (outcome_type == 'multicoded') keeps its own p-value on every level, not just the first", {

  # Hand-built rather than via calc_stats() - simpler to set outcome_type =
  # "multicoded" directly, and it lets p_value be assigned by hand so the
  # test can check exact numbers rather than just "non-NA". Two options
  # ("Option A"/"Option B") for a Q1 multicode variable, broken down by sex,
  # each carrying its OWN p_value (0.02 / 0.15) - deliberately different, so
  # a bug that collapsed them to one shared value (or blanked the second)
  # would be caught. Same on both the Male and Female rows for a given
  # option, matching how calc_stats() actually joins p_value (constant
  # across p_cat1 for one outcome/predictor pair - see run_assoc_test() in
  # calc_stats.R).
  fake_data <- tibble(
    cross_break      = c("Total", "Total", "sex", "sex", "sex", "sex"),
    outcome          = "Q1_Option", o_lab = "Q1",
    o_cat            = c("Option A", "Option B", "Option A", "Option B", "Option A", "Option B"),
    stat             = "perc",
    outcome_type     = "multicoded",
    estimate         = c(0.5, 0.5, 0.6, 0.4, 0.4, 0.6),
    base             = c(6, 6, 3, 3, 3, 3),
    base_description = NA_character_,
    p_lab1           = c("Total", "Total", "sex", "sex", "sex", "sex"),
    p_cat1           = c("Total", "Total", "Male", "Male", "Female", "Female"),
    p_value          = c(NA, NA, 0.02, 0.15, 0.02, 0.15)
  )

  result <- pivot_crosstab(fake_data)[[1]]
  print(result)

  # 5 rows, not 4 - label + Option A + Option B + the relocated section's
  # own header row + the actual Base row (see test 1's comment).
  expect_equal(nrow(result), 5)
  expect_equal(result$Variable, c("Q1", "Option A", "Option B", "Sample sizes", "Q1"))

  # outcome/o_cat are dropped entirely from the returned table (see test 1's
  # comment) - o_cat in particular used to leak a copy of Option A's o_cat
  # onto the label row, which mattered here specifically: once the p-value
  # join started keying on (outcome, o_cat, stat_type), that stale copy
  # would have quietly matched the label row to Option A's p-value (0.02),
  # relying entirely on the row_type != "data" check to blank it back out
  # again. Dropping the column removes the risk outright.
  #
  # o_lab IS kept now, unlike outcome/o_cat - see test 1's comment on why.
  expect_false(any(c("outcome", "o_cat") %in% names(result)))
  expect_true("o_lab" %in% names(result))

  # stat_code had the same leftover-copy issue (a stray "perc") but is kept
  # in the table (format_crosstab() will need it later), so it's fixed by
  # blanking rather than dropping. Variable == "Q1" & row_type == "label"
  # together, not either alone - Variable alone catches the label row AND
  # the relocated Base row (which now deliberately carries that SAME
  # Variable text - see pivot_crosstab()'s relocation comment), while
  # row_type == "label" alone catches the label row AND the relocated
  # section's own "Sample sizes" header row (also row_type == "label" - the
  # actual bug this combination was needed to fix, caught by a real test
  # run: "actual: TRUE TRUE" against "expected: TRUE" - two rows matched,
  # not one). Only the intersection of both conditions is unique to the
  # real label row.
  expect_true(is.na(result$stat_code[result$Variable == "Q1" & result$row_type == "label"]))

  pval_col <- "p_value (sex)"
  expect_true(pval_col %in% names(result))

  # Both option rows keep their own value - neither blanked, neither
  # overwritten with the other's. This is the behaviour that actually
  # differs from an ordinary categorical variable (test 1/7), where only
  # the first data row would show a p-value.
  expect_equal(result[[pval_col]][result$Variable == "Option A"], 0.02)
  expect_equal(result[[pval_col]][result$Variable == "Option B"], 0.15)

  # Label row and Base row never show a p-value, multicoded or not -
  # Variable == "Q1" & row_type == "label" disambiguates the same way as
  # the stat_code check above (row_type == "label" alone would also match
  # the relocated section's "Sample sizes" header row); row_type == "base"
  # alone is fine on its own (only one row has it), replacing the old
  # Variable == "" check now that the relocated base row's Variable is no
  # longer blank.
  expect_true(is.na(result[[pval_col]][result$Variable == "Q1" & result$row_type == "label"]))
  expect_true(is.na(result[[pval_col]][result$row_type == "base"]))

  # outcome_type IS kept (unlike outcome/o_cat) - format_crosstab() will
  # need it to bold a numeric variable's own row without bolding a
  # categorical/multicoded level row, a distinction row_type can't make on
  # its own. Constant across the whole block, including the label row - no
  # "leftover from row 1" issue the way o_cat/stat_code had.
  expect_true("outcome_type" %in% names(result))
  expect_true(all(result$outcome_type == "multicoded"))

  # Metadata columns (row_type, stat_code, outcome_type, o_lab) are grouped
  # in one block at the very end of the table, not scattered among the data
  # columns. 4 now, not 3, with o_lab joining the group.
  meta_cols <- c("row_type", "stat_code", "outcome_type", "o_lab")
  expect_equal(tail(names(result), 4), meta_cols)
})

test_that("7. categorical variable x two predictor sets side by side", {

  # region assigned so the Sex and AgeGroup breakdowns give different but
  # both hand-calculable splits:
  #   sex:       Male (1,2,3): North,North,South -> North=2/3, South=1/3, base=3
  #              Female (4,5,6): South,South,North -> South=2/3, North=1/3, base=3
  #   age_group: Young (1,2,5): North,North,South -> North=2/3, South=1/3, base=3
  #              Old (3,4,6):   South,South,North -> South=2/3, North=1/3, base=3
  #   Total (all 6): North=South=0.5, base=6
  data <- tibble(
    region    = factor(c("North", "North", "South", "South", "South", "North")),
    sex       = factor(c("Male", "Male", "Male", "Female", "Female", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "region", predictors = list("sex", "age_group"),
                             statistics = "perc", multicode = FALSE)
  pivot_result <- pivot_crosstab(stats_table)
  result <- pivot_result[[1]]
  print(result)

  # 5 rows, not 4 - the relocated Base section adds its own header row on
  # top of the actual base row (see test 1's comment on this feature).
  expect_equal(nrow(result), 5)

  # predictor_sets (element 4) carries both sets' labels, in order -
  # format_crosstab() will need this to build a spanning header per set.
  expect_equal(pivot_result[[4]], c(sex = "sex", age_group = "age_group"))

  # More than one predictor set present -> level columns get the
  # "<predictor label>: <level>" prefix (see reshape_one_predictor_set()).
  # Factor level order is alphabetical by default (Female before Male,
  # Old before Young), same reasoning as the earlier ordering discussion.
  expected_cols <- c("Total", "sex: Female", "sex: Male", "age_group: Old", "age_group: Young")
  expect_true(all(expected_cols %in% names(result)))
  # "Total" is merged into the first predictor set only, not repeated.
  expect_equal(sum(names(result) == "Total"), 1)

  north_row <- which(result$Variable == "North")
  south_row <- which(result$Variable == "South")
  # row_type == "base", not Statistics == "Base" - Statistics is blanked
  # (NA) on the relocated base row now that the section header conveys
  # "Base" once for the whole section (see test 1's comment).
  base_row  <- which(result$row_type == "base")

  expect_equal(result$Total[north_row], 0.5)
  expect_equal(result[["sex: Male"]][north_row],   2/3)
  expect_equal(result[["sex: Female"]][north_row], 1/3)
  expect_equal(result[["age_group: Young"]][north_row], 2/3)
  expect_equal(result[["age_group: Old"]][north_row],   1/3)

  expect_equal(result$Total[south_row], 0.5)
  expect_equal(result[["sex: Male"]][south_row],   1/3)
  expect_equal(result[["sex: Female"]][south_row], 2/3)
  expect_equal(result[["age_group: Young"]][south_row], 1/3)
  expect_equal(result[["age_group: Old"]][south_row],   2/3)

  expect_equal(result$Total[base_row], 6)
  expect_true(all(unlist(result[base_row, expected_cols[-1]]) == 3))
})

test_that("8. numeric variable x two predictor sets side by side, with distinct p-values", {

  # sex:       Male (10,20,30) mean=20; Female (40,50,60) mean=50
  # age_group: Young (rows 1,2,5 -> ages 10,20,50) mean = 80/3 = 26.6667
  #            Old   (rows 3,4,6 -> ages 30,40,60) mean = 130/3 = 43.3333
  # Total (10..60): mean = 35
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60),
    sex       = factor(c("Male", "Male", "Male", "Female", "Female", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = list("sex", "age_group"),
                             statistics = "mean", pval = TRUE, multicode = FALSE)
  result <- pivot_crosstab(stats_table)[[1]]
  print(result)

  # 3 rows, not 2 - Mean row + the relocated section's own header row + the
  # actual Base row, no label row for numeric (see test 1's comment).
  expect_equal(nrow(result), 3)

  expect_equal(result$Total[1],  35)
  expect_equal(result[["sex: Male"]][1],   20)
  expect_equal(result[["sex: Female"]][1], 50)
  expect_equal(result[["age_group: Young"]][1], 80 / 3,  tolerance = 1e-6)
  expect_equal(result[["age_group: Old"]][1],   130 / 3, tolerance = 1e-6)

  expect_true(all(c("sex: Male", "sex: Female", "age_group: Young", "age_group: Old") %in% names(result)))

  # Two distinct predictor sets -> two distinct p-value columns, both
  # present and both non-NA on the (only) Mean row.
  expect_true("p_value (sex)" %in% names(result))
  expect_true("p_value (age_group)" %in% names(result))
  expect_false(is.na(result[["p_value (sex)"]][1]))
  expect_false(is.na(result[["p_value (age_group)"]][1]))
  expect_true(result[["p_value (sex)"]][1] >= 0 && result[["p_value (sex)"]][1] <= 1)
  expect_true(result[["p_value (age_group)"]][1] >= 0 && result[["p_value (age_group)"]][1] <= 1)

  # Metadata block still lands at the very end even with two p-value
  # columns appended by two separate loop passes. 4 now, not 3 - o_lab
  # joins the group (see test 1's comment on why it's kept).
  expect_equal(tail(names(result), 4), c("row_type", "stat_code", "outcome_type", "o_lab"))
  expect_true(all(result$outcome_type == "numeric"))

  # Each set's p-value column sits right after that set's OWN level
  # columns, not clustered together after every set's columns - "p-value
  # (sex)" reads right after sex's last level column ("sex: Male", the last
  # of that set's level_order), before age_group's columns even start.
  expect_equal(names(result)[which(names(result) == "sex: Male") + 1], "p_value (sex)")
  expect_equal(names(result)[which(names(result) == "age_group: Young") + 1], "p_value (age_group)")
})

test_that("9. a real multi-code variable end-to-end keeps a per-level p_value (calc_stats(multicode=TRUE, pval=TRUE) -> pivot_crosstab())", {

  # Same Q1_OptionA/B/C + sex construction as test_calc_stats.R's test 35,
  # extended to 8 rows (4 Male, 4 Female) and pval = TRUE - test 35 itself
  # never requested p-values, so this is the first real (non-hand-built)
  # check of the mechanics test 6 checks by hand. Options are given visibly
  # different Yes/No splits by sex on purpose (A: Male 3/4, Female 1/4; C:
  # Male 1/4, Female 3/4; B: even 2/4 both) so this isn't a degenerate
  # all-identical case - but deliberately NOT 0/4 or 4/4 for any option/sex
  # combination. A first version of this test used a 4/4-vs-0/4 split and
  # that broke: when a subgroup has literally zero "Yes" rows for an option,
  # unweighted_perc() never emits a "Yes" row for that (option, sex) cell at
  # all (group_by() only produces rows for combinations actually present in
  # the data), so pivot_wider() fills that cell with a real NA rather than
  # 0 - correct behaviour for calc_stats() in general, but it would have
  # masked genuine estimate/conf columns as missing in a test whose actual
  # point is the p-value columns. Keeping every subgroup strictly between 0
  # and 4 out of 4 avoids that here. The actual chi-square p-values aren't
  # hand-calculable, so this checks structure (present, in range, not
  # collapsed) rather than exact numbers. Also doubles as regression
  # coverage for the convert_multicodes() "crossbreak"/"cross_break" typo
  # fix documented in test 35's comment.
  data <- tibble(
    Q1_OptionA = factor(c("Yes", "Yes", "Yes", "No",  "No",  "No",  "No",  "Yes")),
    Q1_OptionB = factor(c("Yes", "No",  "Yes", "No",  "Yes", "No",  "Yes", "No")),
    Q1_OptionC = factor(c("No",  "No",  "No",  "Yes", "Yes", "Yes", "Yes", "No")),
    sex        = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"))
  )
  attr(data$Q1_OptionA, "label") <- "Q1: Option A"
  attr(data$Q1_OptionB, "label") <- "Q1: Option B"
  attr(data$Q1_OptionC, "label") <- "Q1: Option C"

  stats_table <- calc_stats(data, outcomes = c("Q1_OptionA", "Q1_OptionB", "Q1_OptionC"),
                             predictors = "sex", statistics = "perc", pval = TRUE, multicode = TRUE)

  expect_equal(unique(stats_table$outcome_type), "multicoded")   # sanity check before the real assertion

  result <- pivot_crosstab(stats_table)[[1]]
  print(result)

  # 6 rows, not 5 - label + 3 options + the relocated section's own header
  # row + the actual Base row (see test 1's comment).
  expect_equal(nrow(result), 6)
  expect_equal(result$Variable, c("Q1", "Option A", "Option B", "Option C", "Sample sizes", "Q1"))

  # Regression check (same fix as test 6, here via the real pipeline):
  # outcome/o_cat are absent from the table entirely (o_lab IS kept now -
  # see test 1's comment), and the label row's stat_code is NA rather than
  # a leftover copy of Option A's "perc". Variable == "Q1" & row_type ==
  # "label" together, not either alone - Variable alone also catches the
  # relocated Base row (same text now), and row_type == "label" alone also
  # catches the relocated section's own "Sample sizes" header row (also
  # row_type == "label" - the bug test 6 actually caught on a real run, see
  # its comment for the exact failure).
  expect_false(any(c("outcome", "o_cat") %in% names(result)))
  expect_true("o_lab" %in% names(result))
  expect_true(is.na(result$stat_code[result$Variable == "Q1" & result$row_type == "label"]))

  # Guards against the degenerate-split issue described above: every
  # option's estimate should be a real proportion in every column, never NA.
  option_rows_idx <- which(result$Variable %in% c("Option A", "Option B", "Option C"))
  expect_true(all(!is.na(unlist(result[option_rows_idx, c("Total", "Male", "Female")]))))
  expect_equal(result$Total[option_rows_idx], c(0.5, 0.5, 0.5))
  expect_equal(result$Male[option_rows_idx],   c(0.75, 0.5, 0.25))
  expect_equal(result$Female[option_rows_idx], c(0.25, 0.5, 0.75))

  pval_col <- "p_value (sex)"
  expect_true(pval_col %in% names(result))

  option_rows <- result$Variable %in% c("Option A", "Option B", "Option C")
  expect_equal(sum(option_rows), 3)

  # The key behaviour: all three options keep a p-value, not just the first
  # (which is what a categorical/numeric variable's shared-p-value blanking
  # would otherwise do to rows 2 and 3 here).
  expect_true(all(!is.na(result[[pval_col]][option_rows])))
  expect_true(all(result[[pval_col]][option_rows] >= 0 & result[[pval_col]][option_rows] <= 1))

  # Variable == "Q1" & row_type == "label" together, not either alone - see
  # the stat_code check above for why (Variable alone also matches the
  # relocated base row; row_type alone also matches the "Sample sizes"
  # header row). row_type == "base" alone is fine on its own (only one row
  # has it) - the base row's Variable is no longer blank, so "" doesn't
  # match anything any more (see test 1's comment).
  expect_true(is.na(result[[pval_col]][result$Variable == "Q1" & result$row_type == "label"]))
  expect_true(is.na(result[[pval_col]][result$row_type == "base"]))

  # outcome_type kept and constant across the block (including the base
  # row), and the metadata block still sits at the very end even with a
  # p-value column in the mix (checking this specifically because p-value
  # columns are appended after the row_type/stat_code/outcome_type
  # relocate() would otherwise have already placed them at the end).
  expect_true(all(result$outcome_type == "multicoded"))
  # 4 now, not 3 - o_lab joins the metadata group (see test 1's comment).
  expect_equal(tail(names(result), 4), c("row_type", "stat_code", "outcome_type", "o_lab"))
})

test_that("10. format_pvalue() formats to 3dp with a <0.001 floor, and preserves NA", {
  expect_equal(format_pvalue(0.023456), "0.023")
  expect_equal(format_pvalue(0.5),      "0.500")
  expect_equal(format_pvalue(0.0005),   "<0.001")
  expect_true(is.na(format_pvalue(NA_real_)))
})

test_that("11. format_crosstab() builds spanning headers and formats the base row's counts correctly", {

  data <- tibble(
    region    = factor(c("North", "North", "South", "South", "South", "North")),
    sex       = factor(c("Male", "Male", "Male", "Female", "Female", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "region", predictors = list("sex", "age_group"),
                             statistics = "perc", multicode = FALSE)
  result <- format_crosstab(pivot_crosstab(stats_table))
  print(huxtable::as_flextable(result))

  expect_s3_class(result, "huxtable")
  # 7 rows, not 6 - the relocated Base section's own header row ("Sample
  # sizes") sits between South and the actual Base row now (see test 1's
  # comment on this feature, in test_pivot_crosstab.R's pivot-level tests).
  expect_equal(nrow(result), 7)   # spanning header + column names + label + North + South + Sample sizes header + Base

  # Spanning header row (row 1): "sex" merged over its two level columns,
  # "age_group" merged over its two, "Total" left blank (not part of either
  # span - it's shared/first-set-only, not sex- or age_group-specific).
  #
  # colspan()/the merged text are only stored at the ANCHOR cell of a merged
  # block - huxtable::merge_cells() puts them at the first (lowest-index)
  # column of the range, not at every column the merge covers; a non-anchor
  # cell in the same merged block reports colspan 1, not the group's real
  # span. format_crosstab() itself anchors at min(col_idx) (see the spans
  # loop), so the check here needs to do the same rather than picking an
  # arbitrary column from within the group - checking at "sex: Male"
  # specifically (not the anchor, since "sex: Female" sorts first
  # alphabetically) is exactly what failed on the first run of this test:
  # expected colspan 2, got 1, because Male is the second cell of that
  # merged pair, not the first. "age_group: Old" only happened to pass
  # because Old sorts before Young, so it WAS already the anchor there -
  # coincidence, not evidence the code only works for alphabetically-first
  # levels. Computed dynamically below rather than hardcoded either way.
  sex_cols_idx <- which(names(result) %in% c("sex: Male", "sex: Female"))
  sex_anchor   <- min(sex_cols_idx)
  age_cols_idx <- which(names(result) %in% c("age_group: Old", "age_group: Young"))
  age_anchor   <- min(age_cols_idx)
  total_col    <- which(names(result) == "Total")

  expect_equal(result[[sex_anchor]][1], "sex")
  expect_equal(huxtable::colspan(result)[1, sex_anchor], 2)
  expect_equal(result[[age_anchor]][1], "age_group")
  expect_equal(huxtable::colspan(result)[1, age_anchor], 2)
  expect_equal(result[["Total"]][1], "")

  # Column-name row (row 2, below the spanning row): displays the bare
  # level ("Male"/"Female"/"Old"/"Young"), not the "<p_lab>: <level>" text
  # the underlying column is actually named - that prefix is redundant once
  # the spanning header above already says "sex"/"age_group", and repeating
  # it in every column made the table harder to read. Column NAMES (used
  # for every lookup in this test, like sex_anchor above) stay prefixed -
  # only the displayed header row's text changes.
  expect_equal(result[["sex: Male"]][2],        "Male")
  expect_equal(result[["sex: Female"]][2],      "Female")
  expect_equal(result[["age_group: Old"]][2],   "Old")
  expect_equal(result[["age_group: Young"]][2], "Young")
  expect_equal(result[["Total"]][2], "Total")   # unprefixed to begin with - unaffected

  # Base row's counts: digits=0/comma-separated, not run through format_
  # statistic()'s stat_code-keyed rule (stat_code is NA on a base row, which
  # would fall through to that function's generic 6-decimal fallback, e.g.
  # "6.000000" instead of "6") - the bug self-checking caught before this
  # was ever run for real.
  #
  # Found via the "Sample sizes" header text, not Statistics == "Base" -
  # row_type isn't even a column any more by this point (format_crosstab()
  # drops it before building the huxtable), and Statistics itself is
  # blanked (NA) on the base row now that the section header conveys "Base"
  # once for the whole section (see the pivot-level tests' comment on this).
  # The base row is always exactly one row below its section header, since
  # relocation guarantees the header immediately precedes the whole block.
  sample_sizes_row <- which(result[["Variable"]] == "Sample sizes")
  base_row <- sample_sizes_row + 1
  expect_equal(result[["Total"]][base_row],      "6")
  expect_equal(result[["sex: Male"]][base_row],  "3")

  # region's label row is bold; North's level row is not. [1] matters here -
  # the relocated base row now ALSO reads Variable == "region" (its own
  # variable's label, per the relocation feature), and it always sorts
  # after the real label row since relocation moves it to the bottom, so
  # the first match is always the genuine label row.
  region_row <- which(result[["Variable"]] == "region")[1]
  north_row  <- which(result[["Variable"]] == "North")
  variable_col <- which(names(result) == "Variable")
  expect_true(huxtable::bold(result)[region_row, variable_col])
  expect_false(huxtable::bold(result)[north_row, variable_col])

  # North's estimate: "50" (0.5 -> no "%" sign, 0dp, same rule as
  # format_summary()'s percentages).
  expect_equal(result[["Total"]][north_row], "50")
})

test_that("12. format_crosstab() shows SE as an inserted row and merges it with its Mean row", {

  data <- tibble(
    age = c(10, 20, 30, 40, 50, 60),
    sex = factor(c("Male", "Male", "Male", "Female", "Female", "Female"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = "sex",
                             statistics = "mean", conf = "se", multicode = FALSE)
  result <- format_crosstab(pivot_crosstab(stats_table))
  print(huxtable::as_flextable(result))

  # 6 rows, not 5: spanning header ("sex" over Male/Female, Total blank,
  # same as test 11 - a single predictor set still gets a span) + column
  # names + Mean row + inserted SE row + the relocated section's own header
  # row + the actual Base row (see test 1's/test 11's comment on this
  # feature).
  expect_equal(nrow(result), 6)

  variable_col <- which(names(result) == "Variable")
  mean_row <- which(result[["Statistics"]] == "Mean")
  se_row   <- which(result[["Statistics"]] == "SE")
  # Via "Sample sizes" text, not Statistics == "Base" - see test 11's
  # comment on why.
  sample_sizes_row <- which(result[["Variable"]] == "Sample sizes")
  base_row <- sample_sizes_row + 1

  expect_equal(length(mean_row), 1)
  expect_equal(length(se_row), 1)
  expect_equal(se_row, mean_row + 1)   # SE row immediately follows its Mean row

  # Mean row: "20.0"/"50.0" (1dp, same rule as format_summary()'s means).
  # SE row: same formatting rule, holding the SE values instead.
  expect_equal(result[["Male"]][mean_row],   "20.0")
  expect_equal(result[["Female"]][mean_row], "50.0")
  expect_equal(result[["Male"]][se_row],     "5.8")   # 5.773503 -> 5.8
  expect_equal(result[["Female"]][se_row],   "5.8")

  # Base row unaffected by any of this - still digits=0.
  expect_equal(result[["Male"]][base_row], "3")

  # The Mean row's Variable text ("age") rowspans down over its own SE row -
  # checked via rowspan(), same access pattern as test_pivot_summary.R's
  # merge test.
  #
  # NOT "" on the SE row's own Variable cell - a real test run caught this:
  # per huxtable's own docs (spans.Rd), setting rowspan()/colspan() COPIES
  # the anchor cell's content into the cell(s) it covers, the same
  # "copy, not blank" behaviour already confirmed for merge_cells() (see
  # pivot_crosstab()'s Sample-sizes-merge comment). format_crosstab()'s own
  # source code tries to blank the covered row afterwards
  # (`ht[r, variable_col] <- ""`), but that assignment has no effect
  # extracting it back out - `[[` resolves a rowspan-covered cell to its
  # anchor's content regardless of what was last assigned there, the same
  # way a merged cell's stored value can't diverge from its anchor. That
  # blanking loop is dead code, not a bug in what gets DISPLAYED (the
  # printed table already only ever showed the anchor's text there, rowspan
  # collapses the display either way) - just misleading if read as doing
  # something. Confirmed present in pivot_summary.R and
  # pivot_nested_crosstab.R too (same copy-pasted pattern) - flagged to
  # Joe, not yet removed anywhere.
  #
  # Base stays a separate, unmerged row below - its Variable reads "age"
  # too (its own variable's label, per the relocation feature - no longer
  # blank), but rowspan 1 confirms it's NOT vertically merged with the
  # Mean/SE block above it, just carrying the same text coincidentally.
  expect_equal(result[["Variable"]][mean_row], "age")
  expect_equal(huxtable::rowspan(result)[mean_row, variable_col], 2)
  expect_equal(result[["Variable"]][se_row], "age")
  expect_equal(result[["Variable"]][base_row], "age")
  expect_equal(huxtable::rowspan(result)[base_row, variable_col], 1)

  # "age" is bold on its Mean row (is_variable_row - numeric variable's own
  # row); the SE and Base rows are not.
  expect_true(huxtable::bold(result)[mean_row, variable_col])
  expect_false(huxtable::bold(result)[se_row, variable_col])
  expect_false(huxtable::bold(result)[base_row, variable_col])
})

test_that("13. format_crosstab() merges Mean+Median rows and formats their shared p-value", {

  data <- tibble(
    age = c(10, 20, 30, 40, 50, 60),
    sex = factor(c("Male", "Male", "Male", "Female", "Female", "Female"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = "sex",
                             statistics = c("mean", "median"), pval = TRUE, multicode = FALSE)
  result <- format_crosstab(pivot_crosstab(stats_table))
  print(huxtable::as_flextable(result))

  # 6 rows, not 5: spanning header + column names + Mean + Median + the
  # relocated section's own header row + the actual Base row (no conf
  # requested here, so no inserted SE/CI rows - isolates the merge/p-value
  # mechanics from the row-insertion one tested in test 12).
  expect_equal(nrow(result), 6)

  variable_col <- which(names(result) == "Variable")
  mean_row   <- which(result[["Statistics"]] == "Mean")
  median_row <- which(result[["Statistics"]] == "Median")
  # Via "Sample sizes" text, not Statistics == "Base" - see test 11's
  # comment on why.
  sample_sizes_row <- which(result[["Variable"]] == "Sample sizes")
  base_row   <- sample_sizes_row + 1

  expect_equal(median_row, mean_row + 1)

  # Mean+Median merge into one "age" cell, same mechanic as test 12's
  # Mean+SE merge - here from two real statistic rows rather than one
  # statistic plus its inserted conf row.
  #
  # NOT "" on Median's own Variable cell - same "rowspan copies the anchor's
  # content into the cell(s) it covers" behaviour as test 12 (see its
  # comment for the full explanation, including the dead blanking loop this
  # caught in format_crosstab()'s own source).
  expect_equal(result[["Variable"]][mean_row], "age")
  expect_equal(huxtable::rowspan(result)[mean_row, variable_col], 2)
  expect_equal(result[["Variable"]][median_row], "age")

  # p-value: formatted via format_pvalue() (3dp / <0.001 floor), shown once
  # on the Mean row, blank on Median and Base - the underlying "which row
  # keeps the p-value" logic is pivot_crosstab()'s (already covered in test
  # 3), this just checks format_crosstab() renders whatever it's given
  # without disturbing that placement.
  pval_col <- "p_value (sex)"
  expect_true(pval_col %in% names(result))
  expect_false(is.na(result[[pval_col]][mean_row]))
  expect_true(is.na(result[[pval_col]][median_row]))
  expect_true(is.na(result[[pval_col]][base_row]))
  expect_true(stringr::str_detect(result[[pval_col]][mean_row], "^(<0\\.001|[01]\\.\\d{3})$"))
})

test_that("14. format_crosstab() combines everything: two predictor sets, two statistics, SE rows, and two p-value columns", {

  # Same data as test 8 (numeric age, two predictor sets sex/age_group),
  # extended to two statistics (mean, median) and conf = "se" - the most
  # demanding combination the current design supports at once: spanning
  # headers over two sets AND a numeric variable's stat-row merge AND the
  # inserted-conf-row mechanic, all interacting in one table. In particular
  # this checks that the Mean/SE/Median/SE rows all merge into ONE "age"
  # cell spanning all four rows (not two separate two-row merges, and not
  # accidentally pulled apart by the conf-row insertion happening between
  # them) - that interaction isn't exercised by tests 12 or 13 alone, since
  # 12 has one statistic and 13 has no conf rows.
  #
  # sex: Male (10,20,30) mean=20, se=10/sqrt(3)=5.773503; median=20
  #      Female (40,50,60) mean=50, se=5.773503; median=50
  # Total (10..60): mean=35, se=18.708287/sqrt(6)=7.637626; median=35
  # age_group means/medians and their SEs aren't re-verified by hand here
  # (already covered structurally, not numerically, for age_group in test
  # 8) - only sex and Total get exact-value checks; age_group gets presence/
  # structure checks only.
  data <- tibble(
    age       = c(10, 20, 30, 40, 50, 60),
    sex       = factor(c("Male", "Male", "Male", "Female", "Female", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Old"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = list("sex", "age_group"),
                             statistics = c("mean", "median"), conf = "se", pval = TRUE,
                             multicode = FALSE)
  result <- format_crosstab(pivot_crosstab(stats_table))
  print(huxtable::as_flextable(result))

  # 8 rows, not 7: spanning header + column names + Mean + SE + Median + SE +
  # the relocated section's own header row + the actual Base row.
  expect_equal(nrow(result), 8)

  variable_col <- which(names(result) == "Variable")
  mean_row   <- which(result[["Statistics"]] == "Mean")
  median_row <- which(result[["Statistics"]] == "Median")
  se_rows    <- which(result[["Statistics"]] == "SE")
  # Via "Sample sizes" text, not Statistics == "Base" - see test 11's
  # comment on why.
  sample_sizes_row <- which(result[["Variable"]] == "Sample sizes")
  base_row   <- sample_sizes_row + 1

  expect_equal(length(se_rows), 2)
  se_of_mean_row   <- mean_row + 1
  se_of_median_row <- median_row + 1
  expect_setequal(se_rows, c(se_of_mean_row, se_of_median_row))
  expect_equal(median_row, se_of_mean_row + 1)   # Mean, SE, Median, SE, Sample sizes, Base - in that order
  # Sample sizes header sits right after Median's SE row now, not Base
  # directly - the relocated section always inserts its own header
  # immediately before the base row(s) it introduces.
  expect_equal(sample_sizes_row, se_of_median_row + 1)
  expect_equal(base_row, se_of_median_row + 2)

  # Total/sex: Male/sex: Female: exact values, both Mean and its SE row.
  # Two predictor sets here (unlike test 12's single-set case), so sex's
  # columns are prefixed "sex: Male"/"sex: Female" - bare "Male"/"Female"
  # don't exist in this table at all (that was the actual bug this test
  # caught: result[["Male"]] is NULL here, not a missing/NA value).
  expect_equal(result[["Total"]][mean_row],  "35.0")
  expect_equal(result[["sex: Male"]][mean_row],   "20.0")
  expect_equal(result[["sex: Female"]][mean_row], "50.0")
  expect_equal(result[["Total"]][se_of_mean_row],  "7.6")
  expect_equal(result[["sex: Male"]][se_of_mean_row],   "5.8")
  expect_equal(result[["sex: Female"]][se_of_mean_row], "5.8")
  expect_equal(result[["sex: Male"]][median_row],   "20.0")
  expect_equal(result[["sex: Female"]][median_row], "50.0")

  # age_group: present and populated (not re-verified numerically here),
  # both on the Mean row and its SE row.
  expect_true(all(c("age_group: Young", "age_group: Old") %in% names(result)))
  expect_false(is.na(result[["age_group: Young"]][mean_row]))
  expect_false(is.na(result[["age_group: Young"]][se_of_mean_row]))

  # Base row: plain counts, unaffected by any of the merge/conf machinery.
  expect_equal(result[["Total"]][base_row], "6")
  expect_equal(result[["sex: Male"]][base_row],  "3")

  # The key interaction: all four of Mean/SE/Median/SE merge into ONE "age"
  # cell (rowspan 4), not two separate two-row merges - Base stays outside
  # the merge entirely, its own unmerged row (Variable reads "age" too now,
  # per the relocation feature - no longer blank - but rowspan 1 confirms
  # it's not vertically merged with the block above it).
  #
  # NOT "" on the three covered rows (SE, Median, Median's SE) - same
  # "rowspan copies the anchor's content into the cell(s) it covers"
  # behaviour as test 12/13 (see test 12's comment for the full
  # explanation).
  expect_equal(result[["Variable"]][mean_row], "age")
  expect_equal(huxtable::rowspan(result)[mean_row, variable_col], 4)
  expect_equal(result[["Variable"]][se_of_mean_row], "age")
  expect_equal(result[["Variable"]][median_row], "age")
  expect_equal(result[["Variable"]][se_of_median_row], "age")
  expect_equal(result[["Variable"]][base_row], "age")
  expect_equal(huxtable::rowspan(result)[base_row, variable_col], 1)

  # Both spanning headers present (checked at their anchor columns, same
  # reasoning as test 11 - "sex: Female"/"age_group: Old" sort first
  # alphabetically within their groups, so they're the anchors here). Anchor
  # position itself is unaffected by the span now also covering each set's
  # p_value column, since a p_value column always sits LAST within its
  # set's group (after the level columns) - colspan is 3 here, not 2 as in
  # test 11, specifically because this test (unlike test 11) requests
  # pval = TRUE, so there's a p_value column for each span to fold in.
  sex_anchor <- min(which(names(result) %in% c("sex: Male", "sex: Female")))
  age_anchor <- min(which(names(result) %in% c("age_group: Young", "age_group: Old")))
  expect_equal(result[[sex_anchor]][1], "sex")
  expect_equal(huxtable::colspan(result)[1, sex_anchor], 3)   # Female/Male + p_value (sex)
  expect_equal(result[[age_anchor]][1], "age_group")
  expect_equal(huxtable::colspan(result)[1, age_anchor], 3)   # Old/Young + p_value (age_group)

  # Column-name row shows the bare level, not the "<p_lab>: " prefix - same
  # check as test 11, here with a spanning header AND inserted conf rows
  # both present, to confirm the header-row edit isn't somehow scoped to
  # only the row positions test 11 happened to have.
  expect_equal(result[["sex: Male"]][2], "Male")
  expect_equal(result[["age_group: Old"]][2], "Old")

  # Two distinct p-value columns, each shown once - on the Mean row only,
  # blank on every other row (its own SE row, Median, Median's SE, Base).
  # This is the sharpest check that the conf-row insertion didn't disturb
  # pivot_crosstab()'s p-value placement, and that blanking p-values on
  # inserted conf rows didn't accidentally blank them on the real Median
  # row too (a plausible copy-paste-style bug given how similar those two
  # blanking steps are).
  for (pval_col in c("p_value (sex)", "p_value (age_group)")) {
    expect_true(pval_col %in% names(result))
    expect_false(is.na(result[[pval_col]][mean_row]))
    expect_true(is.na(result[[pval_col]][se_of_mean_row]))
    expect_true(is.na(result[[pval_col]][median_row]))
    expect_true(is.na(result[[pval_col]][se_of_median_row]))
    expect_true(is.na(result[[pval_col]][base_row]))
  }

  # Column order: each set's p-value column sits right after that set's own
  # level columns ("p_value (sex)" right after "sex: Male", before
  # age_group's columns even start), not clustered together with every
  # other set's p-value column at the very end - the thing this test
  # actually caught wrong on its first run.
  expect_equal(names(result)[which(names(result) == "sex: Male") + 1], "p_value (sex)")
  expect_equal(names(result)[which(names(result) == "age_group: Young") + 1], "p_value (age_group)")
})


# Regression test for the stat_type_of() extension: mean and sd both need to
# land in the "num_u" bucket now (see stat_type_of()'s header comment on why
# two statistics of the same type share one significance test/p-value rather
# than one each). Same "shown once, on the first row" pattern as test 3
# (mean+median), just with sd instead of median as the second statistic -
# confirming this isn't special-cased to the original three numeric stats.

test_that("15. mean and sd share one p-value, same as mean and median do in test 3", {
  data <- tibble(
    age = c(10, 20, 30, 40, 50, 60),
    sex = factor(c("Male", "Male", "Male", "Female", "Female", "Female"))
  )
  stats_table <- calc_stats(data, outcomes = "age", predictors = "sex",
                             statistics = c("mean", "sd"), pval = TRUE, multicode = FALSE)
  result <- pivot_crosstab(stats_table)[[1]]
  print(result)

  # 4 rows, not 3 - Mean + SD + the relocated section's own header row + the
  # actual Base row (see test 1's comment on this feature).
  expect_equal(nrow(result), 4)
  expect_equal(result$Statistics, c("Mean", "SD", NA, NA))

  pval_col <- "p_value (sex)"
  expect_true(pval_col %in% names(result))
  expect_false(is.na(result[[pval_col]][1]))   # shown on Mean (first row of the num_u block)
  expect_true(is.na(result[[pval_col]][2]))    # blank on SD
  expect_true(is.na(result[[pval_col]][3]))    # blank on the section header
  expect_true(is.na(result[[pval_col]][4]))    # blank on Base
})


# Tests 16-17 cover the Base-relocation feature ported from
# pivot_nested_crosstab()/format_nested_crosstab() - every variable's Base
# row pulled into one consolidated "Sample sizes" section at the bottom,
# rather than sitting inline right under each variable's own data;
# individual base rows show no "Base" text in Statistics since the section
# header already conveys it once, and Variable+Statistics are merged into a
# single wide cell in that section for extra label room. Same feature, same
# reasoning, as the nested table's own tests 12-13 - see pivot_crosstab()'s
# header note on why this ported cleanly (base_wide already merges every
# predictor set's level columns into one row per variable, so multiple
# predictor sets don't add any real complexity here).
#
# Test 16 is pivot_crosstab()'s side - deliberately uses two outcomes of
# DIFFERENT types (numeric age, categorical result) requested together, same
# reasoning as the nested table's test 12: the case most likely to expose an
# outcome-type-specific assumption in the relocation logic (there isn't one -
# relocation keys off row_type, not outcome_type). Test 17 is format_
# crosstab()'s side - the padding/merge/alignment refinements, using two
# ADJACENT NUMERIC outcomes (age, score) so it also re-checks the o_lab-based
# block-boundary fix (format_crosstab()'s new_block, no longer derivable from
# "row right after a base row" once every base row moves to the bottom) the
# same way the nested table's test 13 does, plus the header-alignment fix
# (row 1's spanning header should stay centered, not get overwritten back to
# right by the later "everything else is right-aligned" call).

test_that("16. pivot_crosstab() relocates every variable's Base row into one consolidated section at the bottom, correctly labelled, with two outcomes of DIFFERENT types requested together", {
  data <- tibble(
    age    = c(10, 20, 30, 40, 50, 60, 70, 80),
    result = factor(c("Pass", "Pass", "Fail", "Fail", "Pass", "Pass", "Fail", "Fail")),
    sex    = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                     levels = c("Male", "Female"))
  )
  # "mean" only applies to the numeric outcome (age), "perc" only to the
  # categorical one (result) - calc_stats() strips whichever doesn't apply
  # to each outcome's type internally, so requesting both together is
  # exactly how a real mixed-type call would look, not a special case this
  # test has to work around (same reasoning as the nested table's test 12).
  stats_table <- calc_stats(data, outcomes = c("age", "result"), predictors = "sex",
                             statistics = c("mean", "perc"), multicode = FALSE)
  pivoted <- pivot_crosstab(stats_table)[[1]]
  print(pivoted)

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

  # hand-calculated: 4 respondents per sex level, for both outcomes - no
  # missing data for either variable in this dataset (4 Male, 4 Female).
  expect_true(all(base_rows$Male == 4))
  expect_true(all(base_rows$Female == 4))
  expect_true(all(base_rows$Total == 8))
})

test_that("17. format_crosstab() still separates two adjacent NUMERIC variables, gives the Base section header its own top-padding gap, keeps every base row after it tight, and keeps the spanning header centered", {
  data <- tibble(
    age   = c(10, 20, 30, 40, 50, 60, 70, 80),
    score = c(1, 2, 3, 4, 5, 6, 7, 8),
    sex   = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                    levels = c("Male", "Female"))
  )
  stats_table <- calc_stats(data, outcomes = c("age", "score"), predictors = "sex",
                             statistics = "mean", multicode = FALSE)
  result <- format_crosstab(pivot_crosstab(stats_table))
  print(huxtable::as_flextable(result))

  # 7 rows: spanning header ("sex" over Male/Female - a single predictor set
  # still gets a span, same as test 12) + column names + age's Mean row +
  # score's Mean row + the relocated section's own header row + 2 base rows.
  expect_equal(nrow(result), 7)

  variable_col <- which(names(result) == "Variable")

  # Statistics == "Mean" matters here, not just Variable == "age"/"score" -
  # the relocated Base row for a numeric outcome deliberately carries that
  # SAME Variable text (see pivot_crosstab()'s relocation comment), so
  # matching on Variable alone would catch both rows.
  age_row   <- which(result[[variable_col]] == "age"   & result[["Statistics"]] == "Mean")
  score_row <- which(result[[variable_col]] == "score" & result[["Statistics"]] == "Mean")

  # Both age (the table's very first data row) and score (a new variable
  # starting right after age, with nothing base-related between them any
  # more) get the same 6pt block-start gap - confirms format_crosstab()'s
  # new_block correctly derives this from o_lab rather than the old "row
  # right after a base row" proxy, which would have silently failed here
  # now that no base row ever sits between two adjacent variables.
  expect_equal(huxtable::top_padding(result)[age_row, variable_col], 6)
  expect_equal(huxtable::top_padding(result)[score_row, variable_col], 6)

  sample_sizes_row <- which(result[[variable_col]] == "Sample sizes")
  expect_equal(length(sample_sizes_row), 1)
  # NOT is.na() here - huxtable's merge_cells() COPIES the anchor
  # (Variable) cell's content into the merged Statistics cell rather than
  # blanking it (confirmed against huxtable's own docs, not hand-traced -
  # same correction already made for the nested table's equivalent test).
  expect_equal(result[["Statistics"]][sample_sizes_row], "Sample sizes")

  # The section header is a block start (row_type == "label" upstream), so
  # it gets the same 6pt top-padding gap any other block start gets.
  expect_equal(huxtable::top_padding(result)[sample_sizes_row, variable_col], 6)

  # Every base row AFTER the header belongs to the same consolidated
  # section, not a block of its own - default padding (1), not the 6pt gap.
  base_data_rows <- (sample_sizes_row + 1):nrow(result)
  expect_true(all(huxtable::top_padding(result)[base_data_rows, variable_col] == 1))

  # Variable and Statistics are merged into one wide cell across the whole
  # relocated section (header row included).
  statistics_col <- which(names(result) == "Statistics")
  expect_equal(huxtable::colspan(result)[sample_sizes_row, variable_col], 2)
  for (r in base_data_rows) {
    expect_equal(huxtable::colspan(result)[r, variable_col], 2)
    expect_equal(result[["Statistics"]][r], result[[variable_col]][r])
  }

  # Row 1 (the spanning header, "sex" over Male/Female) stays centered, not
  # right-aligned - this is the header-alignment fix: right-alignment used
  # to be applied "everywhere" (all rows), silently overwriting the center
  # this row was explicitly given earlier in format_crosstab(). Row 2
  # (bare "Male"/"Female" column names) stays right-aligned, unaffected -
  # only row 1 was ever wrongly overwritten. Anchor computed dynamically
  # (same reasoning as test 11/14's sex_anchor) rather than assumed to be
  # "Male" specifically - the merged/displayed text only lives at
  # min(col_idx), whichever of Male/Female that happens to be.
  sex_col_idx <- which(names(result) %in% c("Male", "Female"))
  anchor <- min(sex_col_idx)
  expect_equal(result[[anchor]][1], "sex")
  expect_equal(huxtable::align(result)[1, anchor], "center")
  expect_equal(huxtable::align(result)[2, anchor], "right")
  expect_equal(huxtable::align(result)[age_row, anchor], "right")
})


# Tests 18-20 cover calc_stats(pairwise = TRUE) support - a "legend" row
# (one letter per level column, built from a lookup pivot_crosstab() now
# returns as its 6th list element, NOT baked into pivoted itself - see
# reshape_one_predictor_set()'s header note on why) and sig_diff shown as
# its own inserted row, same mechanic as SE/CI, with SE/CI suppressed
# entirely whenever sig_diff is present. Test 18 is pivot_crosstab()'s side
# (the legend element and __sigdiff columns); test 19 checks the NULL/absent
# case for backward compatibility; test 20 is format_crosstab()'s side (the
# actual rendered legend row, the Sig. diff row, and SE/CI's absence).
#
# Uses the same North 8/10 Yes vs South 2/10 Yes design as test 10/18 in
# test_add_pairwise_sig.R (unweighted proportions, wt = 1 and ids = 1 for
# test 19's weighted call so the true covariance between the two disjoint
# groups is genuinely zero) - a clean, hand-verified significant difference,
# not re-derived here.

test_that("18. pivot_crosstab() returns a legend (element 6) and __sigdiff columns when calc_stats(pairwise = TRUE) was used", {
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2), rep("Yes", 2), rep("No", 8)), levels = c("No", "Yes")),
    region   = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South"))
  )
  stats_table <- calc_stats(data, outcomes = "response", predictors = "region",
                             statistics = "perc", conf = "se", multicode = FALSE, pairwise = TRUE)
  pivot_result <- pivot_crosstab(stats_table)

  legend <- pivot_result[[6]]
  expect_false(is.null(legend))
  expect_equal(sort(legend$level_col), c("North", "South"))
  expect_equal(length(unique(legend$sig_letter)), 2)
  # Total never gets a legend entry - it's excluded from pairwise testing
  # entirely (add_pairwise_sig()'s own eligible filter), nothing to compare
  # a single overall estimate against.
  expect_false("Total" %in% legend$level_col)

  result <- pivot_result[[1]]
  expect_true(any(str_detect(names(result), "__sigdiff")))
})

test_that("19. pivot_crosstab() returns a NULL legend and no __sigdiff columns when pairwise wasn't used (backward compatibility)", {
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2), rep("Yes", 2), rep("No", 8)), levels = c("No", "Yes")),
    region   = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South"))
  )
  stats_table <- calc_stats(data, outcomes = "response", predictors = "region",
                             statistics = "perc", conf = "se", multicode = FALSE, pairwise = FALSE)
  pivot_result <- pivot_crosstab(stats_table)

  expect_null(pivot_result[[6]])
  expect_false(any(str_detect(names(pivot_result[[1]]), "__sigdiff")))
})

test_that("20. format_crosstab() shows the legend as its own first row, a Sig. diff row instead of SE, with the letters matching pivot_crosstab()'s own legend element", {
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2), rep("Yes", 2), rep("No", 8)), levels = c("No", "Yes")),
    region   = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South")),
    wt       = rep(1, 20)
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  stats_table <- calc_stats(design, outcomes = "response", predictors = "region",
                             statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)
  pivot_result <- pivot_crosstab(stats_table)
  legend <- pivot_result[[6]]
  result <- format_crosstab(pivot_result)
  print(huxtable::as_flextable(result))

  variable_col <- which(names(result) == "Variable")
  legend_row   <- which(result[["Variable"]] == "Column reference")
  expect_equal(length(legend_row), 1)

  # Variable + Statistics merged into one wide cell, same treatment (and
  # same "merge copies the anchor's content" caveat) as the Sample sizes
  # section header - see test 17's identical check.
  expect_equal(huxtable::colspan(result)[legend_row, variable_col], 2)
  expect_equal(result[["Statistics"]][legend_row], "Column reference")

  # Each level column shows that column's own letter - cross-checked
  # against pivot_crosstab()'s own legend element (element 6), not
  # re-derived or hardcoded to a/b, since which letter each level actually
  # gets isn't the point of this test (that's assign_sig_letters()'s own
  # coverage in test_add_pairwise_sig.R) - only that format_crosstab()
  # renders whatever it's given, in the right place.
  north_letter <- legend$sig_letter[legend$level_col == "North"]
  south_letter <- legend$sig_letter[legend$level_col == "South"]
  expect_equal(result[["North"]][legend_row], north_letter)
  expect_equal(result[["South"]][legend_row], south_letter)

  # Legend sits above every real variable row - checked against the "Yes"
  # level row specifically (response is categorical, so its own label row
  # sits between the legend and "Yes" - not asserting the legend is
  # IMMEDIATELY above the first data row, just that it's the first thing
  # shown, period).
  yes_row <- which(result[["Variable"]] == "Yes")
  expect_true(legend_row < yes_row)

  # No SE row anywhere, even though conf = "se" was requested - suppressed
  # entirely because pairwise sig_diff is being shown instead (has_pairwise
  # in format_crosstab()).
  expect_false("SE" %in% result[["Statistics"]])

  # A "Sig. diff" row directly below "Yes"'s own estimate row, holding the
  # OTHER column's letter on each side - North (80% Yes) and South (20%
  # Yes) are clearly significantly different here (same design as test
  # 10/18 in test_add_pairwise_sig.R).
  yes_diff_row <- yes_row + 1
  expect_equal(result[["Statistics"]][yes_diff_row], "Sig. diff")
  expect_equal(result[["North"]][yes_diff_row], south_letter)
  expect_equal(result[["South"]][yes_diff_row], north_letter)
})


test_that("21. format_crosstab() on a fuller, semi-realistic table - two predictor sets, weighted AND unweighted, pairwise + conf + p-values together - printed as a flextable for visual review", {
  # Four quadrants of 15, giving BOTH sex and age_group a real (not
  # hand-verified to the decimal here - tests 10/18/21 in test_add_
  # pairwise_sig.R already cover the exact arithmetic) difference to look
  # at: Male-Young 12/15 Yes, Male-Old 9/15, Female-Young 6/15, Female-Old
  # 3/15 -> sex: Male 70% vs Female 30%; age_group: Young 60% vs Old 40%.
  data <- tibble(
    response = factor(c(rep("Yes", 12), rep("No", 3),    # Male-Young
                         rep("Yes", 9),  rep("No", 6),    # Male-Old
                         rep("Yes", 6),  rep("No", 9),    # Female-Young
                         rep("Yes", 3),  rep("No", 12)),  # Female-Old
                       levels = c("No", "Yes")),
    sex       = factor(c(rep("Male", 30), rep("Female", 30)), levels = c("Male", "Female")),
    age_group = factor(rep(c(rep("Young", 15), rep("Old", 15)), 2), levels = c("Young", "Old")),
    wt        = rep(1, 60)
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  # Just w_perc, not c("perc", "w_perc") - pivot_crosstab() only supports
  # one statistic per categorical variable (test 5's own coverage; "response"
  # here is categorical) - c("perc", "w_perc") together is fine for
  # calc_stats() itself (as compare_pairwise_quick.R's both_result already
  # confirmed for real), just not something pivot_crosstab() accepts in one
  # table. w_perc exercises the exact/covmat pairwise path - two predictor
  # sets plus p-values is still a genuinely busier table than test 20's
  # single-set case.
  stats_table <- calc_stats(design, outcomes = "response",
                             predictors = list("sex", "age_group"),
                             statistics = "w_perc", conf = "se", multicode = FALSE,
                             pval = TRUE, pairwise = TRUE)
  result <- format_crosstab(pivot_crosstab(stats_table))
  print(huxtable::as_flextable(result))

  # Light structural checks only - this test's real job is the printed
  # table above, for visual review. The underlying numbers are already
  # covered precisely elsewhere (test_add_pairwise_sig.R, tests 8/11/14 in
  # this file for the multi-set/p-value mechanics individually).
  expect_s3_class(result, "huxtable")
  expect_true("Column reference" %in% result[["Variable"]])
  expect_true("Sig. diff" %in% result[["Statistics"]])
  expect_false("SE" %in% result[["Statistics"]])
  expect_true(all(c("p_value (sex)", "p_value (age_group)") %in% names(result)))
})


# Tests 22-24: the same pairwise mechanism (tests 18/20/21 above), but for a
# numeric mean/w_mean statistic instead of perc/w_perc - added once
# weighted_mean_svyby() gave calc_stats(pairwise = TRUE) a real covariance
# for means too (see calc_stats.R/test_weighted_mean_svyby.R). Not expected
# to need any CODE changes in pivot_crosstab.R itself - reshape_one_
# predictor_set()'s sig_diff/sig_letter handling and format_crosstab()'s
# sigdiff-row/legend-row insertion are both written generically, keyed off
# whether those columns are PRESENT in calc_stats() output, never off `stat`
# or `outcome_type` - see that file's own header notes on reshape_one_
# predictor_set()'s legend construction (which explicitly anticipates a
# predictor set mixing a pairwise-eligible stat with an ineligible one) and
# format_crosstab()'s has_pairwise gate. These tests exist to confirm that
# empirically rather than just asserting it from reading the code, matching
# this project's usual practice.
#
# Test 22 uses plain "mean" (unweighted, approximate/derive_se() path - the
# same path test 27 in test_add_pairwise_sig.R already covers at the
# calc_stats() level) so both the exact-covmat (w_mean) and approximate
# (mean) pairwise paths get exercised somewhere in this file, not just one.
# Tests 23-24 use w_mean, the exact-covmat path, matching tests 20/21's own
# choice of w_perc over perc for the same reason (exercising the real
# covariance machinery, not the approximation).
#
# North/South split into two obviously-separated numeric ranges (10-28 vs
# 80-98) rather than a hand-calculable exact SE (unlike test 12 in
# test_weighted_mean_svyby.R) - the point of these tests is the pivot/format
# MECHANISM (legend/sigdiff row placement, SE suppression), not the
# arithmetic, which is already covered precisely elsewhere. A wide,
# unambiguous separation just guarantees a real significant difference
# regardless of the exact variance formula in play, so the test doesn't
# become fragile to how that variance is computed.

test_that("22. pivot_crosstab() returns a legend (element 6) and __sigdiff columns for a numeric \"mean\" statistic under calc_stats(pairwise = TRUE)", {
  data <- tibble(
    score  = c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28,
               80, 82, 84, 86, 88, 90, 92, 94, 96, 98),
    region = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South"))
  )
  stats_table <- calc_stats(data, outcomes = "score", predictors = "region",
                             statistics = "mean", conf = "se", multicode = FALSE, pairwise = TRUE)
  pivot_result <- pivot_crosstab(stats_table)

  legend <- pivot_result[[6]]
  expect_false(is.null(legend))
  expect_equal(sort(legend$level_col), c("North", "South"))
  expect_equal(length(unique(legend$sig_letter)), 2)
  expect_false("Total" %in% legend$level_col)

  result <- pivot_result[[1]]
  expect_true(any(str_detect(names(result), "__sigdiff")))
})

test_that("23. format_crosstab() shows a legend row, a Sig. diff row, and suppresses SE for a weighted mean (w_mean) statistic", {
  data <- tibble(
    score  = c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28,
               80, 82, 84, 86, 88, 90, 92, 94, 96, 98),
    region = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South")),
    wt     = rep(1, 20)
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  stats_table <- calc_stats(design, outcomes = "score", predictors = "region",
                             statistics = "w_mean", conf = "se", multicode = FALSE, pairwise = TRUE)
  pivot_result <- pivot_crosstab(stats_table)
  legend <- pivot_result[[6]]
  result <- format_crosstab(pivot_result)
  print(huxtable::as_flextable(result))

  variable_col <- which(names(result) == "Variable")
  legend_row   <- which(result[["Variable"]] == "Column reference")
  expect_equal(length(legend_row), 1)

  expect_equal(huxtable::colspan(result)[legend_row, variable_col], 2)
  expect_equal(result[["Statistics"]][legend_row], "Column reference")

  north_letter <- legend$sig_letter[legend$level_col == "North"]
  south_letter <- legend$sig_letter[legend$level_col == "South"]
  expect_equal(result[["North"]][legend_row], north_letter)
  expect_equal(result[["South"]][legend_row], south_letter)

  # Numeric variable: no separate label row (unlike test 20's categorical
  # "Yes" row) - "score" (o_lab) IS the data row's own Variable text, per
  # pivot_one_block()'s !is_categorical branch. BUT the relocated base row
  # ALSO reads Variable == "score" (test 2's own documented behaviour -
  # base_row's Variable is set to `this_lab`, not blank), and after
  # format_crosstab()'s Variable+Statistics merge_cells() on that base row,
  # its Statistics cell copies the SAME "score" text too (merging copies the
  # anchor cell's content rather than blanking the other cell - see that
  # function's own comment on this). So filtering on Variable alone would
  # match both rows; disambiguated here via Statistics == "Mean (w)" (the
  # real data row's tidy_statistic_description() label - see
  # pivot_summary.R), which the base row never has.
  score_row <- which(result[["Variable"]] == "score" & result[["Statistics"]] == "Mean (w)")
  expect_equal(length(score_row), 1)
  expect_true(legend_row < score_row)

  # No SE row anywhere, even though conf = "se" was requested - suppressed
  # because sig_diff is being shown instead (has_pairwise).
  expect_false("SE" %in% result[["Statistics"]])

  diff_row <- score_row + 1
  expect_equal(result[["Statistics"]][diff_row], "Sig. diff")
  expect_equal(result[["North"]][diff_row], south_letter)
  expect_equal(result[["South"]][diff_row], north_letter)
})


test_that("24. format_crosstab() on a fuller w_mean table - two predictor sets, pairwise + conf + p-values together - printed as a flextable for visual review", {
  # Same quadrant-of-15 structure as test 21's response variable (Male-Young/
  # Male-Old/Female-Young/Female-Old), continuous instead of categorical:
  # decreasing means (90/70/50/30) give both sex (Male ~80 vs Female ~40) and
  # age_group (Young ~70 vs Old ~50) a real, unambiguous difference.
  set.seed(11)
  n <- 15
  data <- tibble(
    score     = c(rnorm(n, 90, 5), rnorm(n, 70, 5), rnorm(n, 50, 5), rnorm(n, 30, 5)),
    sex       = factor(c(rep("Male", n * 2), rep("Female", n * 2)), levels = c("Male", "Female")),
    age_group = factor(rep(c(rep("Young", n), rep("Old", n)), 2), levels = c("Young", "Old")),
    wt        = rep(1, n * 4)
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  stats_table <- calc_stats(design, outcomes = "score",
                             predictors = list("sex", "age_group"),
                             statistics = "w_mean", conf = "se", multicode = FALSE,
                             pval = TRUE, pairwise = TRUE)
  result <- format_crosstab(pivot_crosstab(stats_table))
  print(huxtable::as_flextable(result))

  # Light structural checks only, same restraint as test 21 - the exact
  # arithmetic is covered elsewhere (test_weighted_mean_svyby.R,
  # test_add_pairwise_sig.R's tests 26-29).
  expect_s3_class(result, "huxtable")
  expect_true("Column reference" %in% result[["Variable"]])
  expect_true("Sig. diff" %in% result[["Statistics"]])
  expect_false("SE" %in% result[["Statistics"]])
  expect_true(all(c("p_value (sex)", "p_value (age_group)") %in% names(result)))
})
