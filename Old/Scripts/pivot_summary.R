# =============================================================================
# pivot_summary() / format_summary() — new pivot + formatting layer for
# calc_stats() output
#
# Standalone replacement in progress for pivot1()/pivot1h() (in
# Scripts/summary_table.R, both untouched). pivot_summary() reshapes
# calc_stats() output into a tidy data frame; format_summary() takes that and
# returns a styled huxtable. No combined summary_table()-style wrapper yet.
# Depends on calc_stats() and prepare_base_for_table() from
# simplified_functions.R — sourced below.
#
# pivot_summary()'s new structure, agreed on directly:
#   - Variable and Levels collapse into a single "Variable" column. Numeric
#     variables put the label directly on their one data row (nothing to
#     collapse). Categorical variables get an extra "label row" above their
#     levels holding just the label, with every other column blank — the
#     levels themselves go in that same Variable column, one per row, below.
#   - A `row_type` column ("label"/"data") marks which rows are these new
#     label rows, so a later formatting step doesn't have to re-infer it from
#     blank/NA cells the way pivot1h() currently does.
#   - base_description handling is untouched — same collapse-and-format logic
#     as pivot1(), via the existing prepare_base_for_table(), and (as in
#     pivot1()) it does NOT appear as a per-row column in the pivoted table —
#     only as the separate footnote text returned alongside it.
#
# Note on multiple statistics per variable: a variable shown with more than
# one statistic (e.g. mean and median for the same numeric variable, or
# percentage and count for the same categorical variable) gets one block —
# and, for a categorical variable, one label row — per statistic, since
# blocks are split by the (variable, statistic) combination as specified.
# The variable's name will appear more than once in that case. Flagging this
# since it's a visible consequence worth confirming is what you want, rather
# than something to silently special-case. Test 8 below demonstrates it.
#
# Method: split the table into one chunk per (outcome, stat) combination —
# each chunk already shares one o_lab, one base, and (for categorical stats)
# one full set of o_cat levels, so it's a natural unit to pivot in isolation —
# pivot each chunk, then bind them all back together. fct_inorder() keeps
# chunks in their original order; group_split() on a plain character column
# would otherwise re-sort them alphabetically.
#
# A `stat_code` column carries the original (unrelabelled) stat string
# through too, since number-formatting decisions downstream need to key off
# "perc"/"mean"/etc, not the human-readable "%"/"Mean" text in Statistics.
# This replaces the original's reliance on stat_mem's *position* (pivot1h()
# did `ht |> pull(1)`) with a named column instead — one less thing that
# breaks if a column gets reordered later.
#
# A `block_id` column (an integer, one value per (outcome, stat) block, in
# the same first-appearance order as everything else) is carried through the
# same way, so a later formatting step can stripe alternating background
# colours per variable block without needing to re-derive block boundaries
# from scratch — the old identify_natural_breaks_pivot2() helper existed
# purely to work that out after the fact; here the boundary is already known
# at the point the blocks are built.
# =============================================================================

library(tidyverse)
library(huxtable)
source(here::here("Scripts", "simplified_functions.R"))

# Every huxtable function below is called as huxtable::foo() rather than bare
# foo(), even though library(huxtable) is attached above - flextable (loaded
# elsewhere in this project, e.g. Scripts/summary_table.R, and often in the
# same session as this file) masks several names huxtable also uses (align,
# font, etc.), so an unqualified call can silently resolve to the wrong
# package's version depending on load order. Namespacing removes the
# ambiguity regardless of what else happens to be attached.


# ---- tidy_statistic_description: relabel `stat` for display ------------------
#
# SIMPLIFIED from the Scripts/summary_table.R version (that one's untouched -
# it's what the old pivot1()/pivot1h() still call). That version could
# rename the *column itself* to "Statistics (weighted)"/"Statistics
# (unweighted)"/plain "Statistics" depending on the data, and offered a
# concise/verbose choice ("Mean" vs "Unweighted mean") on top of that.
#
# Both are gone. stat_code (see pivot_summary() below) already carries the
# raw, unrelabelled stat string through, specifically so anything that needs
# to key off it programmatically doesn't have to touch this column at all -
# which means Statistics has no job left except to be read by a person, and
# there's no reason to make that column's name OR its verbosity conditional
# on the data. It's always called "Statistics" and always uses the short
# form - the whole point of combining pivot1/pivot1h in the first place was
# that the old statistics column was "clunky - long text, lots of space
# used"; the verbose form was never something you asked for, just the
# original's long-standing default carried forward out of caution. A named
# lookup vector replaces the case_when() repetition too - one vectorised
# lookup instead of up to 10 explicit comparisons.

tidy_statistic_description <- function(data) {

  labels <- c(
    mean = "Mean", median = "Median", sum = "Sum", perc = "%", count = "Count",
    w_mean = "Mean (w)", w_median = "Median (w)", w_sum = "Sum (w)",
    w_perc = "% (w)", w_count = "Count (w)"
  )

  data %>%
    mutate(stat = unname(labels[stat])) %>%
    rename(Statistics = stat)
}


# ---- pivot_summary: reshape calc_stats() output -------------------------------

pivot_summary <- function(data) {

  bases <- data %>% pull(base_description) %>% unique() %>% paste0(collapse = " X ")
  if (any(!is.na(bases))) bases <- bases %>% prepare_base_for_table()

  if ("cross_break" %in% names(data)) {
    data <- data %>% filter(cross_break == "Total")
  }

  # calc_stats() always includes estimate_se, filled with "-" when conf
  # wasn't requested - drop that placeholder here so it isn't carried
  # through below as a real (but empty) SE column.
  if ("estimate_se" %in% names(data) && all(data$estimate_se == "-")) {
    data <- data %>% select(-estimate_se)
  }

  pivot_one_block <- function(block) {

    is_categorical <- !all(block$o_cat == block$stat)

    data_rows <- block %>%
      mutate(
        stat_code = stat,
        Variable  = if (is_categorical) o_cat else o_lab[1],
        row_type  = "data",
        Base      = base[1],
        block_id  = as.integer(.block[1])
      ) %>%
      rename(Estimate = estimate) %>%
      select(Variable, stat, Estimate, any_of(c("estimate_se", "estimate_ci")),
             Base, row_type, stat_code, block_id)

    if (!is_categorical) return(data_rows)

    # A label row carrying just the variable name, placed above its levels.
    # stat/Estimate/etc are copied from the first level row for now, purely
    # so tidy_statistic_description() below has a real value to match on (it
    # chokes on NA) - blanked for display in the last step below.
    label_row <- data_rows[1, ] %>%
      mutate(Variable = block$o_lab[1], row_type = "label")

    bind_rows(label_row, data_rows)
  }

  pivoted <- data %>%
    mutate(.block = fct_inorder(paste(outcome, stat, sep = "___"))) %>%
    group_split(.block) %>%
    map_df(pivot_one_block) %>%
    tidy_statistic_description() %>%
    rename(any_of(c(SE = "estimate_se", `95% CI` = "estimate_ci"))) %>%
    mutate(across(-c(Variable, row_type, stat_code, block_id),
                   ~ replace(.x, row_type == "label", NA)))

  list(pivoted, bases)
}


# ---- format_summary: style pivot_summary() output as a huxtable --------------
#
# Takes pivot_summary()'s own output (the list of pivoted data frame + bases
# footnote text) and returns a styled huxtable, ready to export via e.g.
# huxtable::quick_xlsx()/quick_docx(). Huxtable specifically, not flextable
# or gt - it's the one of the three that exports cleanly to Excel.
#
# Almost no merge_rows()/rowspan machinery here: pivot_summary() already put
# each *categorical* variable's label on its own explicit row, so there's
# nothing left to merge for those. The one exception is a numeric variable
# shown under more than one statistic (e.g. "x" for both Mean and Median) -
# there's no spare row to hold its label, so its Variable text repeats
# per-row and gets rowspan-merged back down to one cell instead, the same
# way pivot1h() merged repeated cells, just scoped to this one column and
# case (see the merge step below).
#
# NA formatting (agreed directly): non-statistic columns, and every column on
# a label row, stay the huxtable default (blank). Estimate/SE/95% CI render
# as "-" specifically when genuinely missing on a *data* row (e.g. a small
# subgroup median CI that grouped_medianci() dropped) - visually distinct
# from a label row's blank cells, which are structural, not missing data.
# Base is treated as non-statistic (a headcount, not a reported estimate) and
# stays blank if it's ever NA, though in practice it never is on a data row
# currently.
#
# Alignment: Variable and Statistics (both text) left-aligned; Estimate,
# SE/95% CI, and Base (all numbers or number-shaped) right-aligned.
#
# Digit formatting keys off stat_code, not column position: percentages as
# "12.3%" (Estimate/SE store the raw 0-1 proportion, hence the *100), counts
# and sums/weighted sums with thousands separators, means/medians to 1
# decimal place - same rules pivot1h() used, just looked up by name instead
# of position.
#
# Done as a plain-text transformation on the data frame, before as_hux() ever
# runs - not via huxtable's set_number_format(). One function
# (format_statistic()) covers Estimate, SE, and both halves of 95% CI: the CI
# string from calc_stats() (paste0(estimate_low, " - ", estimate_upp)) gets
# split on " - ", each half run through the same rule as Estimate, then
# rejoined - so a CI's precision now matches the Estimate next to it, rather
# than being stuck at whatever calc_stats() happened to produce. Doing this
# in plain R rather than through huxtable also means format_statistic() can
# be tested directly against known inputs/outputs, same as everything else in
# this file - only the alignment/border/colour/NA-string steps below actually
# need a real huxtable object to check.
#
# Alternating stripes are plain row-by-row zebra striping (huxtable's own
# stripe_rows()) - one row one colour, the next row the other, regardless of
# block boundaries. (An earlier version of this function struck the striping
# by block_id instead - one colour per whole variable - which was wrong;
# fixed back to plain per-row striping.)

# Vectorised: given a numeric vector and its parallel stat_code, returns the
# formatted display string for each element (NA in, NA out - never "NA" as
# text, so a blank/"-" na_string override downstream still applies cleanly).
format_statistic <- function(x, stat_code) {
  case_when(
    is.na(x) ~ NA_character_,
    stat_code %in% c("perc", "w_perc")                        ~ sprintf("%.1f%%", 100 * x),
    stat_code %in% c("count", "w_count")                      ~ formatC(x, digits = 0, big.mark = ",", format = "f"),
    stat_code %in% c("mean", "w_mean", "median", "w_median")  ~ formatC(x, digits = 1, format = "f"),
    stat_code %in% c("sum", "w_sum")                          ~ formatC(x, digits = 1, big.mark = ",", format = "f"),
    TRUE ~ formatC(x, format = "f")   # unrecognised stat_code - shouldn't happen in practice
  )
}

# Splits a "<low> - <high>" string built by calc_stats() (see
# apply_conf_columns() in simplified_functions.R) into its two bounds, runs
# each through format_statistic() using the same stat_code, and rejoins.
# Kept as its own function, separate from format_summary(), specifically so
# it can be tested directly against known input/output pairs without needing
# a huxtable at all.
format_ci_string <- function(ci_string, stat_code) {
  ci_parts <- str_split_fixed(ci_string, " - ", 2)
  low  <- format_statistic(as.numeric(ci_parts[, 1]), stat_code)
  high <- format_statistic(as.numeric(ci_parts[, 2]), stat_code)
  if_else(is.na(low) | is.na(high), NA_character_, paste0(low, " - ", high))
}

format_summary <- function(pivot_result) {

  data  <- pivot_result[[1]]
  bases <- pivot_result[[2]]

  # Pulled out before dropping them from the table - internal signals for the
  # formatting decisions below, not meant to be displayed.
  stat_code <- data$stat_code
  row_type  <- data$row_type

  # ---- digit formatting, as plain text, before the huxtable exists ----
  data$Estimate <- format_statistic(data$Estimate, stat_code)
  if ("SE" %in% names(data)) data$SE <- format_statistic(data$SE, stat_code)
  if ("95% CI" %in% names(data)) {
    data[["95% CI"]] <- format_ci_string(data[["95% CI"]], stat_code)
  }
  data$Base <- if_else(is.na(data$Base), NA_character_,
                        formatC(data$Base, digits = 0, big.mark = ",", format = "f"))

  ht <- data %>% select(-row_type, -stat_code, -block_id) %>% huxtable::as_hux(add_colnames = TRUE)

  no_of_rows <- nrow(ht)                     # includes the header row
  data_rows  <- which(row_type == "data") + 1

  est_col      <- which(names(ht) == "Estimate")
  se_or_ci     <- which(names(ht) %in% c("SE", "95% CI"))
  variable_col <- which(names(ht) == "Variable")

  # ---- NA formatting: blank by default, "-" for a genuinely missing statistic on a data row ----
  ht <- ht %>%
    huxtable::set_na_string(value = "") %>%
    huxtable::set_na_string(row = data_rows, col = c(est_col, se_or_ci), value = "-")

  # ---- alignment ----
  ht <- ht %>%
    huxtable::set_align(huxtable::everywhere, which(names(ht) %in% c("Variable", "Statistics")), "left") %>%
    huxtable::set_align(huxtable::everywhere, which(names(ht) %in% c("Estimate", "SE", "95% CI", "Base")), "right")

  # ---- merge repeated Variable text where one variable spans multiple ----
  # ---- statistic rows (e.g. "x" shown for both Mean and Median) ----
  # Only ever fires for that case in practice: categorical levels are always
  # distinct text (North/South/East, ...), so a run of literally identical
  # consecutive Variable values only happens when the same numeric variable
  # repeats across more than one statistic - no need to special-case by
  # variable type, this only ever finds something to merge where it should.
  # Label rows get a unique per-row key so one can never be pulled into a
  # merge with the data rows below it.
  merge_key   <- if_else(row_type == "data", data$Variable, paste0("__label__", seq_along(row_type)))
  run_lengths <- rle(merge_key)$lengths
  run_starts  <- cumsum(c(1, head(run_lengths, -1)))

  for (i in seq_along(run_starts)) {
    if (run_lengths[i] > 1) {
      start_row <- run_starts[i] + 1   # +1 for the header row offset
      span_len  <- run_lengths[i]
      ht <- huxtable::set_rowspan(ht, row = start_row, col = variable_col, value = span_len)
      for (r in (start_row + 1):(start_row + span_len - 1)) {
        ht[r, variable_col] <- ""
      }
    }
  }

  # ---- alternating background, one row one colour, next row the other ----
  # Plain row-by-row zebra striping - huxtable's own built-in function for
  # exactly this, applied to the whole table (header included for now; the
  # header styling step right after this one sets row 1's background back to
  # grey95 regardless, so whatever stripe_rows() put there doesn't matter).
  ht <- huxtable::stripe_rows(ht, stripe1 = "#f5f7fa", stripe2 = "#ffffff")

  # ---- minimal styling: small font, tight padding, light grey borders ----
  ht <- ht %>%
    huxtable::set_all_borders(huxtable::everywhere, huxtable::everywhere,
                               huxtable::brdr(0.5, "solid", "grey85")) %>%
    huxtable::set_bottom_border(1, huxtable::everywhere, huxtable::brdr(1, "solid", "grey40")) %>%
    huxtable::set_all_padding(1) %>%
    huxtable::set_font_size(8) %>%
    huxtable::set_font("Arial") %>%
    huxtable::set_bold(row = 1, col = huxtable::everywhere) %>%
    huxtable::set_background_color(1, huxtable::everywhere, "grey95") %>%
    huxtable::set_valign("middle")

  # ---- footnotes (unchanged from pivot1()/pivot1h()) ----
  if (all(unlist(bases) != "NA")) {
    for (i in seq_along(bases)) {
      ht <- ht %>% huxtable::add_footnote(bases[[i]], border = NULL)
    }
  }

  ht
}


# =============================================================================
# Initial tests
#
# I haven't run these myself - same caveat as the other test files in this
# project. Run with:
#
#   testthat::test_file(here::here("Scripts", "pivot_summary.R"))
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
    stat = "mean", estimate = 45, base = 100, base_description = NA_character_
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
    stat = "mean", estimate = 45, estimate_low = 40, estimate_upp = 50,
    estimate_ci = "40 - 50", base = 100, base_description = NA_character_
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
  expect_equal(result[["Estimate"]][north_row], "50.0%")
  expect_equal(result[["Estimate"]][south_row], "33.3%")
  expect_equal(result[["Estimate"]][east_row],  "16.7%")
  expect_true(is.na(result[["Estimate"]][region_row]))   # label row - structural, not missing data

  expect_equal(result[["Base"]][age_row],   "6")
  expect_equal(result[["Base"]][north_row], "6")

  # Alignment: Variable left, Estimate right, checked on a real body row.
  # align() is a huxtable accessor matrix, same access pattern as rowspan()
  # used in the merge_rows tests earlier in this file.
  variable_col <- which(names(result) == "Variable")
  estimate_col <- which(names(result) == "Estimate")
  expect_equal(huxtable::align(result)[age_row, variable_col], "left")
  expect_equal(huxtable::align(result)[age_row, estimate_col], "right")

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
  expect_equal(format_statistic(0.5,     "perc"),   "50.0%")
  expect_equal(format_statistic(0.16667, "w_perc"), "16.7%")
  expect_equal(format_statistic(1234,    "count"),  "1,234")
  expect_equal(format_statistic(41.6667, "mean"),   "41.7")
  expect_equal(format_statistic(35,      "median"), "35.0")
  expect_equal(format_statistic(115000,  "w_sum"),  "115,000.0")
  expect_true(is.na(format_statistic(NA_real_, "mean")))
})

test_that("12. format_ci_string() reformats both CI bounds to match Estimate's precision, preserving NA", {
  expect_equal(format_ci_string("40.234 - 52.891", "mean"), "40.2 - 52.9")
  expect_equal(format_ci_string("0.1234 - 0.5678", "perc"), "12.3% - 56.8%")
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
  expect_equal(result[["Estimate"]][north_row],  "50.0%")
  expect_equal(result[["Estimate"]][south_row],  "33.3%")
  expect_equal(result[["Estimate"]][east_row],   "16.7%")
  expect_true(is.na(result[["Estimate"]][region_row]))

  expect_true(all(result[["Base"]][c(age_row, income_row, north_row, south_row, east_row)] == "6"))

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
  variable_col <- which(names(result) == "Variable")
  expect_equal(result[["Variable"]][mean_pos],   "x")
  expect_equal(result[["Variable"]][median_pos], "")
  expect_equal(huxtable::rowspan(result)[mean_pos, variable_col], 2)
})
