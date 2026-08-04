# =============================================================================
# Tests for pivot_battery.R
# Same caveat as every other test file in this project: I haven't run these
# myself (no R in this environment). Run with:
#   testthat::test_file("tests/testthat/test-pivot_battery.R"), or devtools::test()
# Tests 1-3 cover the three validation errors (a predictor present, a
# non-categorical outcome, more than one statistic requested). Test 4 is the
# main hand-calculated case: two Likert-style items sharing one 3-level
# response scale, but with item2 having zero respondents in two of the three
# categories - the deliberate "not an error" case described in
# pivot_battery()'s header comment, confirming a missing category renders as
# a real NA (formatted "-") rather than blocking the whole table. Test 5
# covers format_battery()'s digit formatting, alignment, and the blank-
# header unit column (column 2, right after the stub column, holding "%"
# or "N" on every row) - category/Base column indices all shift right by
# one to make room for it.
# Test 6 is a bigger, more realistic-looking battery - a 4-item customer
# satisfaction scale, 24 respondents, real variable labels, and a couple of
# genuinely skipped responses on one item - meant to actually look like a
# battery grid when printed, not just exercise the smallest case that proves
# a rule. Correctness isn't hand-calculated here (24 rows x 4 items isn't
# something to hand-trace reliably) - instead it's cross-checked against an
# independent table()/prop.table() computation run directly on the same
# source data, with the formatted-string comparison routed through
# format_statistic() itself (imported via pivot_summary.R) rather than a
# second, hand-rolled rounding rule - so there's no risk of the test's own
# rounding disagreeing with sprintf("%.0f", ...)'s on some awkward fraction
# (e.g. 3/24 = 12.5%, sat right on a rounding tie).
# Test 7 covers common_prefix_stem()/find_common_stem() directly (the R port of
# myutils.py's find_common_stem() - leading-numbering strip, min_length
# floor, last-space trim, and the extra fewer-than-2-labels guard this
# version adds that the Python one doesn't need). Test 8 checks
# pivot_battery() actually uses it end to end: a battery whose labels share
# a real >25-character stem gets that stem stripped from every row's
# Variable text and promoted to the stub column's own header; test 6 above
# (re-checked, not modified) already covers the "no shared stem" case
# staying exactly as before, since none of its four labels share a
# qualifying common prefix across all of them.
# Test 9 covers the stat %in% c("perc", "count", "w_perc", "w_count") check
# added alongside the existing outcome_type check - can't be reached via a
# real calc_stats() call (see pivot_battery()'s own note on why), so it's
# tested against a hand-built data frame that deliberately breaks the
# outcome_type/stat pairing calc_stats() itself would never produce.
# Test 10 directly answers "does the blank-header unit column interfere
# with the label-stem stripping?" - no: they're independent steps at two
# different layers (stem detection/stripping runs in pivot_battery(), on
# the raw labels, before format_battery() ever sees them; the unit column
# is inserted by format_battery() right after whatever the stub column
# ends up called, stem or not). Uses statistics = "count" specifically (not
# "perc", like test 8) to show the unit column correctly tracks the actual
# requested statistic ("N", not "%") independently of the stem logic,
# rather than the two tests only ever exercising percentages together with
# a stem.
# NOTE ON EARLIER VERSIONS: tests 5/6/8/10 went through three prior
# designs - a "(%)"/"(N)" suffix on the stub column's own header text, a
# row above the real header, then a row below it in the table body -
# before settling on the current one: a narrow extra COLUMN, right after
# the stub column, blank header (a single space, not the real column
# names), holding "%"/"N" (+ " (w)" if weighted) on every row. Updated
# below; every category/Base column index shifts right by one from where
# it would be without this column.
# =============================================================================


test_that("1. pivot_battery() rejects a predictor", {
  data <- tibble(
    item1 = factor(c("Agree", "Disagree", "Agree", "Disagree")),
    grp   = factor(c("A", "A", "B", "B"))
  )
  stats_table <- calc_stats(data, outcomes = "item1", predictors = "grp",
                             statistics = "perc", multicode = FALSE)

  expect_error(pivot_battery(stats_table), "no room for a predictor")
})


test_that("2. pivot_battery() rejects a non-categorical (numeric) outcome", {
  data <- tibble(age = c(1, 2, 3))
  stats_table <- calc_stats(data, outcomes = "age", statistics = "mean")

  expect_error(pivot_battery(stats_table), "categorical")
})


test_that("3. pivot_battery() rejects more than one statistic per call", {
  data <- tibble(item1 = factor(c("Agree", "Disagree")))
  stats_table <- calc_stats(data, outcomes = "item1", statistics = c("perc", "count"),
                             multicode = FALSE)

  expect_error(pivot_battery(stats_table), "exactly one statistic")
})


# item1: Agree, Agree, Neutral, Disagree (levels Disagree/Neutral/Agree) ->
#   Disagree = 1/4 = 25%, Neutral = 1/4 = 25%, Agree = 2/4 = 50%. Base = 4.
# item2: Agree, Agree, Agree, NA (same 3 declared levels, but Disagree/
#   Neutral never actually occur, and the NA drops out of item2's own
#   complete-case base) -> Agree = 3/3 = 100%, Base = 3. Disagree/Neutral
#   never appear as rows for item2 at all (dplyr::group_by() drops unused
#   factor levels by default) - after pivot_wider() these come back as real
#   NAs, not zeros, which is the intended, documented behaviour, not a bug.
#
# Column order: Disagree, Neutral, Agree - item1's own factor level order,
# since item1 contributes all three categories to the first-appearance scan
# and item2 only ever adds one that's already present.

test_that("4. pivot_battery() matches hand-calculated percentages, with a missing category rendering as NA, not an error", {
  data <- tibble(
    item1 = factor(c("Agree", "Agree", "Neutral", "Disagree"),
                    levels = c("Disagree", "Neutral", "Agree")),
    item2 = factor(c("Agree", "Agree", "Agree", NA),
                    levels = c("Disagree", "Neutral", "Agree"))
  )
  stats_table <- calc_stats(data, outcomes = c("item1", "item2"),
                             statistics = "perc", multicode = FALSE)
  result <- pivot_battery(stats_table)[[1]]

  expect_equal(names(result), c("Variable", "Disagree", "Neutral", "Agree", "Base"))
  expect_equal(result$Variable, c("item1", "item2"))

  item1_row <- result %>% filter(Variable == "item1")
  item2_row <- result %>% filter(Variable == "item2")

  expect_equal(item1_row$Disagree, 0.25)
  expect_equal(item1_row$Neutral, 0.25)
  expect_equal(item1_row$Agree, 0.5)
  expect_equal(item1_row$Base, 4)

  expect_true(is.na(item2_row$Disagree))
  expect_true(is.na(item2_row$Neutral))
  expect_equal(item2_row$Agree, 1)
  expect_equal(item2_row$Base, 3)
})


test_that("5. format_battery() formats percentages as bare whole numbers, missing categories as \"-\", aligns Variable left / categories right, and adds a centered, blank-header \"%\" unit column right after Variable", {
  data <- tibble(
    item1 = factor(c("Agree", "Agree", "Neutral", "Disagree"),
                    levels = c("Disagree", "Neutral", "Agree")),
    item2 = factor(c("Agree", "Agree", "Agree", NA),
                    levels = c("Disagree", "Neutral", "Agree"))
  )
  stats_table <- calc_stats(data, outcomes = c("item1", "item2"),
                             statistics = "perc", multicode = FALSE)
  pivoted <- pivot_battery(stats_table)
  result  <- format_battery(pivoted)
  print(huxtable::as_flextable(result))

  # Row 1 is the header text itself (as_hux(add_colnames = TRUE)) - back to
  # a single header row, no extra row anywhere - the two real data rows are
  # rows 2 (item1) and 3 (item2).
  expect_equal(result[["Disagree"]][2], "25")
  expect_equal(result[["Neutral"]][2], "25")
  expect_equal(result[["Agree"]][2], "50")
  expect_equal(result[["Base"]][2], "4")

  # set_na_string() is a DISPLAY instruction, not a data mutation - the
  # underlying cell stays real NA regardless of what na_string is set to
  # (same "stored vs displayed" split huxtable's rowspan/merge behaviour
  # already caught elsewhere in this project - see pivot_summary.R's note on
  # set_rowspan()). Plain `result[["Disagree"]][3]` therefore still reads
  # NA, not "-": confirmed by an actual test run, not assumed. Checked two
  # ways instead - the cell really is missing, AND na_string() was actually
  # told to render it as "-" - rather than asserting on `[[` output that
  # huxtable was never going to change.
  expect_true(is.na(result[["Disagree"]][3]))
  expect_true(is.na(result[["Neutral"]][3]))
  expect_equal(result[["Agree"]][3], "100")
  expect_equal(result[["Base"]][3], "3")

  variable_col  <- 1
  disagree_col  <- which(names(result) == "Disagree")
  neutral_col   <- which(names(result) == "Neutral")
  agree_col     <- which(names(result) == "Agree")
  expect_equal(huxtable::na_string(result)[3, disagree_col], "-")
  expect_equal(huxtable::na_string(result)[3, neutral_col], "-")

  expect_equal(huxtable::align(result)[2, variable_col], "left")
  expect_equal(huxtable::align(result)[2, agree_col], "right")

  # Stub column header is plain "Variable" - the "what do these numbers
  # mean" indicator lives in its own column now, not appended to this
  # header text (see format_battery()'s own note on the two earlier
  # designs this replaced).
  expect_equal(names(result)[1], "Variable")

  # The unit column: column 2, header text is a single space (not "" -
  # see format_battery()'s note on why), "%": on every data row, centered.
  # Every other column index shifts right by one to make room for it -
  # Disagree/Neutral/Agree/Base above are all found via which(), so they
  # already reflect that shift correctly without needing separate handling
  # here.
  unit_col <- 2
  expect_equal(names(result)[unit_col], " ")
  expect_equal(result[[unit_col]][1], " ")     # header row - blank-looking, not real column-name text
  expect_equal(result[[unit_col]][2], "%")      # item1's data row
  expect_equal(result[[unit_col]][3], "%")      # item2's data row
  expect_equal(huxtable::align(result)[2, unit_col], "center")
})


test_that("6. pivot_battery()/format_battery() on a larger, realistic-looking 4-item satisfaction battery", {
  set.seed(42)
  n <- 24
  likert_levels <- c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")

  draw_item <- function(probs) {
    factor(sample(likert_levels, n, replace = TRUE, prob = probs), levels = likert_levels)
  }

  data <- tibble(
    ease      = draw_item(c(0.05, 0.10, 0.15, 0.40, 0.30)),
    value     = draw_item(c(0.10, 0.20, 0.25, 0.30, 0.15)),
    recommend = draw_item(c(0.05, 0.05, 0.10, 0.35, 0.45)),
    support   = draw_item(c(0.15, 0.15, 0.20, 0.30, 0.20))
  )
  # A couple of realistic skipped responses on one item - support's own base
  # should come out lower than the other three items' as a result.
  data$support[c(3, 17)] <- NA

  attr(data$ease, "label")      <- "The product is easy to use"
  attr(data$value, "label")     <- "The product is good value for money"
  attr(data$recommend, "label") <- "I would recommend this product to a friend"
  attr(data$support, "label")   <- "Customer support was helpful"

  stats_table <- calc_stats(data, outcomes = c("ease", "value", "recommend", "support"),
                             statistics = "perc", multicode = FALSE)
  pivoted <- pivot_battery(stats_table)
  raw     <- pivoted[[1]]
  result  <- format_battery(pivoted)

  cat("\n==== Test 6: 4-item satisfaction battery ====\n")
  print(huxtable::as_flextable(result))

  expect_equal(nrow(raw), 4)
  # All 5 columns should appear given ~96 draws total across 4 items at
  # probabilities that are never 0 for any level - not asserted as a strict
  # equality against likert_levels, though, since that would make this test
  # fragile to the (unlikely but real) chance that one level happens to get
  # zero observations across every single item, which would legitimately
  # (and correctly) mean that column doesn't exist at all - see
  # pivot_battery()'s header note on why a missing category isn't an error.
  expect_true(all(likert_levels %in% names(raw)))
  expect_true(all(c("Variable", "Base") %in% names(raw)))
  expect_equal(raw$Variable, c(
    "The product is easy to use", "The product is good value for money",
    "I would recommend this product to a friend", "Customer support was helpful"
  ))

  # Cross-checked against an independent table()/prop.table() computation on
  # the same source data - not a second hand-typed set of expected numbers -
  # so this doesn't depend on being able to hand-trace what set.seed(42)
  # actually drew.
  check_item <- function(col_name, label) {
    observed <- data[[col_name]][!is.na(data[[col_name]])]
    counts   <- table(factor(observed, levels = likert_levels))
    expected_props <- setNames(as.numeric(counts) / length(observed), likert_levels)

    row <- raw %>% filter(Variable == label)
    # Column 1, not `result[["Variable"]]` - pivot_battery()'s common-stem
    # step (not relevant for this particular battery, but a safe habit
    # regardless) can rename the stub column away from "Variable", so
    # `result` (unlike `raw`, the pre-formatting pivot-level tibble just
    # above) isn't guaranteed to have a column literally called that.
    # Column 1 stays the stub column either way - format_battery()'s unit
    # column (see its own note) is inserted at position 2, not 1, so it
    # doesn't disturb this lookup.
    formatted_row <- result[result[[1]] == label, ]

    for (lvl in likert_levels) {
      if (expected_props[[lvl]] == 0) {
        # A category with zero respondents never gets a row at all
        # (dplyr::group_by() drops unused factor levels by default - see
        # pivot_battery()'s header note) - real NA, not a formatted "0".
        expect_true(is.na(row[[lvl]]))
      } else {
        expect_equal(row[[lvl]], expected_props[[lvl]], tolerance = 1e-9)
        # Routed through format_statistic() itself, same as format_battery()
        # does internally - not a second, hand-rolled rounding rule that
        # could disagree with sprintf("%.0f", ...) on an awkward fraction.
        expect_equal(formatted_row[[lvl]], format_statistic(expected_props[[lvl]], "perc"))
      }
    }
    expect_equal(row$Base, length(observed))
  }

  check_item("ease", "The product is easy to use")
  check_item("value", "The product is good value for money")
  check_item("recommend", "I would recommend this product to a friend")
  check_item("support", "Customer support was helpful")

  # support had 2 of 24 responses set to NA above - its base should be lower
  # than the other three items' (24), not silently sharing one table-wide N.
  support_base <- raw %>% filter(Variable == "Customer support was helpful") %>% pull(Base)
  expect_equal(support_base, 22)
  other_bases <- raw %>% filter(Variable != "Customer support was helpful") %>% pull(Base)
  expect_true(all(other_bases == 24))

  # Stub header stays plain "Variable" - the indicator lives in its own
  # blank-header column instead (column 2, right after Variable - see test
  # 5's note), not appended to the header text.
  expect_equal(names(result)[1], "Variable")
  expect_equal(names(result)[2], " ")
  expect_equal(result[[2]][2], "%")   # first item's data row (row 2, header is row 1)
  expect_equal(huxtable::align(result)[2, 2], "center")
})


test_that("7. common_prefix_stem() and find_common_stem() match hand-worked cases", {

  # ---- common_prefix_stem(): plain character-level longest common prefix ----
  # Renamed from common_prefix() - that name collided with calc_stats.R's
  # OWN, differently-implemented common_prefix() (used for multicode
  # variable-name-stem detection); sourcing both files let this one silently
  # overwrite that one, since both used to define a same-named function in
  # the same environment. See this function's own header note in
  # pivot_battery.R.
  expect_equal(common_prefix_stem(c("apple pie", "apple tart")), "apple ")
  expect_equal(common_prefix_stem(c("abc", "abd", "abe")), "ab")
  expect_equal(common_prefix_stem(c("abc", "xyz")), "")
  expect_equal(common_prefix_stem("abc"), "abc")   # single string - its own "common prefix"

  # ---- find_common_stem(): built from a shared_stem variable, not a second ----
  # ---- hand-typed copy of the same text - so the expectation can't drift ----
  # ---- out of sync with what the test data actually shares -----------------
  shared_stem <- "Please rate your satisfaction with the following aspects of our service:"
  expect_true(nchar(shared_stem) > 25)   # confirms this test actually exercises the min_length floor, not just clears it by luck

  labels <- c(paste0(shared_stem, " Speed"),
              paste0(shared_stem, " Price"),
              paste0(shared_stem, " Support"))
  expect_equal(find_common_stem(labels), shared_stem)

  # Same shared_stem, but each label carries its own leading item number
  # first ("1. ", "2. ", "3. ") - confirms the numbering-strip regex runs
  # BEFORE the common-prefix comparison, not after, since the numbers
  # themselves obviously don't match each other and would otherwise block
  # any stem from being found at all.
  numbered_labels <- c(paste0("1. ", shared_stem, " Speed"),
                        paste0("2. ", shared_stem, " Price"),
                        paste0("3. ", shared_stem, " Support"))
  expect_equal(find_common_stem(numbered_labels), shared_stem)

  # Short shared prefix (well under min_length) - correctly reports no stem,
  # not a truncated one.
  expect_equal(find_common_stem(c("12a. Yes please", "12b. No thanks")), "")

  # Fewer than 2 labels - the guard find_common_stem() has that the ported-
  # from-Python common_prefix_stem() primitive doesn't (see its own header note):
  # a single string's "common prefix with itself" is the whole string, which
  # would otherwise strip a lone battery item down to nothing.
  expect_equal(find_common_stem("Just one label here, over 25 characters long"), "")
  expect_equal(find_common_stem(character(0)), "")
})


test_that("8. pivot_battery() strips a shared label stem and renames the stub column to it", {
  shared_stem <- "Please rate your satisfaction with the following aspects of our service:"

  data <- tibble(
    speed   = factor(c("Good", "Good", "Poor"), levels = c("Poor", "Good")),
    price   = factor(c("Poor", "Good", "Good"), levels = c("Poor", "Good")),
    support = factor(c("Good", "Good", "Good"), levels = c("Poor", "Good"))
  )
  attr(data$speed, "label")   <- paste0(shared_stem, " Speed")
  attr(data$price, "label")   <- paste0(shared_stem, " Price")
  attr(data$support, "label") <- paste0(shared_stem, " Support")

  stats_table <- calc_stats(data, outcomes = c("speed", "price", "support"),
                             statistics = "perc", multicode = FALSE)
  pivoted <- pivot_battery(stats_table)
  raw     <- pivoted[[1]]
  result  <- format_battery(pivoted)
  print(huxtable::as_flextable(result))

  # The stub column is no longer literally called "Variable" - it's been
  # renamed to the detected stem - and each row now holds just the unique
  # remainder, not the full original label.
  expect_false("Variable" %in% names(raw))
  expect_true(shared_stem %in% names(raw))
  expect_equal(raw[[shared_stem]], c("Speed", "Price", "Support"))

  # format_battery() no longer looks up the stub column by the name
  # "Variable" (see its own header note - it uses column position 1
  # instead), so the same rename needs to have actually made it through to
  # the finished huxtable's own header text, not just the plain data frame.
  # Plain stem, no suffix - the unit column carries the indicator instead
  # (column 2, right after this one - see test 5's note), not appended to
  # the header text itself.
  expect_equal(names(result)[1], shared_stem)
  expect_equal(result[[shared_stem]][1], shared_stem)   # as_hux(add_colnames = TRUE)'s own header row
  expect_equal(result[[shared_stem]][2], "Speed")         # first real data row

  variable_col <- 1
  expect_equal(huxtable::align(result)[2, variable_col], "left")

  # The unit column sits at position 2, right after the stub column -
  # shifting every category/Base column one to the right of where it'd be
  # without it. "%" shown on every data row, centered.
  unit_col <- 2
  expect_equal(names(result)[unit_col], " ")
  expect_equal(result[[unit_col]][2], "%")
  expect_equal(huxtable::align(result)[2, unit_col], "center")

  # category_order here is Poor, Good (speed's own factor level order,
  # contributing both categories to the first-appearance scan) - speed's
  # own Good = 2/3 = 66.67% -> "67" (format_statistic()'s 0dp rounding).
  good_col <- which(names(result) == "Good")
  expect_equal(result[[good_col]][2], "67")
  expect_equal(huxtable::align(result)[2, good_col], "right")
})


test_that("9. pivot_battery() rejects a stat outside perc/count/w_perc/w_count, even when outcome_type looks categorical", {
  # calc_stats() can never actually produce this row shape itself - factor
  # outcomes only ever get perc/count/w_perc/w_count computed for them (see
  # calc_stats.R's stat_registry/group_on_outcome fork) - so this is built
  # by hand rather than via a real calc_stats() call, specifically to
  # exercise the belt-and-braces stat check on its own, independent of the
  # outcome_type check right above it in pivot_battery().
  malformed <- tibble(
    outcome = "item1", o_lab = "item1", o_cat = "Yes", stat = "mean",
    outcome_type = "categorical", estimate = 0.5, base = 10,
    cross_break = "Total"
  )

  expect_error(pivot_battery(malformed), "perc, count, w_perc, or w_count")
})


test_that("10. label-stem stripping and the blank-header unit column compose correctly, without interfering with each other", {
  shared_stem <- "How would you rate our performance on the following areas:"
  expect_true(nchar(shared_stem) > 25)

  data <- tibble(
    speed = factor(c("Good", "Good", "Poor"), levels = c("Poor", "Good")),
    price = factor(c("Poor", "Good", "Good"), levels = c("Poor", "Good"))
  )
  attr(data$speed, "label") <- paste0(shared_stem, " Speed")
  attr(data$price, "label") <- paste0(shared_stem, " Price")

  # "count", not "perc" (unlike test 8) - the point of this test is to show
  # the unit column tracks whichever statistic was actually requested ("N",
  # not "%") independently of the stem logic, which never looks at
  # stat_code at all.
  stats_table <- calc_stats(data, outcomes = c("speed", "price"),
                             statistics = "count", multicode = FALSE)
  pivoted <- pivot_battery(stats_table)
  raw     <- pivoted[[1]]
  result  <- format_battery(pivoted)
  print(huxtable::as_flextable(result))

  # Stem detection/stripping already happened at the pivot level (raw, from
  # pivot_battery() alone) - unaffected by which statistic was requested,
  # and by definition unaffected by format_battery(), which hasn't run yet
  # at this point.
  expect_true(shared_stem %in% names(raw))
  expect_false("Variable" %in% names(raw))
  expect_equal(raw[[shared_stem]], c("Speed", "Price"))

  # The finished huxtable carries both pieces together, at two different
  # layers, rather than one clobbering the other: the header (row 1) is the
  # stem, untouched and un-suffixed; the unit column (column 2) separately
  # reads "N" on every data row.
  expect_equal(names(result)[1], shared_stem)
  expect_equal(result[[shared_stem]][1], shared_stem)   # as_hux(add_colnames = TRUE)'s own header row
  expect_equal(result[[shared_stem]][2], "Speed")         # first real data row

  unit_col <- 2
  expect_equal(names(result)[unit_col], " ")
  expect_equal(result[[unit_col]][2], "N")
  expect_equal(huxtable::align(result)[2, unit_col], "center")

  # category_order here is Poor, Good (same reasoning as test 8) - speed's
  # own Good count = 2 -> "2" (formatC() with no decimals for "count").
  good_col <- which(names(result) == "Good")
  expect_equal(result[[good_col]][2], "2")
  expect_equal(huxtable::align(result)[2, good_col], "right")
})


# Tests 11-13: SE/CI support, added later than the rest of this file - see
# pivot_battery.R's own header note ("ADDED LATER, not part of the original
# design"). Ported mechanism from format_crosstab()'s own conf row, not a
# new design - these tests confirm the port works in THIS file's shape
# (category columns instead of predictor-level columns, unit column reused
# for the row label instead of a Statistics column, rowspan merge on the
# stub column instead of format_crosstab()'s own equivalent).
#
# Reuses the item1/item2 data from tests 4/5 (real, hand-verified
# percentages: item1 Disagree/Neutral/Agree = 25/25/50 on Base 4; item2
# Agree = 100 on Base 3, Disagree/Neutral never appear as rows at all) - the
# exact SE arithmetic itself is unweighted_perc()'s own concern, already
# covered by its own tests, so these check placement/labelling/structure,
# same restraint test 20 in test_pivot_crosstab.R uses for its own
# legend/sig_diff row (checked against what the function itself produced,
# not a second hand-derived expected value).

test_that("11. pivot_battery() carries \"<category>__conf\" columns and conf_type when conf was requested, and neither when it wasn't (backward compatibility)", {
  data <- tibble(
    item1 = factor(c("Agree", "Agree", "Neutral", "Disagree"),
                    levels = c("Disagree", "Neutral", "Agree")),
    item2 = factor(c("Agree", "Agree", "Agree", NA),
                    levels = c("Disagree", "Neutral", "Agree"))
  )
  with_conf <- calc_stats(data, outcomes = c("item1", "item2"),
                           statistics = "perc", conf = "se", multicode = FALSE)
  without_conf <- calc_stats(data, outcomes = c("item1", "item2"),
                              statistics = "perc", multicode = FALSE)

  pivoted_with    <- pivot_battery(with_conf)
  pivoted_without <- pivot_battery(without_conf)

  expect_equal(pivoted_with[[4]], "se")
  expect_true(all(paste0(c("Disagree", "Neutral", "Agree"), "__conf") %in% names(pivoted_with[[1]])))

  expect_true(is.na(pivoted_without[[4]]))
  expect_false(any(stringr::str_detect(names(pivoted_without[[1]]), "__conf$")))
})


test_that("12. format_battery() inserts an SE row under each item, reusing the unit column for its label, and merges the stub column across the pair", {
  data <- tibble(
    item1 = factor(c("Agree", "Agree", "Neutral", "Disagree"),
                    levels = c("Disagree", "Neutral", "Agree")),
    item2 = factor(c("Agree", "Agree", "Agree", NA),
                    levels = c("Disagree", "Neutral", "Agree"))
  )
  stats_table <- calc_stats(data, outcomes = c("item1", "item2"),
                             statistics = "perc", conf = "se", multicode = FALSE)
  pivoted <- pivot_battery(stats_table)
  result  <- format_battery(pivoted)
  print(huxtable::as_flextable(result))

  # Row 1 is the header text (as_hux(add_colnames = TRUE)). item1's data row
  # is row 2, its SE row directly below it is row 3; item2's data row is
  # row 4, its SE row is row 5.
  expect_equal(result[[1]][2], "item1")
  expect_equal(result[[2]][2], "%")
  expect_equal(result[[2]][3], "SE")
  expect_equal(result[[1]][4], "item2")
  expect_equal(result[[2]][4], "%")
  expect_equal(result[[2]][5], "SE")

  # The SE row's category cells hold the same conf values pivot_battery()
  # itself built - cross-checked against the raw pivoted table (element 1),
  # routed through format_statistic() the same way format_battery() does
  # internally, not a second hand-rolled rounding rule.
  raw <- pivoted[[1]]
  disagree_conf_formatted <- format_statistic(raw$Disagree__conf[raw$Variable == "item1"], "perc")
  expect_equal(result[["Disagree"]][3], disagree_conf_formatted)

  # Base is blank on the SE row - it doesn't carry its own base. Checked two
  # ways, same "stored vs displayed" split test 5 already establishes for
  # this file: the underlying cell really is NA, AND na_string() was told to
  # render it as "-" (the table-wide default every cell already gets).
  expect_true(is.na(result[["Base"]][3]))
  expect_equal(huxtable::na_string(result)[3, which(names(result) == "Base")], "-")

  # Stub column merged across item1's data + SE row pair - a rowspan of 2
  # anchored at the data row, same mechanic format_crosstab() uses for its
  # own numeric-variable data+conf rows.
  expect_equal(huxtable::rowspan(result)[2, 1], 2)
  expect_equal(huxtable::rowspan(result)[4, 1], 2)
})


test_that("13. format_battery() with conf = \"ci\" labels the inserted row \"95% CI\" instead of \"SE\"", {
  data <- tibble(
    item1 = factor(c("Agree", "Agree", "Neutral", "Disagree"),
                    levels = c("Disagree", "Neutral", "Agree"))
  )
  stats_table <- calc_stats(data, outcomes = "item1", statistics = "perc",
                             conf = "ci", multicode = FALSE)
  result <- format_battery(pivot_battery(stats_table))
  print(huxtable::as_flextable(result))

  expect_equal(result[[2]][2], "%")
  expect_equal(result[[2]][3], "95% CI")
  expect_equal(huxtable::rowspan(result)[2, 1], 2)
})
