# =============================================================================
# pivot_battery() / format_battery() — a "battery grid" table: rows are items
# (multiple outcome variables that share one response scale, e.g. a set of
# Likert statements), columns are the shared response categories, one % (or
# N) per cell. R equivalent of Python's freqs()/crosstab_wide() combination
# in myutils.py, but reshaped wide (categories as columns) rather than
# stacked long - the layout actually used for presenting a battery of
# same-scale items together, which neither pivot_summary() (stacks
# categories as rows, one block per item) nor pivot_crosstab() (columns are
# predictor levels, not response categories) can produce.
#
# Takes calc_stats() output directly as `data`, same as pivot_summary()/
# pivot_crosstab() - NOT pivot_numeric_summary()'s pattern of calling
# calc_stats() itself, since there's no weighted/unweighted stat-code choice
# here that has to happen before calc_stats() runs; perc/w_perc/count/
# w_count are just passed straight through as `statistics`.
#
# SCOPE, agreed by the same reasoning pivot_numeric_summary() already used
# for its own "no predictor" restriction: a battery grid's two axes (item,
# category) are already both spoken for, same as pivot_numeric_summary()'s
# (variable, statistic) - there's no room left for a predictor/crossbreak
# third axis without the table turning into something else entirely. Call
# calc_stats() with predictors = NULL (pivot_battery() checks this and
# errors otherwise, rather than silently ignoring predictor columns).
#
# Statistics: exactly ONE per call (perc, count, w_perc, or w_count) - not
# pivot_crosstab()'s "Variable + Statistics" pattern of stacking several
# statistics as extra rows. A battery grid mixing % and N in the same table
# isn't a layout anyone asks for in practice (matches Python's
# crosstab_wide(), which takes a single `value = 'percent'/'count'` per
# call, not both at once) - if you want both, call pivot_battery() twice.
# Enforced by checking length(unique(data$stat)) == 1 below.
#
# Category set/order: NOT validated against every item sharing an identical
# set of response categories - deliberately. dplyr::group_by() drops unused
# factor levels by default, so an item with zero respondents in one category
# legitimately produces no row for that category at all; that's expected,
# not an error, and the resulting cell should read "-" (handled below), not
# trigger a hard stop. What WOULD be a real problem - items using genuinely
# different response scales with little or no overlap - isn't distinguished
# from "one item happens to have a zero-count category" by any check here;
# column order is simply the union of every category that appears anywhere,
# in first-appearance order across the whole stacked table (fct_inorder()),
# which behaves sensibly either way without guessing at a similarity
# threshold that would be arbitrary regardless of where it was set.
#
# Common-stem detection: battery item labels often share a common stem
# ("Satisfaction with: Speed", "Satisfaction with: Price", ...) - R port of
# find_common_stem() in your myutils.py (same leading-item-numbering regex,
# same min_length/last-space-trim rules), used the same way
# stacked_bar_freqs() uses it there: strip the stem from each label, leaving
# just the unique remainder, and surface the stem itself rather than
# discarding it. No chart title to put it in here, so it becomes the row-
# stub column's own header instead of the generic "Variable" - see
# find_common_stem()/pivot_battery() below. Runs automatically (not behind
# an argument), same as the Python version - min_length's 25-character floor
# already guards against a spurious short match.
#
# SE/CI: ADDED LATER, not part of the original design (that original scope
# decision - "no natural home for a per-cell SE/CI, point estimates only,
# same as pivot_numeric_summary()" - is still visible in git history/this
# comment's own earlier revisions). calc_stats(conf = "se"/"ci") output is
# now carried through as "<category>__conf" columns (pivot_battery(), see
# its own note) and rendered as an extra inserted row under each item that
# has one (format_battery(), see its own note) - same mechanic
# format_crosstab() already uses for its own conf row, not a new design.
# Reuses the existing blank-header unit column to hold "SE"/"95% CI" on that
# one row instead of adding a whole new column just for it.
# =============================================================================



# ---- common_prefix_stem: character-level longest common prefix across strings ----
# Named common_prefix_stem(), not the more obvious common_prefix() - that
# name collides with calc_stats.R's OWN common_prefix() (used by
# convert_multicodes() for multicode variable-NAME-stem detection, a
# different implementation for a different purpose). Since this file sources
# calc_stats.R at its own top and used to define a same-named function
# afterward in the same environment, sourcing both silently let this one
# overwrite calc_stats.R's - the two agree on ordinary inputs, but disagree
# on an empty character vector (calc_stats.R's errors, this one returns ""),
# so which one actually ran depended on source order, invisibly. Renamed to
# remove the collision outright rather than relying on load order being
# right.
#
# Base-R equivalent of Python's os.path.commonprefix() (used, on plain
# strings rather than paths, inside myutils.py's find_common_stem()) - no
# built-in R function does this directly. Compares character-by-character up
# to the shortest string's length; two or fewer strings still work (a single
# string's "common prefix" is itself, which find_common_stem() below guards
# against separately - see its own note on why that guard has to live there,
# not here).
common_prefix_stem <- function(strings) {
  strings <- as.character(strings)
  if (length(strings) == 0) return("")
  if (length(strings) == 1) return(strings)

  min_len <- min(stringr::str_length(strings))
  if (min_len == 0) return("")

  char_matrix <- do.call(rbind, strsplit(stringr::str_sub(strings, 1, min_len), ""))
  same_all_the_way <- apply(char_matrix, 2, function(col) all(col == col[1]))

  prefix_len <- if (all(same_all_the_way)) min_len else which(!same_all_the_way)[1] - 1
  stringr::str_sub(strings[1], 1, prefix_len)
}

# ---- find_common_stem: R port of myutils.py's find_common_stem() ------------
# Strips a leading item-numbering prefix ("12a. ") before comparing, finds
# the longest common prefix across the (numbering-stripped) labels, rejects
# it if too short to be meaningful (min_length), and trims back to the last
# space so the stem never ends mid-word - same three steps, same order, as
# the Python version.
#
# One guard the Python version doesn't have: fewer than 2 labels. A single
# string's "common prefix with itself" is the whole string
# (os.path.commonprefix(['x']) == 'x', and common_prefix_stem() above
# matches that for consistency) - fine as a general-purpose primitive, but wrong to
# then treat as a shared "stem" and strip out entirely, which would leave a
# lone battery item with an empty label. Python's callers never seem to hit
# this (stacked_bar_freqs() only ever runs across multiple grouped
# variables), but pivot_battery() has no such guarantee, so it's checked
# explicitly here rather than silently inherited.
find_common_stem <- function(labels, min_length = 25) {
  labels <- as.character(labels)
  if (length(labels) < 2) return("")

  stripped <- stringr::str_remove(labels, "^\\s*\\d+[a-zA-Z]?\\.\\s*")
  stem <- common_prefix_stem(stripped)
  if (stringr::str_length(stem) <= min_length) return("")

  space_positions <- stringr::str_locate_all(stem, " ")[[1]]
  if (nrow(space_positions) > 0) {
    stem <- stringr::str_sub(stem, 1, max(space_positions[, "start"]) - 1)
  }
  stem
}


# ---- pivot_battery: reshape calc_stats() output wide, categories as columns ----

#' Reshape `calc_stats()` output into a battery/grid table (items x category
#' levels)
#'
#' For a set of outcome variables sharing consistent category levels (e.g.
#' a Likert battery, Strongly agree...Strongly disagree) - one row per
#' item, one column per category level, with a common item-label stem
#' detected and stripped automatically (via `find_common_stem()`) where
#' possible. Only one statistic per call (e.g. `"perc"`); `Total` cross-break
#' only, like [pivot_numeric_summary()].
#'
#' @param data A tibble as returned by [calc_stats()], called with a
#'   consistent category-level outcome set and a single statistic.
#'
#' @return A list: `pivoted`, `stat_code`, `category_order`, and
#'   `conf_type` ("se", "ci", or `NA`). Pass this list straight to
#'   [format_battery()].
#'
#' @seealso [format_battery()], [make_battery_table()], [calc_stats()]
#' @keywords internal
pivot_battery <- function(data) {

  if ("cross_break" %in% names(data) && !all(data$cross_break == "Total")) {
    stop("pivot_battery() has no room for a predictor - every row must be cross_break == \"Total\". ",
         "Call calc_stats() with predictors = NULL, or use pivot_crosstab() instead.")
  }

  if (!all(data$outcome_type %in% c("categorical", "multicoded"))) {
    stop("pivot_battery() is for categorical items sharing a response scale - ",
         "every outcome must be outcome_type \"categorical\" or \"multicoded\" ",
         "(request perc/count/w_perc/w_count, not mean/median/sum/...).")
  }

  # Belt-and-braces alongside the outcome_type check just above, not a
  # substitute for it: calc_stats() can never actually produce a row with
  # outcome_type == "categorical"/"multicoded" and a stat outside this set
  # (group_on_outcome, calc_stats.R's own fork deciding which stats even run
  # against a factor outcome, is only TRUE for perc/count/w_perc/w_count -
  # see stat_registry there), so this can only fail on a hand-edited or
  # manually combined data frame, not real calc_stats() output. Kept anyway,
  # since "counts or percentages" is the actual, direct requirement -
  # outcome_type is only a reliable stand-in for it because of that registry
  # fact, not because it says so itself.
  if (!all(data$stat %in% c("perc", "count", "w_perc", "w_count"))) {
    stop("pivot_battery() only supports perc, count, w_perc, or w_count - found: ",
         paste(setdiff(unique(data$stat), c("perc", "count", "w_perc", "w_count")), collapse = ", "))
  }

  if (length(unique(data$stat)) != 1) {
    stop("pivot_battery() supports exactly one statistic per call (perc, count, w_perc, or w_count) - ",
         "call it again for a second one rather than mixing % and N in one grid.")
  }
  stat_code <- data$stat[1]

  # SE/CI: now carried through as parallel "<category>__conf" columns, exact
  # same mechanism reshape_one_predictor_set() uses in pivot_crosstab.R (see
  # that file's own header note on the design), just keyed on category
  # instead of predictor level - category is the column dimension here, the
  # same role a predictor level plays there. format_battery() reads these
  # off to insert an extra row under each item, same mechanic as
  # format_crosstab()'s own conf row (see that function's header note) - NOT
  # concatenated into the estimate cell, and NOT a second, wider column per
  # category (a battery grid is already at its widest reasonable point with
  # one column per response category; a second column per category for SE
  # would double that again).
  #
  # calc_stats() always includes estimate_se, filled with "-" when conf
  # wasn't requested - drop that placeholder here so it isn't mistaken for a
  # real (but empty) SE column below. Same fix as pivot_crosstab()/
  # pivot_nested_crosstab().
  if ("estimate_se" %in% names(data) && all(data$estimate_se == "-")) {
    data <- data %>% select(-estimate_se)
  }

  has_ci <- "estimate_ci" %in% names(data)
  has_se <- "estimate_se" %in% names(data)
  conf_col  <- if (has_ci) "estimate_ci" else if (has_se) "estimate_se" else NA_character_
  conf_type <- if (has_ci) "ci" else if (has_se) "se" else NA_character_

  # ---- column order: every category that appears anywhere, first-appearance order ----
  category_order <- data %>% pull(o_cat) %>% fct_inorder() %>% levels()

  # base is per-item, not per-category - calc_stat_engine() filters to this
  # outcome's own complete cases before computing any of its categories'
  # estimates, so every category row for one item shares the same base.
  # Same distinct()-as-safety-net pattern as pivot_numeric_summary().
  base_by_outcome <- data %>% distinct(outcome, base) %>% rename(Base = base)

  mutated <- data %>%
    mutate(outcome = fct_inorder(outcome),
           o_cat    = factor(o_cat, levels = category_order))

  pivoted <- mutated %>%
    select(outcome, o_lab, o_cat, estimate) %>%
    pivot_wider(id_cols = c(outcome, o_lab), names_from = o_cat, values_from = estimate)

  if (!is.na(conf_col)) {
    conf_wide <- mutated %>%
      select(outcome, o_cat, value = all_of(conf_col)) %>%
      pivot_wider(id_cols = outcome, names_from = o_cat, values_from = value) %>%
      select(outcome, all_of(category_order)) %>%
      rename_with(~ paste0(.x, "__conf"), all_of(category_order))
    pivoted <- pivoted %>% left_join(conf_wide, by = "outcome")
  }

  pivoted <- pivoted %>%
    left_join(base_by_outcome, by = "outcome") %>%
    rename(Variable = o_lab) %>%
    select(-outcome)

  # ---- common-stem detection: strip it from every label, rename the row- ----
  # ---- stub column to the stem itself ----------------------------------------
  # See find_common_stem()'s own header note for the mechanics (direct port
  # of myutils.py's find_common_stem()). "" back from find_common_stem()
  # means either fewer than 2 items, or no shared prefix past min_length -
  # both leave Variable exactly as it was, header included.
  stem <- find_common_stem(pivoted$Variable)
  if (stem != "") {
    numbering_stripped <- stringr::str_remove(pivoted$Variable, "^\\s*\\d+[a-zA-Z]?\\.\\s*")
    pivoted$Variable <- stringr::str_trim(stringr::str_sub(numbering_stripped, stringr::str_length(stem) + 1))
    names(pivoted)[names(pivoted) == "Variable"] <- stem
  }

  # conf_type is the 4th list element (NEW - was list(pivoted, stat_code,
  # category_order) before SE/CI support). Any existing caller that only
  # ever reads elements 1-3 positionally (pivoted[[1]] etc. - the convention
  # every test in this file already uses) is unaffected; this only extends
  # the list, doesn't reorder or repurpose what's already there.
  list(pivoted, stat_code, category_order, conf_type)
}


# ---- format_battery: style pivot_battery() output as a huxtable ------------
#
# Simpler again than format_numeric_summary(): every category column shares
# ONE stat_code (unlike format_numeric_summary(), where each column has its
# own), since pivot_battery() only ever runs one statistic per call - so
# there's no per-column lookup needed, just format_statistic() applied
# straight down every category column with the same stat_code. Every row is
# a real data row (no label rows) - same reasoning as
# format_numeric_summary(), NA renders as "-" uniformly, including the
# expected "item had zero respondents in this category" case noted in
# pivot_battery()'s header comment above.
#
# What-do-these-numbers-mean indicator: format_statistic() deliberately
# strips the "%" off every cell ("50", not "50.0%" - a project-wide
# convention, see format_statistic()'s own comment in pivot_summary.R), and
# unlike pivot_summary()/pivot_crosstab() there's no "Statistics" column
# here to hold "%"/"Count" as text either, since pivot_battery() only ever
# runs one statistic per call.
#
# Went through two earlier designs - a "(%)" suffix on the stub column's
# own header, then a dedicated row (above, then below, the real header) -
# before settling on this: a narrow extra COLUMN, right after the stub
# column, blank header, holding "%" or "N" (+ "(w)" if weighted) on every
# row. A single space (" "), not "" (empty string), for the header text -
# genuinely blank-looking once printed, but avoids the edge cases an
# actual empty-string column name can hit in some tidyverse/huxtable paths
# that a normal (if invisible) one-character name doesn't. Not
# "Row percentages" spelled out any more (that wording made sense as a
# whole-row label, not as repeated per-row column text) - just the bare
# symbol, matching how a "%" or "N" unit marker column reads in the sort of
# printed survey tables this is modelling.
#
# SE/CI (when pivot_battery() carried "<category>__conf" columns through -
# see that function's own note): shown as an extra row directly under an
# item, same mechanic as format_crosstab()'s own conf row (a row only gets
# one inserted if it has at least one real, non-NA conf value - an item
# whose estimates are all missing doesn't get an uninformative blank row of
# its own). Reuses the unit column instead of adding a new one: on a normal
# data row it reads "%"/"N" as always; on that item's conf row it's
# repurposed to read "SE"/"95% CI" instead - the unit column's whole job is
# already "what do these numbers mean", so extending it to cover the conf
# row is a smaller footprint than adding a dedicated Statistics-style column
# just for this. The stub column (Variable, or the detected stem) is merged
# across an item's data+conf row pair via set_rowspan(), same pattern
# format_crosstab() uses to merge a numeric variable's own data+conf rows,
# so the item's label doesn't repeat or leave an awkward blank cell under it.
#' Style `pivot_battery()` output as a huxtable
#'
#' Builds the printable battery grid: a blank-header unit column ("%"/"N",
#' "(w)" appended if weighted) next to the stub column, and an inserted
#' SE/CI row under each item when `pivot_battery()` carried conf columns
#' through - the row label reuses that same unit column ("SE"/"95% CI"),
#' with the item's stub cell merged across its data+conf row pair via
#' `set_rowspan()`.
#'
#' @param pivot_result The list returned by [pivot_battery()].
#'
#' @return A styled `huxtable` object, ready to print or pass to
#'   [export_table_to_excel()].
#'
#' @seealso [pivot_battery()], [make_battery_table()]
#' @keywords internal
format_battery <- function(pivot_result) {

  data           <- pivot_result[[1]]
  stat_code      <- pivot_result[[2]]
  category_order <- pivot_result[[3]]
  conf_type      <- pivot_result[[4]]

  conf_cols <- intersect(paste0(category_order, "__conf"), names(data))
  has_conf  <- !is.na(conf_type) && length(conf_cols) > 0

  for (col in category_order) {
    data[[col]] <- format_statistic(data[[col]], stat_code)
  }
  if (has_conf) {
    for (col in conf_cols) {
      data[[col]] <- if (conf_type == "ci") {
        format_ci_string(data[[col]], stat_code)
      } else {
        format_statistic(data[[col]], stat_code)
      }
    }
  }
  data$Base <- if_else(is.na(data$Base), NA_character_,
                        formatC(data$Base, digits = 0, big.mark = ",", format = "f"))

  # ---- blank-header unit column, right after the stub column -------------
  # Inserted BEFORE the conf-row insertion below (not after, the order
  # format_battery() used pre-SE/CI) - the conf-row step needs this column to
  # already exist so it can overwrite its OWN copy of it with "SE"/"95% CI"
  # on just that one row, leaving every ordinary data row's "%"/"N" alone.
  is_weighted <- stringr::str_starts(stat_code, "w_")
  stat_symbol <- c(perc = "%", w_perc = "%", count = "N", w_count = "N")[[stat_code]]
  if (is_weighted) stat_symbol <- paste0(stat_symbol, " (w)")

  data <- data %>% tibble::add_column(unit_col = stat_symbol, .after = 1)
  names(data)[names(data) == "unit_col"] <- " "

  # ---- row_type + conf-row insertion --------------------------------------
  # row_type didn't exist anywhere in this file before SE/CI support - every
  # row was implicitly "data" (no label/base rows the way pivot_summary()/
  # pivot_crosstab() have). Introduced here purely so conf rows can be told
  # apart from real item rows for the row-index bookkeeping (NA formatting
  # already covers both via the blanket set_na_string() below, unchanged)
  # and the stub-column merge just below.
  data <- data %>% mutate(row_type = "data")

  if (has_conf) {
    conf_label <- if (conf_type == "ci") "95% CI" else "SE"

    data <- map_df(seq_len(nrow(data)), function(i) {
      row <- data[i, , drop = FALSE]
      has_row_conf <- any(!is.na(unlist(row[, conf_cols, drop = FALSE])))

      if (!has_row_conf) return(row %>% select(-all_of(conf_cols)))

      conf_row <- row
      conf_row[[1]]     <- ""            # stub column - blanked, same convention as format_crosstab()'s Variable
      conf_row[[2]]     <- conf_label    # unit column, repurposed for this one row
      conf_row$row_type <- "conf"
      conf_row$Base     <- NA_character_ # a conf row doesn't carry its own base
      for (cat in category_order) {
        cc <- paste0(cat, "__conf")
        if (cc %in% names(row)) conf_row[[cat]] <- row[[cc]]
      }

      bind_rows(row %>% select(-all_of(conf_cols)),
                conf_row %>% select(-all_of(conf_cols)))
    })
  } else {
    data <- data %>% select(-any_of(conf_cols))
  }

  row_type <- data$row_type
  ht <- data %>% select(-row_type) %>% huxtable::as_hux(add_colnames = TRUE)

  header_offset <- 1L   # the as_hux(add_colnames = TRUE) column-name row - no spanning header here, unlike format_crosstab()

  # Column 1, not `which(names(ht) == "Variable")` - pivot_battery()'s
  # common-stem step (see its own header note) renames that column to the
  # detected stem text when one's found, so "Variable" isn't a safe name to
  # look up any more. It's always the row-stub column regardless of what
  # it's actually called, by construction (first column pivot_battery()
  # builds, never touched by the pivot_wider() step that adds the rest).
  # unit_col is always column 2, right after it - inserted immediately
  # above, before the category columns exist to shift anything else around.
  variable_col <- 1
  unit_col     <- 2
  category_col_idx <- which(names(ht) %in% category_order)
  base_col     <- which(names(ht) == "Base")

  ht <- ht %>%
    huxtable::set_na_string(value = "-") %>%
    huxtable::set_align(huxtable::everywhere, variable_col, "left") %>%
    huxtable::set_align(huxtable::everywhere, unit_col, "center") %>%
    huxtable::set_align(huxtable::everywhere, c(category_col_idx, base_col), "right") %>%
    huxtable::set_all_borders(huxtable::everywhere, huxtable::everywhere,
                               huxtable::brdr(0.5, "solid", "grey85")) %>%
    huxtable::set_bottom_border(1, huxtable::everywhere, huxtable::brdr(1, "solid", "grey40")) %>%
    huxtable::set_all_padding(1) %>%
    huxtable::set_font_size(8) %>%
    huxtable::set_font("Arial") %>%
    huxtable::set_bold(row = 1, col = huxtable::everywhere) %>%
    huxtable::set_background_color(1, huxtable::everywhere, "grey95") %>%
    huxtable::set_valign("middle") %>%
    huxtable::stripe_rows(stripe1 = "#f5f7fa", stripe2 = "#ffffff")

  # stripe_rows() re-tints row 1 arbitrarily (same fix format_summary()/
  # format_crosstab()/format_numeric_summary() all needed) - reassert the
  # header styling after it.
  ht <- ht %>%
    huxtable::set_bold(1, huxtable::everywhere, TRUE) %>%
    huxtable::set_background_color(1, huxtable::everywhere, "grey95")

  # ---- merge each item's data+conf row pair in the stub column -----------
  # Same rowspan mechanic format_crosstab() uses for a numeric variable's
  # data+conf rows - cumsum(row_type == "data") gives each data row a new
  # group id, and the conf row (if any) directly below it shares that same
  # id, since cumsum only increments AT the data row itself. Unlike
  # format_crosstab(), there's no outcome_type branch needed here - every
  # item in a battery grid is uniform (no categorical-vs-numeric split), so
  # this applies to every data+conf pair without exception.
  if (any(row_type == "conf")) {
    merge_group <- cumsum(row_type == "data")
    run_lengths <- rle(merge_group)$lengths
    run_starts  <- cumsum(c(1, utils::head(run_lengths, -1)))
    for (i in seq_along(run_starts)) {
      if (run_lengths[i] > 1) {
        start_row <- run_starts[i] + header_offset
        span_len  <- run_lengths[i]
        ht <- huxtable::set_rowspan(ht, row = start_row, col = variable_col, value = span_len)
      }
    }
  }

  ht
}
