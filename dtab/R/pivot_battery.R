# pivot_battery() / format_battery(): a "battery grid" table - rows are
# items sharing one response scale (e.g. a Likert set), columns are the
# shared response categories, one % (or N) per cell.
#
# SCOPE: predictors = NULL only (checked, errors otherwise), exactly one
# statistic per call - a grid mixing % and N isn't a layout anyone needs.
# Category set/order is NOT validated across items - an item with zero
# respondents in one category legitimately produces no row for it, and
# the cell reads "-", not an error.
#
# Common item-label stems ("Satisfaction with: Speed/Price/...") are
# stripped automatically and surfaced as the row-stub column's header.
#
# SE/CI is carried through as "<category>__conf" columns - the same
# "<level>__conf" mechanism reshape_one_predictor_set() uses in
# pivot_crosstab.R, keyed on category instead of predictor level.


# Named common_prefix_stem(), not common_prefix() - that name collides
# with calc_stats.R's own common_prefix() (a different implementation for
# a different purpose; the two disagree on an empty vector).
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

# Strips a leading item-numbering prefix ("12a. "), finds the longest
# common prefix across the rest, rejects it if too short, trims to the
# last space. Fewer than 2 labels returns "" - a single label's "stem"
# would otherwise be the whole label, leaving it blank.
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

  if (!all(data$stat %in% c("perc", "count", "w_perc", "w_count"))) {
    stop("pivot_battery() only supports perc, count, w_perc, or w_count - found: ",
         paste(setdiff(unique(data$stat), c("perc", "count", "w_perc", "w_count")), collapse = ", "))
  }

  if (length(unique(data$stat)) != 1) {
    stop("pivot_battery() supports exactly one statistic per call (perc, count, w_perc, or w_count) - ",
         "call it again for a second one rather than mixing % and N in one grid.")
  }
  stat_code <- data$stat[1]

  # Same "-" placeholder fix as pivot_crosstab()/pivot_nested_crosstab().
  if ("estimate_se" %in% names(data) && all(data$estimate_se == "-")) {
    data <- data %>% select(-estimate_se)
  }

  has_ci <- "estimate_ci" %in% names(data)
  has_se <- "estimate_se" %in% names(data)
  conf_col  <- if (has_ci) "estimate_ci" else if (has_se) "estimate_se" else NA_character_
  conf_type <- if (has_ci) "ci" else if (has_se) "se" else NA_character_

  category_order <- data %>% pull(o_cat) %>% fct_inorder() %>% levels()

  # base is per-item, not per-category (every category row for one item
  # shares one base) - same distinct()-as-safety-net pattern as
  # pivot_numeric_summary().
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

  stem <- find_common_stem(pivoted$Variable)
  if (stem != "") {
    numbering_stripped <- stringr::str_remove(pivoted$Variable, "^\\s*\\d+[a-zA-Z]?\\.\\s*")
    pivoted$Variable <- stringr::str_trim(stringr::str_sub(numbering_stripped, stringr::str_length(stem) + 1))
    names(pivoted)[names(pivoted) == "Variable"] <- stem
  }

  list(pivoted, stat_code, category_order, conf_type)
}


# format_battery(): every category column shares one stat_code, so
# format_statistic() applies with the same code down every column. A
# narrow blank-header unit column holds "%"/"N" (+ "(w)" if weighted) -
# its header text is a single space, not "", to avoid empty-string column
# name edge cases in some tidyverse/huxtable paths. SE/CI shows as an
# inserted row, same mechanic as format_crosstab()'s conf row, reusing the
# unit column ("SE"/"95% CI" on that row instead of "%"/"N").

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

  is_weighted <- stringr::str_starts(stat_code, "w_")
  stat_symbol <- c(perc = "%", w_perc = "%", count = "N", w_count = "N")[[stat_code]]
  if (is_weighted) stat_symbol <- paste0(stat_symbol, " (w)")

  data <- data %>% tibble::add_column(unit_col = stat_symbol, .after = 1)
  names(data)[names(data) == "unit_col"] <- " "

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

  header_offset <- 1L

  # Column 1, not `which(names(ht) == "Variable")` - the common-stem step
  # may have renamed that column, so "Variable" isn't a safe name to
  # look up.
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

  # stripe_rows() re-tints row 1 arbitrarily (same fix every format_*()
  # function needs) - reassert the header styling after it.
  ht <- ht %>%
    huxtable::set_bold(1, huxtable::everywhere, TRUE) %>%
    huxtable::set_background_color(1, huxtable::everywhere, "grey95")

  # Merge each item's data+conf row pair - cumsum(row_type == "data")
  # gives each data row a new group id, shared by the conf row (if any)
  # directly below it.
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
