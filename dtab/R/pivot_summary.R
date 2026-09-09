# pivot_summary() / format_summary(): pivot + formatting layer turning
# calc_stats() output into a Total-only summary table. Variable and Levels
# collapse into one "Variable" column - a numeric variable's label sits on
# its one data row; a categorical variable gets a label row above its
# levels. `is_variable_row` marks which rows hold the actual variable
# name (vs. a level like "North"), for bolding; `stat_code` carries the
# raw stat string for digit-formatting, since Statistics holds display
# text ("%", "Mean") instead.
#
# fct_inorder() keeps (outcome, stat) chunks in their original order -
# group_split() on a plain character column would re-sort alphabetically.
#
# huxtable functions are always called as huxtable::foo(): flextable
# (sometimes loaded in the same session) masks several names huxtable
# uses (align, font, etc.), so a bare call can silently resolve wrong.


# ---- tidy_statistic_description: relabel `stat` for display ------------------
# Always "Statistics" as the column name, always the short form ("Mean",
# not "Unweighted mean") - stat_code already carries the raw stat string
# for anything that needs to key off it programmatically, so this column
# has no job left except to be read by a person.
tidy_statistic_description <- function(data) {

  labels <- c(
    mean = "Mean", median = "Median", sum = "Sum", perc = "%", count = "Count",
    min = "Min", max = "Max", range = "Range", iqr = "IQR", sd = "SD",
    w_mean = "Mean (w)", w_median = "Median (w)", w_sum = "Sum (w)",
    w_perc = "% (w)", w_count = "Count (w)",
    w_iqr = "IQR (w)", w_sd = "SD (w)"
  )

  data %>%
    mutate(stat = unname(labels[stat])) %>%
    rename(Statistics = stat)
}


# ---- pivot_summary: reshape calc_stats() output -------------------------------

#' Reshape `calc_stats()` output into a Total-only summary table
#'
#' Collapses Variable and Levels into a single "Variable" column: a numeric
#' variable's label sits directly on its one data row, while a categorical
#' variable gets a label row above its levels. Meant for a `calc_stats()`
#' call with no predictor (or, if a predictor was supplied, only its
#' "Total" rows are used - see [pivot_crosstab()] for a real crosstab).
#'
#' @param data A tibble as returned by [calc_stats()].
#'
#' @return A list: `pivoted` (the reshaped tibble, with `row_type`,
#'   `stat_code`, `block_id`, and `is_variable_row` metadata columns) and
#'   `bases` (footnote text from `base_description`, via
#'   `prepare_base_for_table()`). Pass this list straight to
#'   [format_summary()].
#'
#' @seealso [format_summary()], [calc_stats()]
#' @keywords internal
pivot_summary <- function(data) {

  bases <- data %>% pull(base_description) %>% unique() %>% paste0(collapse = " X ")
  if (any(!is.na(bases))) bases <- bases %>% prepare_base_for_table()

  if ("cross_break" %in% names(data)) {
    data <- data %>% filter(cross_break == "Total")
  }

  # calc_stats() always includes estimate_se, filled with "-" when conf
  # wasn't requested - drop that placeholder so it isn't carried through
  # as a real (but empty) SE column.
  if ("estimate_se" %in% names(data) && all(data$estimate_se == "-")) {
    data <- data %>% select(-estimate_se)
  }

  pivot_one_block <- function(block) {

    is_categorical <- block$outcome_type[1] %in% c("categorical", "multicoded")

    data_rows <- block %>%
      mutate(
        stat_code       = stat,
        Variable        = if (is_categorical) o_cat else o_lab[1],
        row_type        = "data",
        Base            = base[1],
        block_id        = as.integer(.block[1]),
        is_variable_row = !is_categorical
      ) %>%
      rename(Estimate = estimate) %>%
      select(Variable, stat, Estimate, any_of(c("estimate_se", "estimate_ci")),
             Base, row_type, stat_code, block_id, is_variable_row)

    if (!is_categorical) return(data_rows)

    # stat/Estimate/etc copied from the first level row so
    # tidy_statistic_description() below has a non-NA value to match on -
    # blanked for display further down.
    label_row <- data_rows[1, ] %>%
      mutate(Variable = block$o_lab[1], row_type = "label", is_variable_row = TRUE)

    bind_rows(label_row, data_rows)
  }

  pivoted <- data %>%
    mutate(.block = fct_inorder(paste(outcome, stat, sep = "___"))) %>%
    group_split(.block) %>%
    map_df(pivot_one_block) %>%
    tidy_statistic_description() %>%
    rename(any_of(c(SE = "estimate_se", `95% CI` = "estimate_ci"))) %>%
    mutate(across(-c(Variable, row_type, stat_code, block_id, is_variable_row),
                   ~ replace(.x, row_type == "label", NA)))

  list(pivoted, bases)
}


# format_summary(): Estimate/SE/95% CI render as "-" when genuinely
# missing on a data row (e.g. a dropped small-subgroup median CI) - blank
# on a label row instead, since that's structural, not missing data.
# Digit formatting is a plain-text transform before as_hux() ever runs
# (not huxtable's set_number_format()), so format_statistic() stays
# testable on its own.

# NA in, NA out. Percentages: no "%" sign, rounded to 0dp (0.5 -> "50").
format_statistic <- function(x, stat_code) {
  case_when(
    is.na(x) ~ NA_character_,
    stat_code %in% c("perc", "w_perc")                        ~ sprintf("%.0f", 100 * x),
    stat_code %in% c("count", "w_count")                      ~ formatC(x, digits = 0, big.mark = ",", format = "f"),
    stat_code %in% c("mean", "w_mean", "median", "w_median")  ~ formatC(x, digits = 1, format = "f"),
    stat_code %in% c("sum", "w_sum")                          ~ formatC(x, digits = 1, big.mark = ",", format = "f"),
    # min/max/range/iqr/sd are all in the outcome variable's own units,
    # same as mean/median - 1dp matches those.
    stat_code %in% c("min", "max", "range", "iqr", "sd",
                      "w_iqr", "w_sd")                        ~ formatC(x, digits = 1, format = "f"),
    TRUE ~ formatC(x, format = "f")   # unrecognised stat_code - shouldn't happen in practice
  )
}

# Splits a "<low> - <high>" string built by calc_stats() (see
# apply_conf_columns() in calc_stats.R) into its two bounds, runs each
# through format_statistic() using the same stat_code, and rejoins.
format_ci_string <- function(ci_string, stat_code) {
  ci_parts <- str_split_fixed(ci_string, " - ", 2)
  low  <- format_statistic(as.numeric(ci_parts[, 1]), stat_code)
  high <- format_statistic(as.numeric(ci_parts[, 2]), stat_code)
  if_else(is.na(low) | is.na(high), NA_character_, paste0(low, " - ", high))
}

#' Style `pivot_summary()` output as a huxtable
#'
#' Bolds variable names, blanks label-row cells that don't apply, and
#' applies `format_statistic()`'s digit rules per row's own `stat_code`.
#'
#' @param pivot_result The list returned by [pivot_summary()].
#'
#' @return A styled `huxtable` object, ready to print or pass to
#'   [export_table_to_excel()].
#'
#' @seealso [pivot_summary()]
#' @keywords internal
format_summary <- function(pivot_result) {

  data  <- pivot_result[[1]]
  bases <- pivot_result[[2]]

  stat_code       <- data$stat_code
  row_type        <- data$row_type
  is_variable_row <- data$is_variable_row

  data$Estimate <- format_statistic(data$Estimate, stat_code)
  if ("SE" %in% names(data)) data$SE <- format_statistic(data$SE, stat_code)
  if ("95% CI" %in% names(data)) {
    data[["95% CI"]] <- format_ci_string(data[["95% CI"]], stat_code)
  }
  data$Base <- if_else(is.na(data$Base), NA_character_,
                        formatC(data$Base, digits = 0, big.mark = ",", format = "f"))

  ht <- data %>% select(-row_type, -stat_code, -block_id, -is_variable_row) %>%
    huxtable::as_hux(add_colnames = TRUE)

  no_of_rows <- nrow(ht)                     # includes the header row
  data_rows  <- which(row_type == "data") + 1

  est_col      <- which(names(ht) == "Estimate")
  se_or_ci     <- which(names(ht) %in% c("SE", "95% CI"))
  variable_col <- which(names(ht) == "Variable")

  ht <- ht %>%
    huxtable::set_na_string(value = "") %>%
    huxtable::set_na_string(row = data_rows, col = c(est_col, se_or_ci), value = "-")

  ht <- ht %>%
    huxtable::set_align(huxtable::everywhere, which(names(ht) %in% c("Variable", "Statistics")), "left") %>%
    huxtable::set_align(huxtable::everywhere, which(names(ht) %in% c("Estimate", "SE", "95% CI", "Base")), "right")

  variable_name_rows <- which(is_variable_row) + 1   # +1 for the header row
  ht <- huxtable::set_bold(ht, row = variable_name_rows, col = variable_col, value = TRUE)

  # Merge repeated Variable text where one variable spans multiple
  # statistic rows (e.g. "x" for both Mean and Median). Label rows get a
  # unique per-row key so they never merge with the data rows below them.
  merge_key   <- if_else(row_type == "data", data$Variable, paste0("__label__", seq_along(row_type)))
  run_lengths <- rle(merge_key)$lengths
  run_starts  <- cumsum(c(1, head(run_lengths, -1)))

  # No follow-up blanking of covered cells - set_rowspan() copies the
  # anchor's content into every cell it covers (same fact used in
  # pivot_crosstab.R's identical merge pattern).
  for (i in seq_along(run_starts)) {
    if (run_lengths[i] > 1) {
      start_row <- run_starts[i] + 1   # +1 for the header row offset
      span_len  <- run_lengths[i]
      ht <- huxtable::set_rowspan(ht, row = start_row, col = variable_col, value = span_len)
    }
  }

  ht <- huxtable::stripe_rows(ht, stripe1 = "#f5f7fa", stripe2 = "#ffffff")

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

  # Extra top padding at the start of each variable's block - NOT keyed
  # off run_starts above (that would also flag every distinct categorical
  # level as its own "block"). Instead: among is_variable_row rows only,
  # a change in Variable text means a new variable started.
  variable_true_rows  <- which(is_variable_row)
  variable_texts      <- data$Variable[variable_true_rows]
  is_new_variable     <- variable_texts != dplyr::lag(variable_texts, default = "")
  variable_start_rows <- variable_true_rows[is_new_variable] + 1   # +1 for the header row
  ht <- huxtable::set_top_padding(ht, row = variable_start_rows, col = huxtable::everywhere, value = 6)

  if (all(unlist(bases) != "NA")) {
    for (i in seq_along(bases)) {
      ht <- ht %>% huxtable::add_footnote(bases[[i]], border = NULL)
    }
  }

  ht
}
