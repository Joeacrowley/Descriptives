# min/max/range have no weighted equivalent (a sample's observed extremes
# aren't population estimates weighting would adjust), so under
# weighted = TRUE those three fall back to their unweighted version.


# ---- pivot_numeric_summary: call calc_stats() and reshape wide -------------

numeric_summary_stats <- c("mean", "median", "sum", "min", "max", "range", "iqr", "sd")

numeric_summary_weighted_equiv <- c(
  mean = "w_mean", median = "w_median", sum = "w_sum", sd = "w_sd", iqr = "w_iqr"
)

#' Build a numeric-only summary table (variables x statistics), calling
#' `calc_stats()` internally
#'
#' Rows are variables, columns are statistics (Mean, SD, Min, ...), one
#' number per cell - no predictor/crossbreak dimension, always a single
#' overall ("Total") summary. Unlike [pivot_summary()]/[pivot_crosstab()],
#' this function calls [calc_stats()] itself rather than taking its output,
#' because `weighted` has to choose the exact stat codes (`mean` vs
#' `w_mean`, ...) before `calc_stats()` ever runs.
#'
#' @param data A data frame, or survey design object if `weighted = TRUE`.
#' @param outcomes Character vector of numeric variable names to summarise.
#' @param statistics One or more of `mean`, `median`, `sum`, `min`, `max`,
#'   `range`, `iqr`, `sd`. Default `c("mean", "sd")`.
#' @param weighted If `TRUE`, requests the weighted equivalent of each
#'   statistic that has one (`data` must then be a survey design object).
#'   `min`/`max`/`range` have no weighted equivalent and are returned
#'   unweighted regardless.
#' @param base Optional base/filter expression, passed through to
#'   [calc_stats()].
#'
#' @return A list: `pivoted` (one row per variable, one column per
#'   statistic, plus `Base`) and `stat_lookup` (a named vector mapping each
#'   display column name back to its underlying stat code, for
#'   [format_numeric_summary()]'s digit rules). Pass this list straight to
#'   [format_numeric_summary()].
#'
#' @seealso [format_numeric_summary()], [calc_stats()]
#' @keywords internal
pivot_numeric_summary <- function(data, outcomes, statistics = c("mean", "sd"),
                                   weighted = FALSE, base = NULL) {

  if (!isTRUE(vars_exist(data = data, variable_list = outcomes))) {
    stop("Some variables you want to use do not exist in the data frame.")
  }
  if (!isTRUE(check_all_numeric(data = data, variable_list = outcomes))) {
    stop("All outcomes must be numeric - pivot_numeric_summary() has no room for categorical statistics.")
  }
  if (!all(statistics %in% numeric_summary_stats)) {
    stop("statistics must be one or more of: ", paste(numeric_summary_stats, collapse = ", "))
  }
  if (isTRUE(weighted) && !any(str_detect(class(data), "survey"))) {
    stop("weighted = TRUE requires a survey design object (as_survey_design()) as data.")
  }

  requested_stats <- statistics
  if (isTRUE(weighted)) {
    has_weighted_equiv <- statistics %in% names(numeric_summary_weighted_equiv)
    requested_stats[has_weighted_equiv] <- numeric_summary_weighted_equiv[statistics[has_weighted_equiv]]
  }

  stats_table <- calc_stats(data, outcomes = outcomes, predictors = NULL,
                             statistics = requested_stats, conf = NULL,
                             base = base, pval = NULL, multicode = FALSE)

  labelled <- stats_table %>% tidy_statistic_description()

  # base is per-outcome, not per-statistic - distinct() here is a safety
  # net, not a real aggregation.
  base_by_outcome <- labelled %>% distinct(outcome, base) %>% rename(Base = base)

  pivoted <- labelled %>%
    select(outcome, o_lab, Statistics, estimate) %>%
    pivot_wider(id_cols = c(outcome, o_lab), names_from = Statistics, values_from = estimate) %>%
    left_join(base_by_outcome, by = "outcome") %>%
    rename(Variable = o_lab) %>%
    select(-outcome)

  # stat_lookup maps each display column back to its stat_code (stat_code
  # varies per COLUMN here, unlike pivot_summary()/pivot_crosstab() where
  # it's a data column) - built the same way pivot_wider() named columns
  # above, so the labels match.
  stat_lookup <- setNames(requested_stats,
                           tidy_statistic_description(tibble(stat = requested_stats))$Statistics)

  list(pivoted, stat_lookup)
}


#' Style `pivot_numeric_summary()` output as a huxtable
#'
#' Simpler than [format_summary()]/[format_crosstab()]: every row is a real
#' data row (no label rows, no base rows, no inserted conf rows), so no
#' `row_type`/block logic is needed. Applies `format_statistic()`'s digit
#' rules per column, via `stat_lookup`. A genuinely missing statistic
#' renders as `"-"`.
#'
#' @param pivot_result The list returned by [pivot_numeric_summary()].
#'
#' @return A styled `huxtable` object, ready to print or pass to
#'   [export_table_to_excel()].
#'
#' @seealso [pivot_numeric_summary()]
#' @keywords internal
format_numeric_summary <- function(pivot_result) {

  data        <- pivot_result[[1]]
  stat_lookup <- pivot_result[[2]]
  stat_cols   <- names(stat_lookup)

  for (col in stat_cols) {
    data[[col]] <- format_statistic(data[[col]], stat_lookup[[col]])
  }
  data$Base <- if_else(is.na(data$Base), NA_character_,
                        formatC(data$Base, digits = 0, big.mark = ",", format = "f"))

  ht <- data %>% huxtable::as_hux(add_colnames = TRUE)

  variable_col <- which(names(ht) == "Variable")
  stat_col_idx <- which(names(ht) %in% stat_cols)
  base_col     <- which(names(ht) == "Base")

  ht <- ht %>%
    huxtable::set_na_string(value = "-") %>%
    huxtable::set_align(huxtable::everywhere, variable_col, "left") %>%
    huxtable::set_align(huxtable::everywhere, c(stat_col_idx, base_col), "right") %>%
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

  # stripe_rows() re-tints row 1 arbitrarily - reassert header styling.
  ht <- ht %>%
    huxtable::set_bold(1, huxtable::everywhere, TRUE) %>%
    huxtable::set_background_color(1, huxtable::everywhere, "grey95")

  ht
}
