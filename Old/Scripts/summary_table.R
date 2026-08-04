# =============================================================================
# summary_table() — a single-step Totals overview table
#
# Ports pivot1() and pivot1h() (plus their three small dependencies) from
# 0_0_Old/Deprecated_Tables/Tables function/, unchanged in behaviour, and adds
# summary_table() at the bottom to chain calc_stats() -> pivot1() -> pivot1h()
# -> as_flextable() in one call for the common case: an overview table of
# numeric and categorical variables together, no crosstab breakdown.
#
# Depends on calc_stats() and its own dependencies (this project's
# Scripts/simplified_functions.R) plus prepare_base_for_table() (also in
# simplified_functions.R) — source that first.
#
# TWO CHANGES FROM THE ORIGINAL, both discussed and agreed on directly:
#
#  1. pivot1h() previously merged repeated Statistics-column cells (col 4)
#     only inside a separate "extra merging if no percentage columns" step,
#     gated on `!any(grepl("%", statistics))`. That check only works as
#     intended when concise = TRUE, since non-concise percentage labels
#     render as the word "Percentage" (no % character) and so never match
#     the check regardless of whether percentages are actually present.
#     Replaced with an unconditional merge of column 4, using the exact same
#     grouping key and row exclusion as the existing Variable-column merge
#     (col 2) just above it — so a variable's statistic label now only shows
#     once per block, the same way its name already does, in every mode.
#
#  2. summary_table() defaults concise = TRUE (pivot1() itself keeps its
#     original concise = NULL default, unchanged) — "Weighted percentage"
#     repeated down every row of a table was the main complaint; "% (w)"
#     shown once per block after fix #1 is a large reduction in both text
#     length and repetition, for zero cost.
# =============================================================================

library(tidyverse)
library(huxtable)
library(flextable)

# Every huxtable function below is called as huxtable::foo() rather than bare
# foo(), even though library(huxtable) is attached above - flextable (also
# attached here) masks several names huxtable also uses (align, font, etc.),
# so an unqualified call can silently resolve to the wrong package's version
# depending on load order. Namespacing removes the ambiguity.


# ---- tidy_statistic_description: relabel `stat` for display ------------------
# Unchanged from the original.

tidy_statistic_description <- function(data, concise = NULL) {

  if (!is.null(concise)) {

    data <- data %>%
      mutate(stat = case_when(
        stat == "mean"     ~ "Mean",
        stat == "median"   ~ "Median",
        stat == "sum"      ~ "Sum",
        stat == "perc"     ~ "%",
        stat == "count"    ~ "Count",
        stat == "w_mean"   ~ "Mean (w)",
        stat == "w_median" ~ "Median (w)",
        stat == "w_sum"    ~ "Sum (w)",
        stat == "w_perc"   ~ "% (w)",
        stat == "w_count"  ~ "Count (w)"
      )) %>%
      rename(Statistics = stat)

  } else if (all(str_detect(data$stat, "w_"))) {

    data <- data %>%
      mutate(stat = case_when(
        stat == "w_mean"   ~ "Mean",
        stat == "w_median" ~ "Median",
        stat == "w_sum"    ~ "Sum",
        stat == "w_perc"   ~ "Percentage",
        stat == "w_count"  ~ "Count"
      )) %>%
      rename(`Statistics (weighted)` = stat)

  } else if (all(!str_detect(data$stat, "w_"))) {

    data <- data %>%
      mutate(stat = case_when(
        stat == "mean"   ~ "Mean",
        stat == "median" ~ "Median",
        stat == "sum"    ~ "Sum",
        stat == "perc"   ~ "Percentage",
        stat == "count"  ~ "Count"
      )) %>%
      rename(`Statistics (unweighted)` = stat)

  } else {

    data <- data %>%
      mutate(stat = case_when(
        stat == "mean"     ~ "Unweighted mean",
        stat == "median"   ~ "Unweighted median",
        stat == "sum"      ~ "Unweighted sum",
        stat == "perc"     ~ "Unweighted percentage",
        stat == "count"    ~ "Unweighted count",
        stat == "w_mean"   ~ "Weighted mean",
        stat == "w_median" ~ "Weighted median",
        stat == "w_sum"    ~ "Weighted sum",
        stat == "w_perc"   ~ "Weighted percentage",
        stat == "w_count"  ~ "Weighted count"
      )) %>%
      rename(Statistics = stat)
  }

  data
}


# ---- merge_rows: vertically merge repeated huxtable cells --------------------
# Unchanged from the original. Groups consecutive rows sharing identical
# values across col_nums, then merges cols_to_merge for those runs (rowspan +
# blank the cells below), skipping any run whose start row is in
# rows_to_exclude.

merge_rows <- function(data, col_nums, cols_to_merge, rows_to_exclude = NULL) {

  group_keys <- data[, col_nums] %>%
    as.data.frame() %>%
    unite(vars, everything(), sep = " ") %>%
    unlist() %>%
    unname()

  rle_groups <- rle(group_keys)
  row_start  <- cumsum(c(1, head(rle_groups$lengths, -1)))
  row_lengths <- rle_groups$lengths

  for (i in seq_along(row_start)) {
    start_row <- row_start[i]
    if (!start_row %in% rows_to_exclude) {
      span_len <- row_lengths[i]
      if (span_len > 1) {
        rs <- huxtable::rowspan(data)
        rs[start_row, cols_to_merge] <- span_len
        huxtable::rowspan(data) <- rs
        for (r in (start_row + 1):(start_row + span_len - 1)) {
          data[r, cols_to_merge] <- ""
        }
      }
    }
  }

  data
}


# ---- identify_natural_breaks_pivot2: find section boundaries -----------------
# Unchanged from the original. Returns the row index where each run of
# identical values across `vars` ends.

identify_natural_breaks_pivot2 <- function(data, vars) {
  suppressMessages(suppressWarnings({
    group_keys <- data %>% unite(merged_vars, all_of(vars), sep = " ") %>% pull(merged_vars)
    rle_groups <- rle(group_keys)
    cumsum(rle_groups$lengths) %>% sort()
  }))
}


# ---- pivot1: reshape calc_stats() output into a display-ready Totals table ---
# Unchanged from the original, including its own concise = NULL default —
# summary_table() below is what changes the default, not this function.

pivot1 <- function(data, concise = NULL) {

  results_as_list <- list()

  bases <- data %>% pull(base_description) %>% unique() %>% paste0(collapse = " X ")
  if (any(!is.na(bases))) bases <- bases %>% prepare_base_for_table()

  if ("cross_break" %in% names(data)) {
    data <- data %>%
      filter(cross_break == "Total") %>%
      select(!all_of(grep("p_lab|p_cat|predictor", names(data), value = TRUE))) %>%
      select(-cross_break, -p_value, -p_method)
  }

  lookup <- c(
    "Estimate" = "estimate",
    "95% CI"   = "estimate_ci",
    "SE"       = "estimate_se",
    "Base"     = "base",
    "Variable" = "o_lab",
    "Levels"   = "o_cat"
  )

  result <- data %>%
    select(-base_description) %>%
    mutate(stat_mem = stat, .before = 1) %>%
    select(!contains(c("upp", "low", "outcome", "unweighted_n"))) %>%
    relocate(base, .after = everything()) %>%
    mutate(o_cat = case_when(stat == o_cat ~ "", TRUE ~ o_cat)) %>%
    tidy_statistic_description(concise = concise) %>%
    rename(any_of(lookup))

  if ("estimate_se" %in% names(data)) {
    se_not_included <- all(data %>% pull(estimate_se) == "-")
    if (se_not_included) result <- result %>% select(-SE)
  }

  results_as_list[[1]] <- result
  results_as_list[[2]] <- bases
  results_as_list
}


# ---- pivot1h: format the pivot1() output as a huxtable -----------------------

pivot1h <- function(table) {

  data <- table[[1]]
  ht <- data %>% huxtable::as_hux()

  stat_mem_values <- ht |> pull(1)
  statistics       <- ht |> pull(4)
  no_of_rows <- nrow(ht)
  no_of_cols <- ncol(ht)

  est_cols <- if (no_of_cols == 7) c(5:6) else 5

  rows_for_numeric_variables <- which(
    stat_mem_values %in% c("mean", "w_mean", "median", "w_median", "sum", "w_sum", "num_base")
  )
  cat_rows <- which(!(1:no_of_rows) %in% rows_for_numeric_variables)

  numeric_outcomes_only <- all(data$stat_mem %in%
    c("mean", "w_mean", "median", "w_median", "sum", "w_sum", "num_base"))

  # ---- 2. Natural breaks for alternate section coloring ----
  natural_breaks <- identify_natural_breaks_pivot2(data, vars = c(2)) + 1
  colour_coding <- list()
  start <- 2
  for (xxx in seq_along(natural_breaks)) {
    colour_coding[[xxx]] <- seq(from = start, to = natural_breaks[xxx], by = 1)
    start <- natural_breaks[xxx] + 1
  }

  # ---- 3. Basic table styling ----
  formatted_table <- ht |>
    huxtable::set_all_borders(1, huxtable::everywhere, huxtable::brdr(1, style = "solid", "black")) |>
    huxtable::set_all_borders(2:no_of_rows, huxtable::everywhere, huxtable::brdr(1, style = "solid", "grey70")) |>
    huxtable::set_top_border(1, huxtable::everywhere, huxtable::brdr(1.5, style = "solid", "black")) |>
    huxtable::set_bottom_border(c(1, no_of_rows), huxtable::everywhere, huxtable::brdr(1.5, style = "solid", "black")) |>
    huxtable::set_all_padding(2) |>
    huxtable::set_font_size(10) |>
    huxtable::set_width(1) |>
    huxtable::set_font("Arial") |>
    huxtable::set_italic(col = no_of_cols, row = 2:no_of_rows)

  # ---- 4. Number formatting rules by statistic type ----
  formatted_table <- formatted_table |>
    huxtable::set_number_format(col = est_cols, row = stat_mem_values %in% c("w_perc", "perc"),
                       value = list(function(x) sprintf("%.1f%%", 100 * x))) |>
    huxtable::set_number_format(col = est_cols, row = stat_mem_values %in% c("count"),
                       value = list(function(x) formatC(x, digits = 0, format = "f"))) |>
    huxtable::set_number_format(col = est_cols, row = stat_mem_values %in% c("mean", "w_mean", "median", "w_median"),
                       value = list(function(x) formatC(x, digits = 1, format = "f"))) |>
    huxtable::set_number_format(col = est_cols, row = stat_mem_values %in% c("sum", "w_count", "w_sum"),
                       value = list(function(x) formatC(x, digits = 1, big.mark = ",", format = "fg"))) |>
    huxtable::set_number_format(col = no_of_cols,
                       value = list(function(x) formatC(x, digits = 0, big.mark = ",", format = "f"))) |>
    huxtable::set_na_string(value = "NA", col = est_cols) |>
    huxtable::map_text_color(huxtable::by_cases(is.na(.) ~ "red")) |>
    huxtable::map_text_color(huxtable::by_cases(grepl("NaN", .) ~ "red"))

  # ---- 5. Merge repeated cells ----
  # CHANGE #1: the Statistics-column merge (col 4) used to be a separate,
  # conditionally-run step further down ("extra merging if no percentage
  # columns"). It's unconditional here now, using the same key/exclusion as
  # the Variable-column merge (col 2) directly above it — see header note.
  formatted_table <- formatted_table |>
    merge_rows(col_nums = c(1, 2), cols_to_merge = no_of_cols, rows_to_exclude = rows_for_numeric_variables) |>
    merge_rows(col_nums = c(2),    cols_to_merge = no_of_cols, rows_to_exclude = cat_rows) |>
    merge_rows(col_nums = c(1, 2), cols_to_merge = c(2),       rows_to_exclude = rows_for_numeric_variables) |>
    merge_rows(col_nums = c(1, 2), cols_to_merge = c(4),       rows_to_exclude = rows_for_numeric_variables) |>
    merge_rows(col_nums = c(2),    cols_to_merge = c(3),       rows_to_exclude = cat_rows) |>
    huxtable::merge_repeated_rows(col = c(2), row = rows_for_numeric_variables)

  # ---- 6. Alignment & header styles ----
  formatted_table <- formatted_table |>
    huxtable::set_align(huxtable::everywhere, 1:4, "left") |>
    huxtable::set_align(huxtable::everywhere, 5:no_of_cols, "centre") |>
    huxtable::set_valign("middle") |>
    huxtable::set_bold(row = 1, col = huxtable::everywhere) |>
    huxtable::set_background_color(1, huxtable::everywhere, "grey95") |>
    huxtable::set_caption("Variable overview")

  # ---- 7. Alternate section coloring ----
  alternate_colours <- list(c("#f5f7fa", "#ffffff"), c("#f4faf5", "#ffffff"))
  for (xxx in seq_along(colour_coding)) {
    formatted_table <- formatted_table |>
      huxtable::map_background_color(colour_coding[[xxx]], huxtable::everywhere, huxtable::by_rows(alternate_colours[[1]])) |>
      huxtable::set_background_color(colour_coding[[xxx]], c(2, no_of_cols), alternate_colours[[1]][1]) |>
      huxtable::set_bottom_border(max(colour_coding[[xxx]]), huxtable::everywhere, huxtable::brdr(1.5, style = "solid", "grey70"))
    alternate_colours <- rev(alternate_colours)
  }

  # ---- 8. Drop unnecessary columns ----
  if (numeric_outcomes_only) formatted_table <- formatted_table |> select(-3)   # Levels column
  formatted_table <- formatted_table |> select(-1)                              # stat_mem column

  # ---- 9. Footnotes ----
  bases <- table[[2]]
  if (all(unlist(bases) != "NA")) {
    for (i in seq_along(bases)) {
      formatted_table <- formatted_table %>% huxtable::add_footnote(bases[[i]], border = NULL)
    }
  }

  formatted_table
}


# =============================================================================
# summary_table() — the combined single-step function
# =============================================================================
#
# calc_stats() -> pivot1() -> pivot1h() -> as_flextable(), in one call, for a
# Totals-only overview of numeric and categorical outcomes together.
#
# predictors is deliberately not exposed: pivot1() only ever keeps the Total
# row anyway, so passing predictors through would just compute crosstab
# breakdowns that get filtered straight back out.
#
# statistics defaults to c("mean", "perc") — calc_stats() already filters
# invalid stat/type combinations per outcome on its own (a numeric outcome
# only ever gets "mean" rows, a factor outcome only "perc" rows), so this one
# default line covers "numeric and categorical variables together" without
# summary_table() needing any type-detection logic of its own. Override if
# you want median/sum/count instead, or a weighted statistic with a survey
# design object as `data`.
#
# raw = TRUE returns pivot1()'s own output (list of tidy data frame + base
# description text) instead of the formatted flextable — useful if you want
# the numbers without the huxtable styling on top.

summary_table <- function(
    data,
    outcomes,
    statistics = c("mean", "perc"),
    conf       = NULL,
    base       = NULL,
    pval       = NULL,
    multicode  = TRUE,
    concise    = TRUE,
    raw        = FALSE
) {

  stats_table <- calc_stats(
    data       = data,
    outcomes   = outcomes,
    predictors = NULL,
    statistics = statistics,
    conf       = conf,
    base       = base,
    pval       = pval,
    multicode  = multicode
  )

  pivoted <- pivot1(stats_table, concise = concise)

  if (raw) return(pivoted)

  pivoted %>% pivot1h() %>% flextable::as_flextable()
}
