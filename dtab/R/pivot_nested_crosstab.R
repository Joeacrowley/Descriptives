# pivot_nested_crosstab() / format_nested_crosstab(): a crosstab nested one
# level deeper than pivot_crosstab() - one outer predictor (e.g. sex) with
# one inner predictor (e.g. age_group) nested inside each of its levels, a
# 4-row header per column:
#
#   row 1: outer variable's label,  spanning EVERY column
#   row 2: outer variable's levels, each spanning its own inner columns
#   row 3: inner variable's label,  repeated once per outer level
#   row 4: inner variable's levels, one per column, repeating under each
#          outer level
#
# Scoped to exactly one nesting level and one nested set per table - no
# side-by-side flat sets alongside it (pivot_crosstab()'s job), no
# arbitrary N-level nesting. calc_stats() already supports `predictors =
# list(c(outer_var, inner_var))` and returns predictor1/p_lab1/p_cat1
# (outer) + predictor2/p_lab2/p_cat2 (inner); pivot_crosstab() drops those.
#
# p-values: one per OUTER LEVEL ("among women, is age associated with this
# outcome"), not one for the whole nested set - calc_stats.R's
# nested_pvalues() computes it; this file surfaces it as one column per
# outer level (e.g. "p_value (age_group | Male)"), same multicode
# treatment as pivot_crosstab().
#
# Rectangularity: if some (outer, inner) combination has zero respondents,
# calc_stats() never produces a row for it, which would silently narrow
# one outer level's span by a column. reshape_nested_predictor_set() forces
# a full (outer, inner) grid so a missing combination is a blank column
# instead.
#
# A categorical/multicoded outcome missing a row for one o_cat LEVEL within
# an otherwise-populated (outer, inner) combination (e.g. zero "Yes" among
# Female/Old) no longer reaches this file at all - calc_stat_engine()'s
# group_by(.drop = FALSE) on the outcome axis restores it as a real 0
# upstream. The o_cat-crossed skeleton below still runs (for the
# Rectangularity case above), it just has nothing left to fill in for this
# one - the row already exists, so completion is a no-op rather than the
# active fix. The skeleton is built with distinct() + expand_grid() rather
# than tidyr::complete(), which sorts and would override the row order
# calc_stats() produced (alphabetical o_cat/outcome once o_cat is character,
# as it is after multicode collapsing).
# base still can't go through the same o_cat-crossed completion (a property
# of the (outer, inner) group, not any o_cat level) - it's computed from the
# grid directly, before that completion runs.


# `rows_for_set` is already filtered to one nested cross_break's rows
# (Total excluded). Builds one composite column per (outer, inner)
# combination - "Male: Young", "Male: Old", ... - since the bare inner
# level alone isn't unique across outer groups. header_info carries the
# outer/inner label and level for each column, so format_nested_crosstab()
# never has to re-parse the composite name.
reshape_nested_predictor_set <- function(rows_for_set) {

  outer_order <- rows_for_set %>% distinct(p_cat1) %>% pull(p_cat1) %>% fct_inorder() %>% levels()
  inner_order <- rows_for_set %>% distinct(p_cat2) %>% pull(p_cat2) %>% fct_inorder() %>% levels()

  outer_label <- rows_for_set$p_lab1[1]
  inner_label <- rows_for_set$p_lab2[1]

  # The rectangular grid - built before base is touched (see the header
  # note above on why base has to stay separate from the o_cat-crossed
  # completion below).
  grid <- tidyr::crossing(p_cat1 = factor(outer_order, levels = outer_order),
                           p_cat2 = factor(inner_order, levels = inner_order)) %>%
    arrange(p_cat1, p_cat2) %>%
    mutate(col_key = paste0(as.character(p_cat1), ": ", as.character(p_cat2)))

  col_order <- grid$col_key

  header_info <- grid %>%
    transmute(column_name = col_key,
              outer_label = outer_label, outer_level = as.character(p_cat1),
              inner_label = inner_label, inner_level = as.character(p_cat2))

  base_by_group <- rows_for_set %>%
    distinct(outcome, o_lab, p_cat1, p_cat2, base) %>%
    mutate(p_cat1 = factor(p_cat1, levels = outer_order),
           p_cat2 = factor(p_cat2, levels = inner_order))

  base_wide <- rows_for_set %>%
    distinct(outcome, o_lab) %>%
    tidyr::crossing(grid %>% select(p_cat1, p_cat2, col_key)) %>%
    left_join(base_by_group, by = c("outcome", "o_lab", "p_cat1", "p_cat2")) %>%
    select(outcome, o_lab, col_key, base) %>%
    pivot_wider(names_from = col_key, values_from = base) %>%
    select(outcome, o_lab, all_of(col_order))

  # Row skeleton: every (outcome, stat, o_cat) combination crossed with the
  # full (outer, inner) grid. Deliberately NOT tidyr::complete()/nesting()/
  # crossing(): those sort their output, so o_cat (and outcome) come back
  # alphabetical whenever the column is character - which it is as soon as
  # multicode collapsing has run (o_cat = right_stem, then bind_rows()
  # coerces the whole column). distinct() and expand_grid() keep order of
  # first appearance, i.e. the order calc_stats() produced, and every
  # later step (pivot_wider, fct_inorder, group_split) just preserves it.
  row_keys <- rows_for_set %>%
    distinct(outcome, o_lab, outcome_type, stat, o_cat)
  grid_keys <- grid %>% select(p_cat1, p_cat2)
  skeleton_index <- tidyr::expand_grid(row = seq_len(nrow(row_keys)),
                                       col = seq_len(nrow(grid_keys)))
  skeleton <- bind_cols(row_keys[skeleton_index$row, ], grid_keys[skeleton_index$col, ])

  completed <- skeleton %>%
    left_join(
      rows_for_set %>%
        mutate(p_cat1 = factor(p_cat1, levels = outer_order),
               p_cat2 = factor(p_cat2, levels = inner_order)),
      by = c("outcome", "o_lab", "outcome_type", "stat", "o_cat", "p_cat1", "p_cat2")
    ) %>%
    mutate(col_key = paste0(as.character(p_cat1), ": ", as.character(p_cat2)))

  has_ci <- "estimate_ci" %in% names(completed)
  has_se <- "estimate_se" %in% names(completed)
  conf_col <- if (has_ci) "estimate_ci" else if (has_se) "estimate_se" else NA_character_

  estimate_wide <- completed %>%
    select(outcome, o_lab, o_cat, stat, outcome_type, col_key, estimate) %>%
    pivot_wider(names_from = col_key, values_from = estimate) %>%
    select(outcome, o_lab, o_cat, stat, outcome_type, all_of(col_order))

  if (!is.na(conf_col)) {
    conf_wide <- completed %>%
      select(outcome, o_lab, o_cat, stat, outcome_type, col_key, value = all_of(conf_col)) %>%
      pivot_wider(names_from = col_key, values_from = value) %>%
      select(outcome, o_lab, o_cat, stat, outcome_type, all_of(col_order)) %>%
      rename_with(~ paste0(.x, "__conf"), all_of(col_order))
    estimate_wide <- estimate_wide %>%
      left_join(conf_wide, by = c("outcome", "o_lab", "o_cat", "stat", "outcome_type"))
  }

  if ("sig_diff" %in% names(completed)) {
    sigdiff_wide <- completed %>%
      select(outcome, o_lab, o_cat, stat, outcome_type, col_key, value = sig_diff) %>%
      pivot_wider(names_from = col_key, values_from = value) %>%
      select(outcome, o_lab, o_cat, stat, outcome_type, all_of(col_order)) %>%
      rename_with(~ paste0(.x, "__sigdiff"), all_of(col_order))
    estimate_wide <- estimate_wide %>%
      left_join(sigdiff_wide, by = c("outcome", "o_lab", "o_cat", "stat", "outcome_type"))
  }

  # legend: built from rows_for_set, not the o_cat-crossed `completed`
  # grid - sig_letter is constant per (p_cat1, p_cat2), same as
  # pivot_crosstab()'s legend.
  legend <- if ("sig_letter" %in% names(rows_for_set)) {
    rows_for_set %>%
      filter(!is.na(sig_letter)) %>%
      distinct(p_cat1, p_cat2, sig_letter) %>%
      mutate(level_col = paste0(as.character(p_cat1), ": ", as.character(p_cat2))) %>%
      select(level_col, sig_letter)
  } else {
    NULL
  }

  list(estimate_wide = estimate_wide, base_wide = base_wide,
       col_order = col_order, header_info = header_info, legend = legend)
}


#' Reshape `calc_stats()` output into a nested crosstab (two-variable columns)
#'
#' For a single NESTED predictor set (`predictors = list(c(<outer>,
#' <inner>))`) - a 4-row header with the outer variable's levels
#' (e.g. Sex) each split into the inner variable's levels (e.g. AgeGroup)
#' beneath it, one p-value column per outer level ("among women, is age
#' associated with this outcome?"). For a flat (single-variable, or several
#' single-variable) predictor set instead, use [pivot_crosstab()].
#'
#' @param data A tibble as returned by [calc_stats()], called with a nested
#'   `predictors` set (`predictors = list(c(<outer variable>,
#'   <inner variable>))`).
#'
#' @return A list: `pivoted`, `bases`, `col_order`, `header_info`,
#'   `conf_type` ("se", "ci", or `NA`), and `legend` (a tibble, or `NULL`
#'   if `pairwise = TRUE` wasn't used). Pass this list straight to
#'   [format_nested_crosstab()].
#'
#' @seealso [format_nested_crosstab()], [pivot_crosstab()], [calc_stats()]
#' @keywords internal
pivot_nested_crosstab <- function(data) {

  if (!all(c("p_cat2", "p_lab2", "predictor2") %in% names(data))) {
    stop("pivot_nested_crosstab() needs a nested predictor set - call calc_stats() with ",
         "predictors = list(c(<outer variable>, <inner variable>)). Use pivot_crosstab() ",
         "instead for a flat (single-variable) predictor.")
  }

  bases <- data %>% pull(base_description) %>% unique() %>% paste0(collapse = " X ")
  if (any(!is.na(bases))) bases <- bases %>% prepare_base_for_table()
  data <- data %>% select(-base_description)

  if ("estimate_se" %in% names(data) && all(data$estimate_se == "-")) {
    data <- data %>% select(-estimate_se)
  }

  conf_type <- if ("estimate_ci" %in% names(data)) "ci" else if ("estimate_se" %in% names(data)) "se" else NA_character_

  # Total isn't shown as its own column here - there's no natural place
  # for it inside a 2-tier nested header.
  nested_rows <- data %>% filter(p_cat1 != "Total")

  nested_sets <- nested_rows %>% pull(cross_break) %>% unique()
  if (length(nested_sets) == 0) {
    stop("No nested predictor breakdown found in this data - only Total rows were present.")
  }
  if (length(nested_sets) > 1) {
    stop("pivot_nested_crosstab() only supports one nested predictor set at a time - found: ",
         paste(nested_sets, collapse = ", "))
  }

  cat_stat_counts <- nested_rows %>% filter(outcome_type %in% c("categorical", "multicoded")) %>%
    distinct(outcome, stat) %>% count(outcome)
  offending <- cat_stat_counts %>% filter(n > 1) %>% pull(outcome)
  if (length(offending) > 0) {
    stop("pivot_nested_crosstab() only supports one statistic per categorical variable. ",
         "Multiple statistics were requested for: ", paste(offending, collapse = ", "))
  }

  set_result <- reshape_nested_predictor_set(nested_rows)

  estimate_wide <- set_result$estimate_wide
  base_wide     <- set_result$base_wide
  col_order     <- set_result$col_order
  header_info   <- set_result$header_info
  legend        <- set_result$legend

  pivot_one_block <- function(block) {
    is_categorical <- block$outcome_type[1] %in% c("categorical", "multicoded")

    data_rows <- block %>%
      mutate(Variable = if (is_categorical) o_cat else o_lab[1],
             row_type  = "data") %>%
      select(Variable, row_type, outcome, o_lab, o_cat, stat, outcome_type,
             all_of(col_order), any_of(paste0(col_order, "__conf")),
             any_of(paste0(col_order, "__sigdiff")))

    if (!is_categorical) return(data_rows)

    label_row <- data_rows[1, ] %>%
      mutate(Variable = block$o_lab[1], row_type = "label", o_cat = NA_character_) %>%
      mutate(across(c(all_of(col_order), any_of(paste0(col_order, "__conf")),
                      any_of(paste0(col_order, "__sigdiff"))), ~ NA))

    bind_rows(label_row, data_rows)
  }

  pivoted <- estimate_wide %>%
    mutate(.block = fct_inorder(paste(outcome, stat, sep = "___"))) %>%
    group_split(.block) %>%
    map_df(pivot_one_block) %>%
    mutate(stat_code = stat)

  outcomes_in_order <- pivoted %>% pull(o_lab) %>% unique()

  pivoted <- map_df(outcomes_in_order, function(this_lab) {
    variable_rows <- pivoted %>% filter(o_lab == this_lab)
    base_row <- base_wide %>% filter(o_lab == this_lab) %>%
      mutate(Variable = this_lab, stat_code = NA_character_, row_type = "base",
             o_cat = NA_character_, stat = NA_character_,
             outcome_type = variable_rows$outcome_type[1])
    bind_rows(variable_rows, base_row)
  })

  nested_p_values <- nested_rows %>%
    filter(!is.na(p_value)) %>%
    distinct(outcome, o_cat, p_cat1, p_value)

  inner_label <- header_info$inner_label[1]

  if (nrow(nested_p_values) > 0) {
    for (lvl in unique(header_info$outer_level)) {
      p_col_name  <- paste0("p_value (", inner_label, " | ", lvl, ")")
      lvl_pvalues <- nested_p_values %>% filter(p_cat1 == lvl) %>% select(-p_cat1)
      if (nrow(lvl_pvalues) == 0) next

      pivoted <- pivoted %>%
        left_join(lvl_pvalues, by = c("outcome", "o_cat")) %>%
        group_by(outcome) %>%
        mutate(data_row_number = cumsum(row_type == "data"),
               "{p_col_name}" := case_when(
                 row_type != "data" ~ NA_real_,
                 outcome_type == "multicoded" ~ p_value,
                 data_row_number == 1 ~ p_value,
                 TRUE ~ NA_real_
               )) %>%
        ungroup() %>%
        select(-p_value, -data_row_number)

      # Right after that outer level's own last inner column, not
      # wherever the loop happened to append it.
      last_col_for_lvl <- header_info %>% filter(outer_level == lvl) %>%
        pull(column_name) %>% utils::tail(1)
      pivoted <- pivoted %>% relocate(all_of(p_col_name), .after = all_of(last_col_for_lvl))
    }
  }

  # o_lab is kept - format_nested_crosstab() needs it for block-boundary
  # detection once Base rows no longer sit inline.
  pivoted <- pivoted %>% select(-outcome, -o_cat)

  pivoted <- pivoted %>%
    tidy_statistic_description() %>%
    mutate(Statistics = case_when(
      row_type == "label" ~ NA_character_,
      row_type == "base"  ~ NA_character_,
      TRUE ~ Statistics
    ),
    stat_code = if_else(row_type == "label", NA_character_, stat_code)) %>%
    relocate(Variable, Statistics, .before = 1) %>%
    relocate(row_type, stat_code, outcome_type, .after = dplyr::last_col())

  # Every variable's Base row relocates into one consolidated "Sample
  # sizes" section at the bottom. The synthetic section-title row is given
  # row_type = "label" (not a new type) so format_nested_crosstab() bolts
  # on its top-padding gap via the same logic every label row gets.
  base_rows <- pivoted %>% filter(row_type == "base")
  if (nrow(base_rows) > 0) {
    base_header <- base_rows %>%
      dplyr::slice(1) %>%
      mutate(Variable = "Sample sizes", row_type = "label", Statistics = NA_character_,
             across(c(all_of(col_order), any_of(paste0(col_order, "__conf")),
                      any_of(paste0(col_order, "__sigdiff")),
                      dplyr::starts_with("p_value")), ~ NA))

    pivoted <- bind_rows(pivoted %>% filter(row_type != "base"), base_header, base_rows)
  }

  list(pivoted, bases, col_order, header_info, conf_type, legend)
}


# format_nested_crosstab(): most of the digit formatting, conf-as-
# inserted-row, block padding, row merging, and styling below is
# format_crosstab() almost unchanged. Two new pieces: the 4-row header,
# and p-value columns per OUTER LEVEL (each outer level's span widens to
# fold in its own p-value column).
#
# Row insertion order: insert_row(after = 0) always drops the new row at
# the top, so the three header rows are inserted and populated in reverse
# final order (row 3 first, then row 2, then row 1), incrementing each
# already-tracked row number as the next insert pushes it down.

#' Style `pivot_nested_crosstab()` output as a huxtable
#'
#' Builds the printable nested crosstab: the 4-row header (outer label,
#' outer levels, inner label, inner levels), SE/CI rows inserted under
#' each data row when present, a Sig. diff row (and legend block) per
#' outer level when `pivot_nested_crosstab()` was given pairwise-tested
#' data, and `format_statistic()`'s digit rules applied per row's own
#' `stat_code`.
#'
#' @param pivot_result The list returned by [pivot_nested_crosstab()].
#'
#' @return A styled `huxtable` object, ready to print or pass to
#'   [export_table_to_excel()].
#'
#' @seealso [pivot_nested_crosstab()]
#' @keywords internal
format_nested_crosstab <- function(pivot_result) {

  data        <- pivot_result[[1]]
  bases       <- pivot_result[[2]]
  col_order   <- pivot_result[[3]]
  header_info <- pivot_result[[4]]
  conf_type   <- pivot_result[[5]]
  legend      <- pivot_result[[6]]

  conf_cols    <- intersect(paste0(col_order, "__conf"), names(data))
  sigdiff_cols <- intersect(paste0(col_order, "__sigdiff"), names(data))
  pval_cols    <- names(data)[stringr::str_starts(names(data), "p_value")]

  has_pairwise <- length(sigdiff_cols) > 0

  is_base_row <- data$row_type == "base"
  for (col in col_order) {
    data[[col]] <- dplyr::if_else(
      is_base_row,
      dplyr::if_else(is.na(data[[col]]), NA_character_,
                      formatC(data[[col]], digits = 0, big.mark = ",", format = "f")),
      format_statistic(data[[col]], data$stat_code)
    )
  }
  if (!is.na(conf_type)) {
    for (col in conf_cols) {
      data[[col]] <- if (conf_type == "ci") {
        format_ci_string(data[[col]], data$stat_code)
      } else {
        format_statistic(data[[col]], data$stat_code)
      }
    }
  }
  for (col in pval_cols) {
    data[[col]] <- format_pvalue(data[[col]])
  }

  if (!is.na(conf_type) && length(conf_cols) > 0 && !has_pairwise) {
    conf_label <- if (conf_type == "ci") "95% CI" else "SE"

    data <- map_df(seq_len(nrow(data)), function(i) {
      row <- data[i, , drop = FALSE]
      has_conf <- row$row_type == "data" &&
        any(!is.na(unlist(row[, conf_cols, drop = FALSE])))

      if (!has_conf) return(row %>% select(-all_of(conf_cols)))

      conf_row <- row %>%
        mutate(Variable = "", Statistics = conf_label,
               row_type = "conf", stat_code = NA_character_)
      for (lc in col_order) {
        cc <- paste0(lc, "__conf")
        if (cc %in% names(row)) conf_row[[lc]] <- row[[cc]]
      }
      for (pc in pval_cols) conf_row[[pc]] <- NA_character_

      bind_rows(row %>% select(-all_of(conf_cols)),
                conf_row %>% select(-all_of(conf_cols)))
    })
  } else {
    data <- data %>% select(-any_of(conf_cols))
  }

  if (has_pairwise) {
    data <- map_df(seq_len(nrow(data)), function(i) {
      row <- data[i, , drop = FALSE]
      sd_values <- unlist(row[, sigdiff_cols, drop = FALSE])
      has_sigdiff <- row$row_type == "data" && any(!is.na(sd_values))

      if (!has_sigdiff) return(row %>% select(-all_of(sigdiff_cols)))

      sigdiff_row <- row %>%
        mutate(Variable = "", Statistics = "Sig. diff",
               row_type = "sigdiff", stat_code = NA_character_)
      for (lc in col_order) {
        sc <- paste0(lc, "__sigdiff")
        if (sc %in% names(row)) sigdiff_row[[lc]] <- row[[sc]]
      }
      for (pc in pval_cols) sigdiff_row[[pc]] <- NA_character_

      bind_rows(row %>% select(-all_of(sigdiff_cols)),
                sigdiff_row %>% select(-all_of(sigdiff_cols)))
    })
  } else {
    data <- data %>% select(-any_of(sigdiff_cols))
  }

  if (!is.null(legend) && nrow(legend) > 0) {
    legend_wide <- legend %>% pivot_wider(names_from = level_col, values_from = sig_letter)

    legend_row <- data %>% dplyr::slice(0) %>% tibble::add_row()
    legend_row$Variable     <- "Column reference"
    legend_row$row_type     <- "legend"
    legend_row$Statistics   <- NA_character_
    legend_row$o_lab        <- data$o_lab[1]
    legend_row$outcome_type <- data$outcome_type[1]
    for (lc in col_order) {
      if (lc %in% names(legend_wide)) legend_row[[lc]] <- legend_wide[[lc]][1]
    }

    data <- bind_rows(legend_row, data)
  }

  # Block boundaries via o_lab, not "the row after a base row" - that
  # proxy stopped working once every base row relocates to the bottom
  # section. A numeric data row starts a new block when its o_lab differs
  # from the row before it - immune to a conf row blanking Variable, and
  # to more than one statistic sharing the same outcome (mean AND median
  # for "age" share one o_lab, so aren't treated as different variables).
  new_block <- data$row_type == "label" |
    (data$row_type == "data" & data$outcome_type == "numeric" &
       dplyr::lag(data$o_lab, default = "") != data$o_lab)
  block_id <- cumsum(new_block)

  row_type     <- data$row_type
  outcome_type <- data$outcome_type

  ht <- data %>% select(-row_type, -stat_code, -outcome_type, -o_lab) %>%
    huxtable::as_hux(add_colnames = TRUE)

  variable_col  <- which(names(ht) == "Variable")
  level_col_idx <- which(names(ht) %in% col_order)

  # header_info is in the same left-to-right order as col_order (both
  # built from the same arrange(p_cat1, p_cat2)), so level_col_idx[k] and
  # header_info's row k always describe the same physical column.
  outer_runs <- rle(header_info$outer_level)
  run_starts <- cumsum(c(1, utils::head(outer_runs$lengths, -1)))
  outer_label <- header_info$outer_label[1]
  inner_label <- header_info$inner_label[1]

  outer_group_cols <- purrr::map(seq_along(run_starts), function(i) {
    lvl <- outer_runs$values[i]
    lvl_level_cols <- level_col_idx[run_starts[i]:(run_starts[i] + outer_runs$lengths[i] - 1)]
    p_col_name <- paste0("p_value (", inner_label, " | ", lvl, ")")
    c(lvl_level_cols, which(names(ht) == p_col_name))
  })
  all_group_cols <- sort(unlist(outer_group_cols))

  ht <- huxtable::insert_row(ht, rep("", ncol(ht)), after = 0)
  inner_label_row <- 1L
  for (i in seq_along(run_starts)) {
    cols_i <- outer_group_cols[[i]]
    ht[inner_label_row, min(cols_i)] <- inner_label
    if (length(cols_i) > 1) ht <- huxtable::merge_cells(ht, inner_label_row, range(cols_i))
  }

  ht <- huxtable::insert_row(ht, rep("", ncol(ht)), after = 0)
  outer_level_row <- 1L
  inner_label_row <- inner_label_row + 1L
  for (i in seq_along(run_starts)) {
    cols_i <- outer_group_cols[[i]]
    ht[outer_level_row, min(cols_i)] <- outer_runs$values[i]
    if (length(cols_i) > 1) ht <- huxtable::merge_cells(ht, outer_level_row, range(cols_i))
  }

  ht <- huxtable::insert_row(ht, rep("", ncol(ht)), after = 0)
  outer_label_row <- 1L
  outer_level_row <- outer_level_row + 1L
  inner_label_row <- inner_label_row + 1L
  ht[outer_label_row, min(all_group_cols)] <- outer_label
  if (length(all_group_cols) > 1) ht <- huxtable::merge_cells(ht, outer_label_row, range(all_group_cols))

  header_offset <- 4L
  bare_row <- header_offset   # the original as_hux(add_colnames = TRUE) row, now row 4

  ht[bare_row, level_col_idx] <- header_info$inner_level

  pval_col_idx <- which(names(ht) %in% pval_cols)
  if (length(pval_col_idx) > 0) ht[bare_row, pval_col_idx] <- "p-value"

  ht <- ht %>%
    huxtable::set_align(seq_len(3), huxtable::everywhere, "center") %>%
    huxtable::set_bold(seq_len(header_offset), huxtable::everywhere, TRUE) %>%
    huxtable::set_bottom_border(header_offset, huxtable::everywhere,
                                 huxtable::brdr(1, "solid", "grey40"))

  # Vertical divider between outer groups (header and body).
  group_start_cols <- level_col_idx[run_starts]
  group_start_cols <- setdiff(group_start_cols, min(level_col_idx))
  if (length(group_start_cols) > 0) {
    ht <- huxtable::set_left_border(ht, huxtable::everywhere, group_start_cols,
                                     huxtable::brdr(1, "solid", "grey60"))
  }

  no_of_rows     <- nrow(ht)
  data_rows      <- which(row_type == "data") + header_offset
  conf_rows      <- which(row_type == "conf") + header_offset
  sigdiff_rows   <- which(row_type == "sigdiff") + header_offset
  legend_hux_row <- which(row_type == "legend") + header_offset

  ht <- ht %>%
    huxtable::set_na_string(value = "") %>%
    huxtable::set_na_string(row = c(data_rows, conf_rows, sigdiff_rows), col = level_col_idx, value = "-")

  ht <- ht %>%
    huxtable::set_align(huxtable::everywhere, variable_col, "left") %>%
    huxtable::set_align(huxtable::everywhere, which(names(ht) == "Statistics"), "left") %>%
    huxtable::set_align(header_offset:no_of_rows,
                         which(!names(ht) %in% c("Variable", "Statistics")), "right")

  is_variable_row <- row_type == "label" | (row_type == "data" & outcome_type == "numeric")
  variable_name_rows <- which(is_variable_row) + header_offset
  ht <- huxtable::set_bold(ht, row = variable_name_rows, col = variable_col, value = TRUE)

  merge_group <- dplyr::if_else(
    row_type %in% c("data", "conf") & outcome_type == "numeric",
    paste0("numeric_", block_id),
    paste0("nomerge_", seq_along(row_type))
  )
  run_lengths <- rle(merge_group)$lengths
  merge_run_starts <- cumsum(c(1, utils::head(run_lengths, -1)))

  for (i in seq_along(merge_run_starts)) {
    if (run_lengths[i] > 1) {
      start_row <- merge_run_starts[i] + header_offset
      span_len  <- run_lengths[i]
      ht <- huxtable::set_rowspan(ht, row = start_row, col = variable_col, value = span_len)
    }
  }

  if (length(legend_hux_row) > 0) {
    statistics_col <- which(names(ht) == "Statistics")
    ht <- huxtable::merge_cells(ht, legend_hux_row, range(c(variable_col, statistics_col)))
    ht <- huxtable::set_italic(ht, legend_hux_row, huxtable::everywhere, TRUE)
  }

  base_data_rows <- which(row_type == "base")
  if (length(base_data_rows) > 0) {
    statistics_col <- which(names(ht) == "Statistics")
    section_header_row <- min(base_data_rows) - 1
    merge_rows <- c(section_header_row, base_data_rows) + header_offset
    for (r in merge_rows) {
      ht <- huxtable::merge_cells(ht, r, range(c(variable_col, statistics_col)))
    }
  }

  ht <- huxtable::stripe_rows(ht, stripe1 = "#f5f7fa", stripe2 = "#ffffff")

  ht <- ht %>%
    huxtable::set_all_borders(huxtable::everywhere, huxtable::everywhere,
                               huxtable::brdr(0.5, "solid", "grey85")) %>%
    huxtable::set_bottom_border(header_offset, huxtable::everywhere,
                                 huxtable::brdr(1, "solid", "grey40")) %>%
    huxtable::set_all_padding(1) %>%
    huxtable::set_font_size(8) %>%
    huxtable::set_font("Arial") %>%
    huxtable::set_bold(row = seq_len(header_offset), col = huxtable::everywhere, value = TRUE) %>%
    huxtable::set_background_color(seq_len(header_offset), huxtable::everywhere, "grey95") %>%
    huxtable::set_valign("middle")

  block_start_rows <- which(new_block) + header_offset
  ht <- huxtable::set_top_padding(ht, row = block_start_rows, col = huxtable::everywhere, value = 6)

  if (length(legend_hux_row) > 0) {
    ht <- ht %>%
      huxtable::set_valign(legend_hux_row, huxtable::everywhere, "top") %>%
      huxtable::set_top_padding(legend_hux_row, huxtable::everywhere, 1) %>%
      huxtable::set_bottom_padding(legend_hux_row, huxtable::everywhere, 6)
  }

  if (all(unlist(bases) != "NA")) {
    for (i in seq_along(bases)) {
      ht <- ht %>% huxtable::add_footnote(bases[[i]], border = NULL)
    }
  }

  ht
}
