# pivot_crosstab() / format_crosstab(): reshape calc_stats() output into a
# crosstab - predictor levels become table columns. One or more FLAT
# predictor sets shown side by side; a nested (2-variable) set is out of
# scope here (use pivot_nested_crosstab()) and its columns are dropped.
#
# With more than one predictor set, each set's level columns are prefixed
# "<predictor label>: " to avoid collisions (e.g. two sets both having a
# "Yes" level) - "Total" stays bare, merged from the first set only.
#
# SE/CI live in a parallel "<level>__conf" column alongside the estimate,
# not spread into their own columns.
#
# Multicoded p-values: a multicoded variable's levels each carry their own
# independent p-value rather than one shared across the variable - each
# level was still a separate raw outcome column when calc_stats() computed
# p-values, before convert_multicodes() collapsed them. This is why the
# p-value join is keyed on (outcome, o_cat, stat_type), not just (outcome,
# stat_type) - o_cat is what lets each level find its own p-value instead
# of all of them fanning out to match once several levels share one
# collapsed `outcome` name.


# Classifies a stat into one of 4 test types - two stats of the same type
# (mean and median, both "num_u") share one p-value from calc_stats(), not
# one each. NA (via case_when()'s default) for anything unrecognised, e.g.
# a base row.
stat_type_of <- function(stat) {
  case_when(
    stat %in% c("count", "perc")               ~ "cat_u",
    stat %in% c("w_count", "w_perc")            ~ "cat_w",
    stat %in% c("mean", "median", "sum",
                "min", "max", "range", "iqr", "sd") ~ "num_u",
    stat %in% c("w_mean", "w_median", "w_sum",
                "w_iqr", "w_sd")                ~ "num_w"
  )
}

# Everything specific to ONE predictor's slice: estimate/conf/base wide on
# its own levels, plus its own p-value data. `prefix` is TRUE only when
# more than one set is present.
reshape_one_predictor_set <- function(rows_for_set, p_lab, prefix) {

  rows_for_set <- rows_for_set %>%
    mutate(p_cat1_label = if (prefix) {
      if_else(p_cat1 == "Total", "Total", paste0(p_lab, ": ", p_cat1))
    } else {
      as.character(p_cat1)
    })

  level_order <- rows_for_set %>% filter(p_cat1 != "Total") %>%
    pull(p_cat1_label) %>% fct_inorder() %>% levels()
  if ("Total" %in% rows_for_set$p_cat1) level_order <- c("Total", level_order)

  has_ci <- "estimate_ci" %in% names(rows_for_set)
  has_se <- "estimate_se" %in% names(rows_for_set)
  conf_col <- if (has_ci) "estimate_ci" else if (has_se) "estimate_se" else NA_character_

  # outcome_type rides along as an id column so it doesn't pick up a
  # ".x"/".y" suffix when the later full_join() merges predictor sets.
  estimate_wide <- rows_for_set %>%
    select(outcome, o_lab, o_cat, stat, outcome_type, p_cat1_label, estimate) %>%
    pivot_wider(names_from = p_cat1_label, values_from = estimate) %>%
    select(outcome, o_lab, o_cat, stat, outcome_type, all_of(level_order))

  if (!is.na(conf_col)) {
    conf_wide <- rows_for_set %>%
      select(outcome, o_lab, o_cat, stat, outcome_type, p_cat1_label, value = all_of(conf_col)) %>%
      pivot_wider(names_from = p_cat1_label, values_from = value) %>%
      select(outcome, o_lab, o_cat, stat, outcome_type, all_of(level_order)) %>%
      rename_with(~ paste0(.x, "__conf"), all_of(level_order))
    estimate_wide <- estimate_wide %>%
      left_join(conf_wide, by = c("outcome", "o_lab", "o_cat", "stat", "outcome_type"))
  }

  # sig_diff (calc_stats(pairwise = TRUE)) gets the same "<level>__sigdiff"
  # treatment as conf - reused as-is by pivot_battery.R (keyed on
  # category) and pivot_nested_crosstab.R (keyed on the composite
  # outer:inner column).
  if ("sig_diff" %in% names(rows_for_set)) {
    sigdiff_wide <- rows_for_set %>%
      select(outcome, o_lab, o_cat, stat, outcome_type, p_cat1_label, value = sig_diff) %>%
      pivot_wider(names_from = p_cat1_label, values_from = value) %>%
      select(outcome, o_lab, o_cat, stat, outcome_type, all_of(level_order)) %>%
      rename_with(~ paste0(.x, "__sigdiff"), all_of(level_order))
    estimate_wide <- estimate_wide %>%
      left_join(sigdiff_wide, by = c("outcome", "o_lab", "o_cat", "stat", "outcome_type"))
  }

  base_wide <- rows_for_set %>%
    distinct(outcome, o_lab, p_cat1_label, base) %>%
    pivot_wider(names_from = p_cat1_label, values_from = base) %>%
    select(outcome, o_lab, all_of(level_order))

  p_values <- rows_for_set %>%
    filter(!is.na(p_value)) %>%
    mutate(stat_type = stat_type_of(stat)) %>%
    distinct(outcome, o_cat, stat_type, p_value)

  # legend: a plain long-format (level_col, sig_letter) lookup, not a
  # letter baked into a still-numeric estimate column - that would force
  # bind_rows() to coerce the whole column to character once tables
  # combine. format_crosstab() builds the display row from this after its
  # own digit-formatting already made the level columns character.
  legend <- if ("sig_letter" %in% names(rows_for_set)) {
    rows_for_set %>%
      filter(p_cat1 != "Total", !is.na(sig_letter)) %>%
      distinct(level_col = p_cat1_label, sig_letter)
  } else {
    NULL
  }

  list(estimate_wide = estimate_wide, base_wide = base_wide,
       p_values = p_values, p_lab = p_lab, level_order = level_order,
       legend = legend)
}

#' Reshape `calc_stats()` output into a crosstab, predictor levels as columns
#'
#' One or more FLAT predictor sets (each `predictors` element of length 1),
#' shown side by side if more than one was requested - a Sex block next to
#' an AgeGroup block, for example. For a single 2-variable nested predictor
#' set instead, use [pivot_nested_crosstab()], not this function.
#'
#' Supports `calc_stats(pairwise = TRUE)` output: a `legend` (returned as
#' the 6th list element) and `"<level>__sigdiff"` columns feed
#' [format_crosstab()]'s inserted legend row and Sig. diff row. A
#' categorical variable is limited to exactly one statistic per call;
#' a numeric variable can have more than one (e.g. mean + median).
#'
#' @param data A tibble as returned by [calc_stats()], with at least one
#'   flat predictor (`predictors` not `NULL`, and no element of length > 1).
#'
#' @return A list: `pivoted`, `bases`, `all_level_cols`, `predictor_sets`
#'   (a named vector - `cross_break` as names, display label as values),
#'   `conf_type` ("se", "ci", or `NA`), and `legend` (a tibble, or `NULL`
#'   if `pairwise = TRUE` wasn't used). Pass this list straight to
#'   [format_crosstab()].
#'
#' @seealso [format_crosstab()], [pivot_nested_crosstab()], [calc_stats()]
#' @keywords internal
pivot_crosstab <- function(data) {

  data <- data %>%
    select(-any_of(grep("p_lab[2-9]|p_cat[2-9]|predictor[2-9]", names(data), value = TRUE)))

  bases <- data %>% pull(base_description) %>% unique() %>% paste0(collapse = " X ")
  if (any(!is.na(bases))) bases <- bases %>% prepare_base_for_table()
  data <- data %>% select(-base_description)

  if ("estimate_se" %in% names(data) && all(data$estimate_se == "-")) {
    data <- data %>% select(-estimate_se)
  }

  conf_type <- if ("estimate_ci" %in% names(data)) "ci" else if ("estimate_se" %in% names(data)) "se" else NA_character_

  predictor_sets <- data %>% filter(cross_break != "Total") %>%
    mutate(cross_break = fct_inorder(cross_break)) %>% pull(cross_break) %>% levels()
  if (length(predictor_sets) == 0) {
    stop("pivot_crosstab() needs at least one predictor - use pivot_summary() for a Total-only table.")
  }

  cat_stat_counts <- data %>% filter(outcome_type %in% c("categorical", "multicoded")) %>%
    distinct(outcome, stat) %>% count(outcome)
  offending <- cat_stat_counts %>% filter(n > 1) %>% pull(outcome)
  if (length(offending) > 0) {
    stop("pivot_crosstab() only supports one statistic per categorical variable. ",
         "Multiple statistics were requested for: ", paste(offending, collapse = ", "))
  }

  multiple_sets <- length(predictor_sets) > 1

  rows_by_set <- map(seq_along(predictor_sets), function(i) {
    if (i == 1) data %>% filter(cross_break %in% c("Total", predictor_sets[i]))
    else        data %>% filter(cross_break == predictor_sets[i])
  })

  p_labs <- map_chr(rows_by_set, function(rows) {
    rows %>% filter(p_lab1 != "Total") %>% pull(p_lab1) %>% unique() %>% .[1]
  })
  if (any(duplicated(p_labs))) {
    stop("Predictor sets must have distinct labels - found a repeated label: ",
         paste(p_labs[duplicated(p_labs)], collapse = ", "))
  }

  set_results <- map2(rows_by_set, p_labs, ~ reshape_one_predictor_set(.x, .y, prefix = multiple_sets))

  estimate_wide <- reduce(map(set_results, "estimate_wide"), full_join,
                          by = c("outcome", "o_lab", "o_cat", "stat", "outcome_type"))
  base_wide <- reduce(map(set_results, "base_wide"), full_join, by = c("outcome", "o_lab"))

  all_level_cols <- map(set_results, "level_order") %>% unlist(use.names = FALSE) %>% unique()

  pivot_one_block <- function(block) {
    # outcome_type is kept in the select() below - the p-value loop
    # further down needs it to tell a multicoded variable's per-level
    # p-values apart from an ordinary shared-p-value block.
    is_categorical <- block$outcome_type[1] %in% c("categorical", "multicoded")

    data_rows <- block %>%
      mutate(Variable = if (is_categorical) o_cat else o_lab[1],
             row_type  = "data") %>%
      select(Variable, row_type, outcome, o_lab, o_cat, stat, outcome_type,
             all_of(all_level_cols), any_of(paste0(all_level_cols, "__conf")),
             any_of(paste0(all_level_cols, "__sigdiff")))

    if (!is_categorical) return(data_rows)

    label_row <- data_rows[1, ] %>%
      mutate(Variable = block$o_lab[1], row_type = "label", o_cat = NA_character_) %>%
      mutate(across(c(all_of(all_level_cols), any_of(paste0(all_level_cols, "__conf")),
                      any_of(paste0(all_level_cols, "__sigdiff"))), ~ NA))

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

  # One p-value column per predictor set. Ordinary variable: shared once
  # per (outcome, stat type) block, shown on the first data row. Multicoded
  # variable: each data row keeps its own independent p-value instead.
  for (i in seq_along(set_results)) {
    p_values_i <- set_results[[i]]$p_values
    if (nrow(p_values_i) == 0) next
    p_lab_i <- set_results[[i]]$p_lab

    pivoted <- pivoted %>%
      mutate(stat_type = stat_type_of(stat)) %>%
      left_join(p_values_i, by = c("outcome", "o_cat", "stat_type")) %>%
      group_by(outcome, stat_type) %>%
      mutate(data_row_number = cumsum(row_type == "data"),
             p_value = case_when(
               row_type != "data" ~ NA_real_,
               outcome_type == "multicoded" ~ p_value,
               data_row_number == 1 ~ p_value,
               TRUE ~ NA_real_
             )) %>%
      ungroup() %>%
      select(-stat_type, -data_row_number)

    names(pivoted)[names(pivoted) == "p_value"] <- paste0("p_value (", p_lab_i, ")")
  }

  for (i in seq_along(set_results)) {
    pval_col_i <- paste0("p_value (", p_labs[i], ")")
    if (pval_col_i %in% names(pivoted)) {
      last_level_col_i <- utils::tail(set_results[[i]]$level_order, 1)
      pivoted <- pivoted %>% relocate(all_of(pval_col_i), .after = all_of(last_level_col_i))
    }
  }

  # o_lab and outcome_type are kept (unlike outcome/o_cat): format_crosstab()
  # needs o_lab for block-boundary detection once a Base row no longer sits
  # directly under its own variable, and outcome_type to rebuild an
  # is_variable_row-equivalent distinction.
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
    relocate(row_type, stat_code, outcome_type, o_lab, .after = dplyr::last_col())

  # Every variable's Base row relocates into one consolidated "Sample
  # sizes" section at the bottom, rather than sitting directly under each
  # variable's own data (same layout choice as pivot_nested_crosstab()).
  base_rows <- pivoted %>% filter(row_type == "base")
  if (nrow(base_rows) > 0) {
    base_header <- base_rows %>%
      dplyr::slice(1) %>%
      mutate(Variable = "Sample sizes", row_type = "label", Statistics = NA_character_,
             across(c(all_of(all_level_cols), any_of(paste0(all_level_cols, "__conf")),
                      any_of(paste0(all_level_cols, "__sigdiff")),
                      dplyr::starts_with("p_value")), ~ NA))

    pivoted <- bind_rows(pivoted %>% filter(row_type != "base"), base_header, base_rows)
  }

  legend <- map(set_results, "legend") %>% compact() %>% bind_rows()
  if (nrow(legend) == 0) legend <- NULL

  # predictor_sets: cross_break (raw name) as names, p_lab (display label)
  # as values - format_crosstab() needs p_lab for spanning headers, and a
  # single predictor set's own level columns stay unprefixed, so there'd
  # be nowhere else to recover the label from.
  list(pivoted, bases, all_level_cols, setNames(p_labs, predictor_sets), conf_type, legend)
}


# format_crosstab(): reuses format_statistic()/format_ci_string() from
# pivot_summary.R. Adds spanning headers over each predictor set's level
# columns (built from `predictor_sets`), SE/CI as an inserted row, and -
# for calc_stats(pairwise = TRUE) - a Sig. diff row and a legend row.
# Block boundaries come from o_lab, not "the row after a base row" - that
# proxy stopped working once Base rows relocate to one section at the
# bottom.
format_pvalue <- function(p) {
  case_when(
    is.na(p)  ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE      ~ formatC(p, digits = 3, format = "f")
  )
}

#' Style `pivot_crosstab()` output as a huxtable
#'
#' Builds the printable crosstab: bolds variable names, inserts SE/CI rows
#' under each data row when present, inserts a Sig. diff row (and a legend
#' block above the table) when `pivot_crosstab()` was given pairwise-tested
#' data, and applies `format_statistic()`'s digit rules per row's own
#' `stat_code`.
#'
#' @param pivot_result The list returned by [pivot_crosstab()].
#'
#' @return A styled `huxtable` object, ready to print or pass to
#'   [export_table_to_excel()].
#'
#' @seealso [pivot_crosstab()]
#' @keywords internal
format_crosstab <- function(pivot_result) {

  data           <- pivot_result[[1]]
  bases          <- pivot_result[[2]]
  all_level_cols <- pivot_result[[3]]
  predictor_sets <- pivot_result[[4]]
  conf_type      <- pivot_result[[5]]
  legend         <- pivot_result[[6]]

  conf_cols    <- intersect(paste0(all_level_cols, "__conf"), names(data))
  sigdiff_cols <- intersect(paste0(all_level_cols, "__sigdiff"), names(data))
  pval_cols    <- names(data)[stringr::str_starts(names(data), "p_value")]

  has_pairwise <- length(sigdiff_cols) > 0

  # A base row's counts live in the same level columns as every other
  # row's estimates, but stat_code is NA there - branched to a plain
  # digits=0/comma-separated rule instead of format_statistic()'s
  # generic (and wrong-looking, e.g. "6.000000") fallback.
  is_base_row <- data$row_type == "base"
  for (col in all_level_cols) {
    data[[col]] <- dplyr::if_else(
      is_base_row,
      dplyr::if_else(is.na(data[[col]]), NA_character_,
                      formatC(data[[col]], digits = 0, big.mark = ",", format = "f")),
      format_statistic(data[[col]], data$stat_code)
    )
  }
  for (col in pval_cols) {
    data[[col]] <- format_pvalue(data[[col]])
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

  # Insert one extra "conf" row under every data row with a non-NA conf
  # value - suppressed entirely when pairwise sig_diff is shown instead.
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
      for (lc in all_level_cols) {
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

  # Same mechanic for a "sigdiff" row. Empty string ("tested, nothing
  # significant") counts as a real result and gets a row; NA ("never
  # tested") is the only case that skips it.
  if (has_pairwise) {
    data <- map_df(seq_len(nrow(data)), function(i) {
      row <- data[i, , drop = FALSE]
      diff_values <- unlist(row[, sigdiff_cols, drop = FALSE])
      has_diff <- row$row_type == "data" && any(!is.na(diff_values))

      if (!has_diff) return(row %>% select(-all_of(sigdiff_cols)))

      sigdiff_row <- row %>%
        mutate(Variable = "", Statistics = "Sig. diff",
               row_type = "sigdiff", stat_code = NA_character_)
      for (lc in all_level_cols) {
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

  # Legend row, built after digit-formatting has already made every level
  # column character (see reshape_one_predictor_set()'s legend note).
  if (!is.null(legend) && nrow(legend) > 0) {
    legend_row <- data %>% dplyr::slice(0) %>% tibble::add_row()
    legend_row$Variable     <- "Column reference"
    legend_row$Statistics   <- NA_character_
    legend_row$row_type     <- "legend"
    legend_row$o_lab        <- data$o_lab[1]
    legend_row$outcome_type <- data$outcome_type[1]
    for (i in seq_len(nrow(legend))) {
      lc <- legend$level_col[i]
      if (lc %in% names(legend_row)) legend_row[[lc]] <- legend$sig_letter[i]
    }
    data <- bind_rows(legend_row, data)
  }

  new_block <- data$row_type %in% c("label", "legend") |
    dplyr::lag(data$row_type, default = "") == "legend" |
    (data$row_type == "data" & data$outcome_type == "numeric" &
       dplyr::lag(data$o_lab, default = "") != data$o_lab)
  block_id <- cumsum(new_block)

  row_type     <- data$row_type
  outcome_type <- data$outcome_type

  ht <- data %>% select(-row_type, -stat_code, -outcome_type, -o_lab) %>%
    huxtable::as_hux(add_colnames = TRUE)

  header_offset <- 1L
  variable_col  <- which(names(ht) == "Variable")
  level_col_idx <- which(names(ht) %in% all_level_cols)

  spans <- purrr::imap(predictor_sets, function(p_lab, cb) {
    level_cols <- if (length(predictor_sets) == 1) {
      setdiff(all_level_cols, "Total")
    } else {
      all_level_cols[stringr::str_starts(all_level_cols, paste0(p_lab, ": "))]
    }
    pval_col <- paste0("p_value (", p_lab, ")")
    cols <- c(level_cols, intersect(pval_col, names(data)))
    list(p_lab = p_lab, cols = cols)
  })
  spans <- purrr::keep(spans, ~ length(.x$cols) > 0)

  if (length(spans) > 0) {
    ht <- huxtable::insert_row(ht, rep("", ncol(ht)), after = 0)
    header_offset <- 2L

    for (s in spans) {
      col_idx <- which(names(ht) %in% s$cols)
      ht[1, min(col_idx)] <- s$p_lab
      if (length(col_idx) > 1) {
        ht <- huxtable::merge_cells(ht, 1, range(col_idx))
      }

      prefix_i <- paste0(s$p_lab, ": ")
      pval_col_i <- paste0("p_value (", s$p_lab, ")")
      bare_labels <- dplyr::case_when(
        stringr::str_starts(s$cols, stringr::fixed(prefix_i)) ~ stringr::str_sub(s$cols, nchar(prefix_i) + 1),
        s$cols == pval_col_i ~ "p_value",
        TRUE ~ s$cols
      )
      ht[header_offset, col_idx] <- bare_labels
    }
    spanned_cols <- unique(unlist(purrr::map(spans, "cols")))
    ht <- ht %>%
      huxtable::set_align(1, huxtable::everywhere, "center") %>%
      huxtable::set_bold(1, huxtable::everywhere, TRUE) %>%
      huxtable::set_bottom_border(1, which(names(ht) %in% spanned_cols),
                                   huxtable::brdr(1, "solid", "grey40"))
  }

  no_of_rows   <- nrow(ht)
  data_rows    <- which(row_type == "data") + header_offset
  conf_rows    <- which(row_type == "conf") + header_offset
  sigdiff_rows <- which(row_type == "sigdiff") + header_offset

  # Total is excluded from "-" on a sigdiff row specifically - it's never
  # part of a pairwise comparison, so blank reads correctly there; "-"
  # would read as "should have a letter but doesn't".
  non_total_level_idx <- level_col_idx[names(ht)[level_col_idx] != "Total"]

  ht <- ht %>%
    huxtable::set_na_string(value = "") %>%
    huxtable::set_na_string(row = c(data_rows, conf_rows), col = level_col_idx, value = "-") %>%
    huxtable::set_na_string(row = sigdiff_rows, col = non_total_level_idx, value = "-")

  # Right-alignment scoped to header_offset:no_of_rows, not "everywhere" -
  # "everywhere" would overwrite the spanning header's center alignment.
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
  run_starts  <- cumsum(c(1, utils::head(run_lengths, -1)))

  # No follow-up blanking of covered cells - set_rowspan() copies the
  # anchor cell's content into every cell it covers, so a covered cell
  # already reads back as that content regardless.
  for (i in seq_along(run_starts)) {
    if (run_lengths[i] > 1) {
      start_row <- run_starts[i] + header_offset
      span_len  <- run_lengths[i]
      ht <- huxtable::set_rowspan(ht, row = start_row, col = variable_col, value = span_len)
    }
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

  legend_hux_row <- which(row_type == "legend") + header_offset
  if (length(legend_hux_row) > 0) {
    statistics_col <- which(names(ht) == "Statistics")
    ht <- huxtable::merge_cells(ht, legend_hux_row, range(c(variable_col, statistics_col)))
    ht <- huxtable::set_italic(ht, legend_hux_row, huxtable::everywhere, TRUE)
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

  # Set after the blanket set_valign()/set_all_padding() above, or these
  # would be immediately overwritten. Asymmetric padding, not just
  # set_valign("top") alone - equal padding on both sides makes "top"
  # look identical to centred.
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
