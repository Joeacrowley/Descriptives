# =============================================================================
# calc_stats(): computes weighted/unweighted descriptive statistics broken
# down by zero or more predictors. Self-contained - everything it needs is
# defined in this file.
# =============================================================================


# ---- shared engine for the 10 mean/median/sum/perc/count functions ----------

calc_stat_engine <- function(data, outcomes, predictors = NULL, conf = NULL,
                             base = NULL, stat_label, compute,
                             group_on_outcome = FALSE) {

  suppressMessages(suppressWarnings({

    one_outcome_table <- function(cur_outcome, cur_predictor = NULL) {

      vars_needed <- c(cur_predictor, cur_outcome)
      base_definition <- if (is.null(base)) NA else
        create_bases(base_info = base, variables = vars_needed)

      group_vars <- cur_predictor
      if (group_on_outcome) group_vars <- c(group_vars, cur_outcome)

      filtered <- data %>% filter(rowSums(across(all_of(vars_needed), is.na)) == 0)

      # group_on_outcome (perc/count): .drop = FALSE restores an outcome
      # category with zero observations as a real 0 row, rather than
      # silently dropping it - a 0 is a genuine, computable answer here
      # (the group's total is known regardless of which categories were
      # picked), unlike a missing predictor level, which has no base to
      # compute anything from. fct_drop() on the predictor first keeps
      # that restoration scoped to the outcome axis - without it, .drop =
      # FALSE would also manufacture rows for a predictor level with zero
      # respondents at all, which divides out to NaN (unweighted) or a
      # hard error in survey_prop() (weighted, empty-group GLM).
      if (group_on_outcome && !is.null(cur_predictor)) {
        filtered <- filtered %>% mutate(across(all_of(cur_predictor), fct_drop))
      }

      grouped <- if (length(group_vars) > 0) {
        group_by(filtered, across(all_of(group_vars)), .drop = !group_on_outcome)
      } else {
        filtered
      }

      tbl <- compute(grouped = grouped, filtered = filtered,
                     outcome = cur_outcome, predictor = cur_predictor, conf = conf)

      # mean/median/sum stamp the stat name into o_cat (e.g. "mean") since
      # there's no real category; perc/count skip this - that column
      # already holds the real category being tabulated.
      if (!group_on_outcome) tbl <- tbl %>% mutate("{cur_outcome}" := stat_label)

      tbl <- tbl %>% mutate(base_description = base_definition)

      if (is.null(cur_predictor)) {
        tbl <- tbl %>%
          mutate(outcome = cur_outcome) %>%
          rename(any_of(c(o_cat = cur_outcome))) %>%
          mutate(o_lab = label_or_var_name(pull_outcome_var(data, cur_outcome), cur_outcome))
      } else {
        tbl <- tbl %>%
          standardise_names(data = data, preds = cur_predictor, out_var = cur_outcome) %>%
          mutate(cross_break = paste0(cur_predictor, collapse = "_X_"))
      }
      tbl
    }

    output <- map(outcomes, function(cur_outcome) {

      total_table <- one_outcome_table(cur_outcome)

      predictor_tables <- NULL
      if (!is.null(predictors)) {
        predictor_tables <- predictors %>%
          discard(~ length(.x) == 0) %>%
          map(~ one_outcome_table(cur_outcome, cur_predictor = .x)) %>%
          bind_rows()
      }

      bind_rows(total_table, predictor_tables) %>%
        mutate(stat = stat_label,
               outcome_type = if (group_on_outcome) "categorical" else "numeric") %>%
        mutate(across(contains(c("cross_break", "predictor", "p_lab", "p_cat")),
                      ~ if_else(is.na(.x), "Total", as.character(.x)))) %>%
        select(contains(c("cross_break", "predictor", "p_lab", "p_cat")),
               outcome, o_lab, o_cat, stat, outcome_type, contains("estimate"),
               base, base_description, any_of("unweighted_n")) %>%
        apply_conf_columns(conf)
    })

    result <- output %>% bind_rows()
    # mean/median/sum have one row per group, so unweighted_n == base.
    # perc/count already set their own per-category unweighted_n inside
    # `compute` and must not have it overwritten here.
    if (!group_on_outcome) result <- result %>% mutate(unweighted_n = base)
    result
  }))
}

# Survey objects store variables under $variables; plain data frames don't.
pull_outcome_var <- function(data, cur_outcome) {
  if (inherits(data, "tbl_svy")) data$variables[[cur_outcome]] else data[[cur_outcome]]
}

label_or_var_name <- function(x, var_name) {
  lab <- attr(x, "label")
  if (is.null(lab)) var_name else lab
}

apply_conf_columns <- function(tbl, conf) {
  if (is.null(conf)) {
    tbl %>% mutate(estimate_se = "-", .after = estimate) %>%
      select(-any_of(c("estimate_low", "estimate_upp")))
  } else if (conf == "se") {
    tbl %>% select(-any_of(c("estimate_low", "estimate_upp")))
  } else { # "ci"
    tbl %>% mutate(estimate_ci = if_else(is.na(estimate_low) | is.na(estimate_upp),
                                          NA_character_,
                                          paste0(estimate_low, " - ", estimate_upp))) %>%
      select(-any_of("estimate_se"))
  }
}


# ---- mean --------------------------------------------------------------------

weighted_mean <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "w_mean", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate = survey_mean(!!sym(outcome), vartype = conf),
                             base = unweighted(n()))
    })
}

unweighted_mean <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "mean", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate     = mean(!!sym(outcome), na.rm = TRUE),
                             n            = n(),
                             sd           = sd(!!sym(outcome), na.rm = TRUE),
                             estimate_se  = sd / sqrt(n),
                             estimate_low = estimate - 1.96 * estimate_se,
                             estimate_upp = estimate + 1.96 * estimate_se,
                             base         = sum(n)) %>%
        select(-n, -sd)
    })
}


# ---- median --------------------------------------------------------------------
# unweighted_median has no analytic SE/CI, so grouped_medianse()/
# grouped_medianci() do their own split-by-group work below.

weighted_median <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "w_median", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>%
        mutate("{outcome}" := haven::zap_labels(.data[[outcome]])) %>%
        summarise(estimate = survey_median(!!sym(outcome), vartype = conf),
                  base = unweighted(n()))
    })
}

unweighted_median <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "median", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {

      point_estimate <- grouped %>% summarise(estimate = median(!!sym(outcome), na.rm = TRUE),
                                               base = n())

      if (is.null(conf)) return(point_estimate)

      if (is.null(predictor)) {
        if (conf == "se") {
          return(point_estimate %>% mutate(estimate_se = wrappedtools::medianse(filtered %>% pull(outcome))))
        } else {
          ci <- wrappedtools::median_cl_boot(filtered %>% pull(outcome))
          return(point_estimate %>% mutate(estimate_low = ci$CIlow, estimate_upp = ci$CIhigh))
        }
      }

      # grouped_medianci() drops any subgroup with 5 or fewer observations,
      # so this full_join can leave some rows with a median but NA CI.
      addon <- if (conf == "se") {
        grouped_medianse(data = filtered, outcome = outcome, predictors = predictor)
      } else {
        grouped_medianci(data = filtered, outcome = outcome, predictors = predictor)
      }
      point_estimate %>% full_join(addon, by = predictor)
    })
}


# ---- sum --------------------------------------------------------------------

weighted_sum <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "w_sum", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>%
        mutate("{outcome}" := haven::zap_labels(.data[[outcome]])) %>%
        summarise(estimate = survey_total(!!sym(outcome), vartype = conf),
                  base = unweighted(n()))
    })
}

unweighted_sum <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "sum", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate     = sum(!!sym(outcome), na.rm = TRUE),
                             base         = n(),
                             sd           = sd(!!sym(outcome), na.rm = TRUE),
                             estimate_se  = sd * sqrt(base),
                             estimate_low = estimate - 1.96 * estimate_se,
                             estimate_upp = estimate + 1.96 * estimate_se) %>%
        select(-sd)
    })
}


# ---- min / max / range --------------------------------------------------------
# Unweighted only - a sample's observed min/max/range isn't a population
# estimate weighting would adjust. No analytic SE/CI either.

unweighted_min <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "min", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate = min(!!sym(outcome), na.rm = TRUE),
                             base     = n())
    })
}

unweighted_max <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "max", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate = max(!!sym(outcome), na.rm = TRUE),
                             base     = n())
    })
}

unweighted_range <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "range", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate = max(!!sym(outcome), na.rm = TRUE) -
                                        min(!!sym(outcome), na.rm = TRUE),
                             base     = n())
    })
}


# ---- IQR -----------------------------------------------------------------------
# Point estimate only, both weighted and unweighted - no ready-made
# bootstrap CI helper for IQR the way median has.

unweighted_iqr <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "iqr", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate = IQR(!!sym(outcome), na.rm = TRUE),
                             base     = n())
    })
}

# survey_quantile() names output columns "<name>_q<p*100>" - both
# quantiles requested under one stem so they come out predictably named.
weighted_iqr <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "w_iqr", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>%
        mutate("{outcome}" := haven::zap_labels(.data[[outcome]])) %>%
        summarise(quantiles = survey_quantile(!!sym(outcome), c(0.25, 0.75), vartype = NULL),
                  base      = unweighted(n())) %>%
        mutate(estimate = quantiles_q75 - quantiles_q25) %>%
        select(-quantiles_q25, -quantiles_q75)
    })
}


# ---- standard deviation ---------------------------------------------------------
# Point estimate only - weighted is sqrt() of survey_var()'s estimate.

unweighted_sd <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "sd", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate = sd(!!sym(outcome), na.rm = TRUE),
                             base     = n())
    })
}

weighted_sd <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "w_sd", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate = sqrt(survey_var(!!sym(outcome), na.rm = TRUE)),
                             base     = unweighted(n()))
    })
}


# ---- percentage --------------------------------------------------------------
# group_on_outcome = TRUE for both perc and count: the outcome's own
# categories are part of what gets grouped on, since the outcome IS the
# thing being tabulated.

weighted_perc <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "w_perc", group_on_outcome = TRUE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate = survey_prop(vartype = conf),
                             unweighted_n = unweighted(n())) %>%
        mutate(base = sum(unweighted_n, na.rm = TRUE))
    })
}

unweighted_perc <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "perc", group_on_outcome = TRUE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      counted <- grouped %>% summarise(unweighted_n = n())
      if (!is.null(predictor)) counted <- counted %>% group_by(across(all_of(predictor)))
      counted %>%
        mutate(estimate     = unweighted_n / sum(unweighted_n, na.rm = TRUE),
               estimate_se  = sqrt(estimate * (1 - estimate) / sum(unweighted_n, na.rm = TRUE)),
               estimate_low = estimate - 1.96 * estimate_se,
               estimate_upp = estimate + 1.96 * estimate_se,
               base         = sum(unweighted_n, na.rm = TRUE))
    })
}


# ---- count --------------------------------------------------------------------

weighted_count <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "w_count", group_on_outcome = TRUE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate = survey_total(vartype = conf),
                             unweighted_n = unweighted(n())) %>%
        mutate(base = sum(unweighted_n, na.rm = TRUE))
    })
}

unweighted_count <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "count", group_on_outcome = TRUE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      counted <- grouped %>% summarise(estimate = n())
      if (!is.null(predictor)) counted <- counted %>% group_by(across(all_of(predictor)))
      counted %>%
        mutate(base         = sum(estimate, na.rm = TRUE),
               prop         = estimate / base,
               estimate_se  = sqrt(base * prop * (1 - prop)),
               estimate_low = estimate - 1.96 * estimate_se,
               estimate_upp = estimate + 1.96 * estimate_se) %>%
        select(-prop)
    })
}


# ---- calc_stats' dispatch: registry instead of 10 if-blocks ------------------

stat_registry <- list(
  mean     = unweighted_mean,  median   = unweighted_median,
  sum      = unweighted_sum,   perc     = unweighted_perc,
  count    = unweighted_count,
  min      = unweighted_min,   max      = unweighted_max,
  range    = unweighted_range, iqr      = unweighted_iqr,
  sd       = unweighted_sd,
  w_mean   = weighted_mean,    w_median = weighted_median,
  w_sum    = weighted_sum,     w_perc   = weighted_perc,
  w_count  = weighted_count,
  w_iqr    = weighted_iqr,     w_sd     = weighted_sd
)



#' Compute weighted or unweighted descriptive statistics, broken down by
#' zero or more predictors
#'
#' The engine every `pivot_*()`/`make_*()` function in this package builds
#' on: one long-format tibble, one row per (outcome, category, predictor
#' level, statistic) combination, computed either on a plain data frame
#' (unweighted) or a survey design object (weighted statistics and
#' design-based SEs/CIs available). Handles categorical, multicoded, and
#' numeric outcomes; flat, nested, or multiple side-by-side predictor
#' sets; optional SE/CI and p-values.
#'
#' @param data A data frame, or a survey design object (e.g. from
#'   `srvyr::as_survey_design()`) for weighted statistics.
#' @param outcomes Character vector of outcome variable names.
#' @param predictors `NULL` for no breakdown (Total only), a character
#'   vector/single variable name, a list of variable names for multiple
#'   side-by-side flat predictor sets, or `list(c(<outer>, <inner>))` for
#'   a nested predictor set. All predictors must be factors.
#' @param statistics Character vector of statistic codes to compute, e.g.
#'   `"perc"`, `"count"`, `"mean"`, `"median"`, `"sum"`, `"min"`, `"max"`,
#'   `"range"`, `"iqr"`, `"sd"`, and their weighted `w_`-prefixed
#'   equivalents (weighted versions require a survey design as `data`).
#' @param conf `NULL`, `"se"`, or `"ci"` - adds a standard error or
#'   confidence interval alongside each estimate.
#' @param base Optional base/filter expression restricting which cases
#'   each outcome is computed over.
#' @param pval Optional p-value method for testing association between an
#'   outcome and a predictor (e.g. chi-square for categorical, ANOVA/
#'   Shapiro-Wilk-gated for numeric).
#' @param multicode If `TRUE` (default), an outcome coded as multiple
#'   binary/dummy columns sharing a common stem is treated as a single
#'   multicoded (select-all-that-apply) variable rather than several
#'   independent ones.
#' @param pairwise If `TRUE`, adds `sig_letter`/`sig_diff` columns from
#'   pairwise significance testing between predictor levels (one t-test/
#'   `svyttest()` per pair, Holm-adjusted within each comparison set,
#'   BH-adjusted again across the full set before flagging significance).
#'   Only the outcome's preferred statistic is tested (weighted over
#'   unweighted, mean/perc over their `w_`-prefixed equivalents where both
#'   are available), and only flat or single-level-nested predictor sets.
#' @param pw_outcome_levels Optional named list restricting which outcome
#'   categories get pairwise-tested (names are outcome variable names,
#'   values are the categories to test) - only used when `pairwise = TRUE`.
#' @param pw_crit_lev Significance threshold for `sig_diff` flagging when
#'   `pairwise = TRUE`. Default `0.05`.
#'
#' @return A long-format tibble with one row per (outcome, category,
#'   predictor level, statistic): `outcome`/`o_lab` (variable name/label),
#'   `o_cat` (category, for categorical/multicoded outcomes),
#'   `cross_break`/`p_cat1`/`p_lab` (predictor variable/level/label, and
#'   `p_cat2`/`p_lab2`/`predictor2` when a nested predictor set was used),
#'   `stat` (the statistic code), `estimate` (and `estimate_se`/
#'   `estimate_ci_low`/`estimate_ci_high` if `conf` was requested), `base`
#'   (and `base_description`), plus `p_value` when requested and
#'   `sig_letter`/`sig_diff` when `pairwise = TRUE`. Feed this
#'   straight into [pivot_summary()], [pivot_crosstab()],
#'   [pivot_nested_crosstab()], [pivot_battery()] - or use one of the
#'   `make_*()` wrappers to skip calling `calc_stats()` directly.
#'
#' @seealso [pivot_summary()], [pivot_crosstab()],
#'   [pivot_nested_crosstab()], [pivot_battery()], [make_table()]
#' @export
calc_stats <- function(
    data,
    outcomes,
    predictors = NULL,
    statistics = c("count", "mean"),
    conf = NULL,
    base = NULL,
    pval = NULL,
    multicode = TRUE,
    pairwise = FALSE,
    pw_outcome_levels = NULL,
    pw_crit_lev = 0.05
) {

  if (is.character(predictors)) predictors <- list(predictors)

  vector_of_variables_concerned <- c(outcomes, unlist(unique(predictors)))
  if (!isTRUE(vars_exist(data = data, variable_list = vector_of_variables_concerned))) {
    stop("Some variables you want to use do not exist in the data frame.")
  }

  if (!is.null(predictors)) {
    if (!isTRUE(outcomes_not_in_predictors(outcomes = outcomes, predictors = predictors))) {
      stop("Outcome appears in predictor list.")
    }
    if (!isTRUE(check_all_factors(data = data, variable_list = predictors))) {
      stop("Not all predictors are factor variables. Make them factors.")
    }
    if (list_depth(predictors) != 1) {
      stop("Predictor list should not contained further lists.")
    }
  }

  suppressMessages(suppressWarnings({

    if (any(str_detect(class(data), "survey"))) {
      unweighted_data <- data[["variables"]]
      weighted_data   <- data
    } else {
      unweighted_data <- data
      weighted_data   <- NULL
    }

    individual_tables <- map(outcomes, function(cur_outcome) {

      single_outcome <- unweighted_data %>% pull(cur_outcome)
      statistic <- statistics
      if (is.factor(single_outcome))  statistic <- str_remove_all(statistic, "mean|median|sum|min|max|range|iqr|sd")
      if (is.numeric(single_outcome)) statistic <- str_remove_all(statistic, "perc|count")

      results <- list()
      for (stat in intersect(statistic, names(stat_registry))) {
        is_weighted <- str_starts(stat, "w_")
        if (is_weighted && is.null(weighted_data)) next

        fn <- stat_registry[[stat]]

        results <- append(results, list(fn(
          data       = if (is_weighted) weighted_data else unweighted_data,
          outcomes   = cur_outcome,
          predictors = predictors,
          conf       = conf,
          base       = base
        )))
      }

      if (length(results) > 0) results <- reduce(results, bind_rows)
      results
    })

    merged_tables <- individual_tables %>% bind_rows()

    if (!is.null(pval)) {
      pvalue_preds <- predictors[predictors %>% map_lgl(~ length(.x) == 1)]
      pvalue_preds <- pvalue_preds %>% unlist() %>% unique()

      if (length(pvalue_preds) == 0) {
        merged_tables <- merged_tables %>% mutate(p_method = NA, p_value = NA)
      } else {
        pvalues <- list()
        if (any(statistics %in% c("mean", "median", "sum", "perc", "count",
                                   "min", "max", "range", "iqr", "sd"))) {
          pvalues <- append(pvalues, list(
            map_return_p_values(data = unweighted_data, outcomes = outcomes, predictors = pvalue_preds)
          ))
        }
        if (!is.null(weighted_data) &&
            any(statistics %in% c("w_mean", "w_median", "w_sum", "w_perc", "w_count",
                                   "w_iqr", "w_sd"))) {
          pvalues <- append(pvalues, list(
            map_return_p_values(data = data, outcomes = outcomes, predictors = pvalue_preds)
          ))
        }
        pvalues <- bind_rows(pvalues)
        merged_tables <- merged_tables %>% left_join(pvalues)
      }

      # Nested (2-variable) predictor sets get their own p-value per outer
      # level (nested_pvalues()), independent of the flat branch above.
      nested_preds <- predictors[predictors %>% map_lgl(~ length(.x) == 2)]
      if (length(nested_preds) > 0) {
        nested_pvalue_rows <- list()
        for (np in nested_preds) {
          outer_var <- np[1]; inner_var <- np[2]
          if (any(statistics %in% c("mean", "median", "sum", "perc", "count",
                                     "min", "max", "range", "iqr", "sd"))) {
            nested_pvalue_rows <- append(nested_pvalue_rows, list(
              nested_pvalues(unweighted_data, outcomes, outer_var, inner_var)
            ))
          }
          if (!is.null(weighted_data) &&
              any(statistics %in% c("w_mean", "w_median", "w_sum", "w_perc", "w_count",
                                     "w_iqr", "w_sd"))) {
            nested_pvalue_rows <- append(nested_pvalue_rows, list(
              nested_pvalues(data, outcomes, outer_var, inner_var)
            ))
          }
        }
        nested_pvalue_rows <- bind_rows(nested_pvalue_rows)

        if (nrow(nested_pvalue_rows) > 0) {
          # coalesce(), not overwrite - a mixed flat+nested call already
          # gave the flat rows their own p_value; this fills in the rest.
          merged_tables <- merged_tables %>%
            left_join(nested_pvalue_rows %>%
                        select(outcome, cross_break, p_cat1, stat,
                               nested_p_value = p_value, nested_p_method = p_method),
                      by = c("outcome", "cross_break", "p_cat1", "stat")) %>%
            mutate(p_value  = dplyr::coalesce(p_value, nested_p_value),
                   p_method = dplyr::coalesce(p_method, nested_p_method)) %>%
            select(-nested_p_value, -nested_p_method)
        }
      }
    } else {
      merged_tables <- merged_tables %>% mutate(p_method = NA, p_value = NA)
    }

    if (is.null(predictors)) merged_tables <- merged_tables %>% mutate(cross_break = "Total")

    if (isTRUE(pairwise)) {
      merged_tables <- add_pairwise_sig(merged_tables, data = data, outcomes = outcomes,
                                         predictors = predictors, statistics = statistics,
                                         pw_outcome_levels = pw_outcome_levels,
                                         pw_crit_lev = pw_crit_lev)
    }

    if (isTRUE(multicode)) {
      merged_tables <- convert_multicodes(data = merged_tables, base_info = base)
    }

    merged_tables
  }))
}


# ---- significance tests: same scaffold-plus-closures fix ---------------------

run_assoc_test <- function(data, outcome, predictor, weighted, stat_labels, test) {

  plain_data <- if (weighted) data[["variables"]] else data
  x_lab <- plain_data %>% select(all_of(predictor)) %>% var_label(unlist = TRUE, null_action = "fill")
  y_lab <- plain_data %>% select(all_of(outcome))   %>% var_label(unlist = TRUE, null_action = "fill")

  test_result <- tryCatch(test(data, outcome, predictor),
                           error = function(e) list(method = "Significance test returned an error.", p = NA))

  output_tbl <- tibble(cross_break = predictor, predictor1 = predictor, p_lab1 = x_lab,
                        outcome = outcome, o_lab = y_lab,
                        p_method = test_result$method, p_value = test_result$p)

  map_df(stat_labels, ~ output_tbl %>% mutate(stat = .x))
}

unweighted_test_numeric_by_cat <- function(data, outcome, predictor) {
  run_assoc_test(data, outcome, predictor, weighted = FALSE,
    stat_labels = c("mean", "median", "sum", "min", "max", "range", "iqr", "sd"),
    test = function(data, outcome, predictor) {
      x <- data %>% pull(predictor); y <- data %>% pull(outcome)
      keep <- complete.cases(y, x); y <- y[keep]; x <- x[keep]
      model <- lm(y ~ x)
      if (shapiro.test(residuals(model))$p.value < 0.05) {
        list(method = "Kruskal-Wallis", p = kruskal.test(y ~ x)$p.value)
      } else {
        list(method = "Welch's ANOVA", p = oneway.test(y ~ x, var.equal = FALSE)$p.value)
      }
    })
}

weighted_test_numeric_by_cat <- function(data, outcome, predictor) {
  run_assoc_test(data, outcome, predictor, weighted = TRUE,
    stat_labels = c("w_mean", "w_median", "w_sum", "w_iqr", "w_sd"),
    test = function(data, outcome, predictor) {
      filtered <- data %>% filter(if_all(all_of(c(outcome, predictor)), ~ !is.na(.x)))
      frmla <- paste0(outcome, " ~ ", predictor)
      model <- survey::svyglm(frmla, design = filtered)
      if (shapiro.test(residuals(model))$p.value < 0.05) {
        list(method = "Kruskal-Wallis", p = survey::svyranktest(frmla, design = filtered)$p.value)
      } else {
        list(method = "Wald Test", p = survey::regTermTest(model, predictor)[["p"]] %>% as.vector())
      }
    })
}

unweighted_test_cat_by_cat <- function(data, outcome, predictor) {
  run_assoc_test(data, outcome, predictor, weighted = FALSE, stat_labels = c("perc", "count"),
    test = function(data, outcome, predictor) {
      x <- data %>% pull(predictor); y <- data %>% pull(outcome)
      keep <- complete.cases(y, x); y <- y[keep]; x <- x[keep]
      list(method = "Chi-Square test", p = chisq.test(x, y)$p.value)
    })
}

weighted_test_cat_by_cat <- function(data, outcome, predictor) {
  run_assoc_test(data, outcome, predictor, weighted = TRUE, stat_labels = c("w_perc", "w_count"),
    test = function(data, outcome, predictor) {
      filtered <- data %>% filter(if_all(all_of(c(outcome, predictor)), ~ !is.na(.x)))
      frmla <- formula(paste0("~", outcome, " + ", predictor))
      list(method = "Chi-Square test",
           p = survey::svychisq(frmla, design = filtered, statistic = "adjWald")$p.value %>% as.vector())
    })
}

test_registry <- list(
  "unweighted.factor"  = unweighted_test_cat_by_cat,
  "unweighted.numeric" = unweighted_test_numeric_by_cat,
  "weighted.factor"    = weighted_test_cat_by_cat,
  "weighted.numeric"   = weighted_test_numeric_by_cat
)

return_pvalues <- function(data, outcome, predictor) {
  weighted   <- any(str_detect(class(data), "survey"))
  plain_data <- if (weighted) data[["variables"]] else data
  is_factor  <- is.factor(plain_data %>% pull(outcome))
  key <- paste(if (weighted) "weighted" else "unweighted", if (is_factor) "factor" else "numeric", sep = ".")
  test_registry[[key]](data = data, outcome = outcome, predictor = predictor)
}

# One p-value per outer level ("among women, is age associated with this
# outcome"), not one omnibus test across every cell - reuses
# return_pvalues() against a subset of the data per outer level.
nested_pvalues <- function(data, outcomes, outer_var, inner_var) {
  plain_data   <- if (any(str_detect(class(data), "survey"))) data[["variables"]] else data
  outer_levels <- plain_data %>% pull(outer_var) %>% unique() %>% na.omit() %>% as.character()

  map_df(outer_levels, function(lvl) {
    subset_data <- data %>% filter(.data[[outer_var]] == lvl)
    map_df(outcomes, function(out) {
      return_pvalues(data = subset_data, outcome = out, predictor = inner_var) %>%
        mutate(cross_break = paste0(outer_var, "_X_", inner_var), p_cat1 = lvl)
    })
  })
}

# ---- p-value fan-out across outcomes x predictors -----------------------------

map_return_p_values <- function(outcomes, predictors, data) {
  map_df(predictors, function(pred) {
    map_df(outcomes, function(out) {
      return_pvalues(data = data, outcome = out, predictor = pred)
    })
  })
}



# ---- input validation (called from calc_stats()) -----------------------------
list_depth <- function(x) pluck_depth(x) - 1

vars_exist <- function(variable_list, data) {
  df_to_check <- if (any(grepl("survey", class(data)))) data[["variables"]] else data
  predictors <- variable_list %>% unlist() %>% unique()
  variables_in_df <- names(df_to_check)
  variables_in_both <- intersect(predictors, variables_in_df)
  length(predictors) == length(variables_in_both)
}

check_all_factors <- function(data, variable_list) {
  df_to_check <- if (any(grepl("survey", class(data)))) data[["variables"]] else data
  df_to_check %>% select(all_of(unlist(variable_list))) %>%
    map_chr(., ~ paste0(class(.x), collapse = ", ")) %>% unique() == "factor"
}

# Can't reuse check_all_factors()'s class()-string-collapsing trick here:
# it relies on every factor column's class() being the single string
# "factor". A numeric column's class() isn't uniform (integer columns
# report "integer", double columns report "numeric"), so is.numeric()
# checks that directly instead.
check_all_numeric <- function(data, variable_list) {
  df_to_check <- if (any(grepl("survey", class(data)))) data[["variables"]] else data
  df_to_check %>% select(all_of(unlist(variable_list))) %>% map_lgl(is.numeric) %>% all()
}

outcomes_not_in_predictors <- function(outcomes, predictors) {
  predictor_vector <- unlist(predictors) %>% unique()
  overlap_of_predictors_and_outcomes <- intersect(outcomes, predictor_vector)
  length(overlap_of_predictors_and_outcomes) == 0
}


# ---- base descriptions (base_information -> create_bases -> prepare_base_for_table) ----

#' Package general and per-variable base descriptions for `calc_stats()`'s
#' `base` argument
#'
#' Bundles a general (fallback) base description with a named list of
#' variable-specific base descriptions, plus the variable labels needed to
#' render each one as readable footnote text. Feed the result to
#' `calc_stats()`'s `base` argument; `create_bases()`/
#' `prepare_base_for_table()` (internal) turn it into the actual footnote
#' text each `format_*()` function attaches to its table.
#'
#' @param data A data frame, or survey design object.
#' @param general_base Character string describing the base that applies
#'   to any outcome without its own entry in `specific_bases`.
#' @param specific_bases A named list, names are outcome variable names,
#'   values are that outcome's own base description (overrides
#'   `general_base` for that variable).
#'
#' @return A list of 4 elements consumed internally by `calc_stats()` and
#'   its footnote-building helpers: `general_base`, `specific_bases`, the
#'   variable labels for variables in `specific_bases`, and the variable
#'   labels for every variable in `data`.
#'
#' @seealso [calc_stats()]
#' @keywords internal
base_information <- function(data, general_base, specific_bases) {
  everything_else <- general_base
  var_descriptions <- specific_bases
  variable_labels <- data %>% select(names(var_descriptions)) %>%
    var_label(unlist = TRUE, null_action = "fill")
  all_variable_labels <- data %>% var_label(unlist = TRUE, null_action = "fill")
  list(everything_else, var_descriptions, variable_labels, all_variable_labels)
}

create_bases <- function(base_info, variables) {
  names_of_variables_with_specific_bases <- base_info[[2]] %>% names()
  bases_to_use <- names_of_variables_with_specific_bases %in% variables
  variable_labels_to_use <- base_info[[3]][bases_to_use]
  bases_to_use <- base_info[[2]][bases_to_use]

  list_of_bases <- if (length(bases_to_use) > 0) {
    paste0(bases_to_use, ":-  ", variable_labels_to_use)
  } else {
    c()
  }

  variables_using_default_description <- variables[!variables %in% names_of_variables_with_specific_bases]
  if (length(variables_using_default_description) > 0) {
    labels_of_variables_using_default_description <- base_info[[4]][variables_using_default_description]
    default_base_description <- paste0(base_info[[1]], ":-  ", labels_of_variables_using_default_description)
    list_of_bases <- c(list_of_bases, default_base_description)
  }

  list_of_bases %>% paste0(collapse = " X ")
}

# Not called by calc_stats() itself - used downstream when formatting a
# table's base description for display.
prepare_base_for_table <- function(bases_for_table) {
  base_results <- list()
  bases <- bases_for_table %>% str_split_1(" X ") %>% unique()
  base_descriptions <- bases %>% str_split_i(":-  ", 1)
  variables <- bases %>% str_split_i(":-  ", 2)
  unique_base_descriptions <- unique(base_descriptions)

  if (length(unique_base_descriptions) == 1) {
    base_results <- append(base_results, list(unique_base_descriptions))
  } else {
    most_common_base_description <- data.frame(base_descriptions = base_descriptions) %>%
      count(base_descriptions) %>% mutate(max_n = max(n)) %>%
      filter(n == max_n) %>% pull(base_descriptions) %>% unique()
    if (length(most_common_base_description) > 1) most_common_base_description <- most_common_base_description[1]

    base_results[[1]] <- most_common_base_description %>%
      paste("All other variables:- ", ., collapse = "") %>% str_to_sentence()

    unique_base_descriptions <- unique_base_descriptions[!unique_base_descriptions %in% most_common_base_description]
    for (xxx in unique_base_descriptions) {
      index <- which(base_descriptions %in% xxx)
      variable_list <- paste0(variables[index], collapse = ", ")
      variable_description <- paste0(xxx, ":- ", variable_list)
      base_results <- append(base_results, variable_description)
    }
  }
  rev(base_results)
}


# ---- name standardisation across nested predictors ---------------------------

standardise_names <- function(data, out_var, preds, table) {
  data_to_look_in <- if (any(str_detect(class(data), "survey"))) data[["variables"]] else data

  standardised_variable_names <- c(paste0("p_cat", seq_along(preds)), "o_cat")
  renamed_variables <- table %>%
    rename(any_of(setNames(c(preds, out_var), standardised_variable_names))) %>%
    mutate(o_lab = label_or_var_name(data_to_look_in[[out_var]], out_var)) %>%
    mutate(outcome = out_var)

  map(seq_len(length(preds)), function(number) {
    standardised_predictor_names <- paste0(c("predictor", "p_lab"), number)
    p_label <- attributes(data_to_look_in[[preds[number]]])$label
    if (is.null(p_label)) p_label <- preds[number]
    renamed_variables %>%
      mutate(predictor = preds[number], p_lab = p_label) %>%
      rename(any_of(setNames(c("predictor", "p_lab"), standardised_predictor_names)))
  }) %>% reduce(full_join)
}


# ---- median SE / CI (there's no analytic equivalent for a plain median) ------

grouped_medianse <- function(data, outcome, predictors) {
  data_with_required_predictors <- data %>% select(all_of(predictors))
  predictors_as_list <- map(names(data_with_required_predictors), ~ data %>% pull(.x) %>% fct_drop())
  split_data <- split(data, predictors_as_list, drop = TRUE)
  map(seq_along(split_data), function(number) {
    outcome_data <- split_data[[number]] %>% pull(outcome)
    median_se <- wrappedtools::medianse(outcome_data)
    split_data[[number]] %>% select(all_of(predictors)) %>% unique() %>% mutate(estimate_se = median_se)
  }) %>% bind_rows()
}

grouped_medianci <- function(data, outcome, predictors) {
  data_with_required_predictors <- data %>% select(all_of(predictors))
  predictors_as_list <- map(names(data_with_required_predictors), ~ data %>% pull(.x) %>% fct_drop())
  split_data <- split(data, predictors_as_list, drop = TRUE)
  map(seq_along(split_data), function(number) {
    outcome_data <- split_data[[number]] %>% pull(outcome)
    if (length(outcome_data) > 5) {
      cil <- wrappedtools::median_cl_boot(outcome_data)
      split_data[[number]] %>% select(all_of(predictors)) %>% unique() %>%
        mutate(estimate_low = cil$CIlow, estimate_upp = cil$CIhigh)
    }
  }) %>% bind_rows()
}


# ---- multicode detection / collapsing ----------------------------------------

common_prefix <- function(x) {
  split_names <- strsplit(x, "")
  min_length <- min(lengths(split_names))
  prefix <- character(0)
  for (i in seq_len(min_length)) {
    chars <- sapply(split_names, `[`, i)
    if (length(unique(chars)) == 1) prefix <- c(prefix, chars[1]) else break
  }
  paste0(prefix, collapse = "")
}

convert_multicodes <- function(data, base_info = NULL,
                                keep = c("Mentioned", "mentioned", "Yes", "yes", "1")) {

  multis <- data %>%
    filter(stat == "perc" | stat == "count" | stat == "w_count" | stat == "w_perc") %>%
    filter(grepl(": ", o_lab)) %>%
    group_by(across(contains(c("cross_break", "p_cat"))), outcome) %>%
    mutate(lev_num = max(row_number())) %>%
    group_by(outcome) %>%
    mutate(lev_relevant = case_when(any(o_cat %in% keep) ~ 1, TRUE ~ 0)) %>%
    filter(lev_num <= 2 & lev_relevant == 1) %>%
    group_by(across(contains(c("cross_break", "p_cat"))), outcome) %>%
    mutate(keep_matches = sum(o_cat %in% keep)) %>%
    ungroup()

  # More than one level matching `keep` within one (cross_break, p_cat,
  # outcome) block means the widened default (or a caller-supplied vector)
  # is ambiguous for this variable - skip it rather than guess, which would
  # otherwise silently keep both rows and corrupt the collapsed output.
  if (any(multis$keep_matches > 1)) {
    warning("convert_multicodes(): more than one category matched `keep` for ",
            "at least one outcome - skipping ambiguous rows rather than guessing.",
            call. = FALSE)
  }

  multis <- multis %>%
    filter(keep_matches == 1) %>%
    select(-keep_matches) %>%
    mutate(left_stem = str_split_i(o_lab, ": ", 1)) %>%
    group_by(across(contains(c("cross_break", "p_cat"))), left_stem) %>%
    mutate(stem_count = n_distinct(outcome), base_count = n_distinct(base)) %>%
    filter(stem_count > 1) %>%
    filter(base_count == 1)

  if (nrow(multis) == 0) return(data)

  multicode_row_identifiers <- multis %>%
    unite(ident, contains(c("cross_break", "outcome"))) %>% pull(ident) %>% unique()

  single_codes <- data %>%
    unite(ident, contains(c("cross_break", "outcome")), remove = FALSE) %>%
    filter(!ident %in% multicode_row_identifiers) %>% select(-ident)

  multis <- if ("cross_break" %in% names(multis)) {
    multis %>% mutate(left_stem2 = paste0(left_stem, " - ", cross_break))
  } else {
    multis %>% mutate(left_stem2 = left_stem)
  }

  unique_stems <- multis$left_stem2 %>% unique()
  result <- map(unique_stems, function(stem) {
    multis2 <- multis %>% filter(left_stem2 == stem)
    old_variable_label <- multis2$o_lab[1]
    multis2 <- multis2 %>% ungroup() %>%
      mutate(right_stem = str_split_i(o_lab, ": ", 2), o_lab = left_stem) %>%
      filter(o_cat %in% keep) %>%
      mutate(o_cat = right_stem, outcome = common_prefix(outcome), outcome_type = "multicoded")

    if (is.null(base_info)) return(multis2)

    new_variable_name <- multis2$outcome %>% unique()
    new_variable_label <- multis2$o_lab %>% unique()
    names(new_variable_label) <- new_variable_name
    existing_base_description <- multis2$base_description[1]

    existing_base_description_2 <- if (str_detect(existing_base_description, " X ")) {
      str_split(existing_base_description, " X ") %>% unlist()
    } else {
      existing_base_description
    }

    index <- str_detect(existing_base_description_2, old_variable_label)
    reverse_index <- !str_detect(existing_base_description_2, old_variable_label)
    existing_base_description_3 <- existing_base_description_2[index] %>% str_split_i(":- ", 1)
    revised_base_label <- paste0(existing_base_description_3, ":- ", new_variable_label)
    names(revised_base_label) <- new_variable_name

    revised_base_description <- if (str_detect(existing_base_description, " X ")) {
      remainder_of_base_description <- existing_base_description_2[reverse_index]
      paste0(c(revised_base_label, remainder_of_base_description), collapse = " X ")
    } else {
      revised_base_label
    }
    multis2 %>% mutate(base_description = revised_base_description)
  })

  tables <- bind_rows(result)

  multis2 <- tables %>% ungroup() %>%
    select(-lev_num, -lev_relevant, -left_stem, -stem_count, -base_count, -left_stem2, -right_stem)
  bind_rows(single_codes, multis2)
}


# ---- pairwise testing: shared t-test primitive --------------------------------

safe_ttest <- function(
    formula,
    data,
    is_categorical = FALSE,
    min_n = 10,
    min_cat_n = 3
    ) {

  is_survey_data <- any(grepl("survey", class(data)))
  if (is_survey_data) {

    plain_data <- data[["variables"]]
    chosen_test <- survey::svyttest

    } else {

    plain_data <- data
    chosen_test <- t.test

  }

  resp_name <- all.vars(formula)[1]
  grp_name  <- all.vars(formula)[2]
  resp <- plain_data[[resp_name]]
  grp  <- plain_data[[grp_name]]

  group_checks <- tapply(resp, grp, function(x) {
    tibble(
      n = length(x),
      min_cat_count = if (is_categorical) min(sum(x == 0), sum(x == 1)) else NA_integer_
    )
  })

  bad_groups <- keep(group_checks, \(g) {
    g$n < min_n || (is_categorical && g$min_cat_count < min_cat_n)
  })

  if (length(bad_groups) > 0) {
    reasons <- imap_chr(bad_groups, \(g, nm) {
      why <- if (g$n < min_n) {
        paste0("n=", g$n, " (< ", min_n, ")")
      } else {
        paste0("only ", g$min_cat_count, " case(s) in the smaller outcome category (< ", min_cat_n, ")")
      }
      paste0(grp_name, "=", nm, ": ", why)
    })
    rlang::abort(paste0("safe_ttest(): comparison not testable - ",
                        paste(reasons, collapse = "; ")))
  }

  chosen_test(formula, data) |> broom::tidy()
}


# ---- pairwise testing: categorical outcomes -----------------------------------

pw_test_cats <- function(outcome, outcome_level, predictor, data,
                         level_comparisons = NULL,
                         p_adjust_method = "holm"){

  is_survey_data <- any(grepl("survey", class(data)))

  nested          <- length(predictor) > 1
  predictor_label <- paste0(predictor, collapse = "_X_")
  predictor_var   <- if (nested) predictor_label else predictor

  nest_val      <- if (nested) predictor[1] else NA_character_
  main_pred_val <- if (nested) paste0(predictor[-1], collapse = "_X_") else predictor

  if (nested) {
    data <- data |>
      mutate(across(all_of(predictor), haven::as_factor)) |>
      mutate("{predictor_var}" := fct_cross(!!!syms(predictor), sep = ":", keep_empty = FALSE))
  }

  if (is.null(level_comparisons)) {
    plain_data <- if (is_survey_data) data$variables else data
    level_comparisons <- combn(levels(plain_data[[predictor_var]]), 2, simplify = FALSE)
  }

  decorate_level <- function(level_string) {
    parts <- str_split_1(level_string, fixed(":"))
    paste(paste(predictor, parts, sep = " - "), collapse = ": ")
  }

  prop_tests <- map_dfr(level_comparisons, \(pr) {

    data |>
      filter(!!sym(predictor_var) %in% pr) |>
      mutate(grp = as.numeric(!!sym(predictor_var) == pr[2]),
             out_num = as.numeric(!!sym(outcome) == outcome_level)) %>%
      safe_ttest(out_num ~ grp, ., is_categorical = TRUE) |>
      mutate(comparison = paste(pr, collapse = " vs "), .before = 1)
  }) |>
    mutate(p_adj = p.adjust(p.value, method = p_adjust_method)) |>
    select(comparison, estimate, p.value, p_adj) |>
    mutate(
      outcome   = outcome,
      o_cat     = outcome_level,
      predictor = predictor_label,
      .before = 1
    ) |>
    mutate(
      comparison_group_1 = map_chr(str_split_i(comparison, " vs ", 1), decorate_level),
      comparison_group_2 = map_chr(str_split_i(comparison, " vs ", 2), decorate_level),
      .after = comparison
    ) |>
    mutate(nest = nest_val, predictor = main_pred_val, .after = outcome) |>
    select(-comparison)

  return(prop_tests)
}

pw_test_cats_multi <- function(outcome,
                               outcome_levels = NULL,
                               predictor,
                               data,
                               level_comparisons = NULL,
                               p_adjust_method = "holm"
                               ){

  if (is.null(outcome_levels)) {
    outcome_levels <- data %>% pull(outcome) %>% unique() %>% as.character()
  }

  map_dfr(outcome_levels, \(lvl) {
    lvl_comparisons <- level_comparisons[[lvl]]
    if (!is.null(level_comparisons) && is.null(lvl_comparisons)) {
      warning("pw_test_cats_multi(): no level_comparisons entry named '", lvl,
              "' - testing all pairwise combinations for this outcome level.")
    }

    pw_test_cats(outcome = outcome,
                 outcome_level = lvl,
                 predictor = predictor,
                 data = data,
                 level_comparisons = lvl_comparisons,
                 p_adjust_method = p_adjust_method)
  }) %>%
    mutate(p_adj = p.adjust(p.value, method = p_adjust_method))
}


# ---- pairwise testing: numeric outcomes ----------------------------------------

pw_test_means <- function(outcome, predictor, data,
                          level_comparisons = NULL){

  is_survey_data <- any(grepl("survey", class(data)))

  nested          <- length(predictor) > 1
  predictor_label <- paste0(predictor, collapse = "_X_")
  predictor_var   <- if (nested) predictor_label else predictor

  nest_val      <- if (nested) predictor[1] else NA_character_
  main_pred_val <- if (nested) paste0(predictor[-1], collapse = "_X_") else predictor

  if (nested) {
    data <- data |>
      mutate(across(all_of(predictor), haven::as_factor)) |>
      mutate("{predictor_var}" := fct_cross(!!!syms(predictor), sep = ":", keep_empty = FALSE))
  }

  if (is.null(level_comparisons)) {
    plain_data <- if (is_survey_data) data$variables else data
    level_comparisons <- combn(levels(plain_data[[predictor_var]]), 2, simplify = FALSE)
  }

  decorate_level <- function(level_string) {
    parts <- str_split_1(level_string, fixed(":"))
    paste(paste(predictor, parts, sep = " - "), collapse = ": ")
  }

  mean_tests <- map_dfr(level_comparisons, \(pr) {

    data |>
      filter(!!sym(predictor_var) %in% pr) |>
      mutate(grp = as.numeric(!!sym(predictor_var) == pr[2]),
             out_val = !!sym(outcome)) %>%
      safe_ttest(out_val ~ grp, ., is_categorical = FALSE) |>
      mutate(comparison = paste(pr, collapse = " vs "), .before = 1)
  }) |>
    mutate(p_adj = p.adjust(p.value, method = "holm")) |>
    select(comparison, estimate, p.value, p_adj) |>
    mutate(
      outcome   = outcome,
      o_cat     = "mean",
      predictor = predictor_label,
      .before = 1
    ) |>
    mutate(
      comparison_group_1 = map_chr(str_split_i(comparison, " vs ", 1), decorate_level),
      comparison_group_2 = map_chr(str_split_i(comparison, " vs ", 2), decorate_level),
      .after = comparison
    ) |>
    mutate(nest = nest_val, predictor = main_pred_val, .after = outcome) |>
    select(-comparison)

  return(mean_tests)
}


# ---- pairwise testing: comparison listing --------------------------------------

list_pw_comparisons <- function(
    predictor,
    data,
    sep = ":") {

  is_survey_data <- any(grepl("survey", class(data)))
  nested <- length(predictor) > 1

  data <- data |>
    mutate(across(all_of(predictor), haven::as_factor))

  if (nested) {
    data <- data |>
      mutate(pw_crossed = fct_cross(!!!syms(predictor), sep = sep, keep_empty = FALSE))
  }

  plain_data <- if (is_survey_data) data$variables else data
  lvls <- if (nested) levels(plain_data$pw_crossed) else levels(plain_data[[predictor]])

  decorate_level <- function(level_string) {
    parts <- str_split_1(level_string, fixed(sep))
    paste(paste(predictor, parts, sep = " - "), collapse = ": ")
  }

  combn(lvls, 2, simplify = FALSE) |>
    map_dfr(\(pr) tibble(
      group_1     = pr[1],
      group_2     = pr[2],
      group_1_lab = decorate_level(pr[1]),
      group_2_lab = decorate_level(pr[2])
    )) %>%
    mutate(id = row_number(), .before = 1)
}

# ---- pairwise testing: significance letters and merge onto calc_stats() output ----

add_letters <- function(calc_stats_df){

  letter_codes <- c(letters, as.vector(t(outer(letters, letters, paste0))))[1:260]

  cs_df_letters <-
    calc_stats_df %>% select(cross_break, contains("p_cat")) %>% unique %>%
    group_by(cross_break) %>% mutate(sig_letter =
      case_when(cross_break != "Total" ~ letter_codes[row_number()]),
      .after = cross_break)

  calc_stats_df <- full_join(calc_stats_df, cs_df_letters) %>% relocate(sig_letter, .after = cross_break)

  calc_stats_df
}

merge_p_letters <- function(one_pw_test_set, calc_stats_df, crit_lev = 0.05){

  groups <- c(one_pw_test_set$comparison_group_1, one_pw_test_set$comparison_group_2) %>% unique

  merge_prep1 <- one_pw_test_set %>%
    mutate(p_value = p.adjust(p.value, method = "BH"))

  merge_prep2 <- map(groups, function(x){
    merge_prep1 %>% filter(
      comparison_group_1 %in% x | comparison_group_2 %in% x
    ) %>%
      mutate(
        ref_group = x,
        comparison = if_else(comparison_group_1 == x, comparison_group_2, comparison_group_1),
        .after = predictor)
  }) %>% bind_rows()

  not_nested <- merge_prep2 %>% pull(nest) %>% is.na() %>% all

  if (not_nested) {

    merge_prep3 <- merge_prep2 %>%
      mutate(cross_break = predictor, .before = 1) %>%
      mutate(
        p_cat1 = str_replace_all(ref_group, paste0(predictor, " - "), ""),
        p_cat2 = "Total",
        comparison_pcat1 = str_replace_all(comparison, paste0(predictor, " - "), ""),
        comparison_pcat2 = "Total",
        group1 = p_cat1, group2 = comparison_pcat1,
        .after = comparison
      )
  } else {

    merge_prep3 <- merge_prep2 %>%
      mutate(cross_break = paste0(nest, "_X_", predictor), .before = 1) %>%
      mutate(
        p_cat1 = str_replace_all(str_split_i(ref_group, ": ", 1), paste0(nest, " - "), ""),
        p_cat2 = str_replace_all(str_split_i(ref_group, ": ", 2), paste0(predictor, " - "), ""),
        comparison_pcat1 = str_replace_all(str_split_i(comparison, ": ", 1), paste0(nest, " - "), ""),
        comparison_pcat2 = str_replace_all(str_split_i(comparison, ": ", 2), paste0(predictor, " - "), ""),
        group1 = paste0(p_cat1, "_X_", p_cat2), group2 = paste0(comparison_pcat1, "_X_", comparison_pcat2),
        .after = comparison
      )
  }

  has_p_cat2 <- "p_cat2" %in% names(calc_stats_df)

  letters_match <- calc_stats_df %>%
    filter(cross_break == merge_prep3$cross_break %>% unique) %>%
    mutate(group1 = if (has_p_cat2) {
      str_replace_all(paste0(p_cat1, "_X_", p_cat2), "_X_Total", "")
    } else {
      p_cat1
    }) %>%
    select(cross_break, outcome, o_cat, group1, sig_letter, any_of("stat"))

  letter_lookup <- letters_match$sig_letter %>% unique
  names(letter_lookup) <- letters_match$group1 %>% unique

  merge_prep4 <- full_join(merge_prep3, letters_match) %>%
    relocate(sig_letter, .after = cross_break) %>%
    mutate(comparison_letter = letter_lookup[group2])

  p_cats_in_cs_df <- calc_stats_df %>% select(contains("p_cat")) %>% names

  merge_prep4 %>%
    filter(p_value < crit_lev) %>%
    group_by(cross_break, sig_letter, outcome, o_cat, predictor, p_cat1, p_cat2, across(any_of("stat"))) %>%
    summarise(sig_diff = paste0(comparison_letter, collapse = ", "), .groups = "drop") %>%
    select(cross_break, outcome, sig_letter, any_of(p_cats_in_cs_df), o_cat, sig_diff, any_of("stat"))
}


# ---- pairwise testing: orchestration across outcomes x predictors -------------

pwtest_over_all_combinations <- function(
    data,
    outcome,
    predictor,
    statistics,
    all_outcome_levels = NULL,
    crit_lev = 0.05
    ){
  results <- list()

  for(out in outcome){

    single_outcome <- data %>% pull(out)
    if (is.factor(single_outcome))  outcome_statistic <- str_remove_all(statistics, "mean|median|sum|min|max|range|iqr|sd")
    if (is.numeric(single_outcome)) outcome_statistic <- str_remove_all(statistics, "perc|count")
    (pairwise_test_statistic <- intersect(c("w_mean", "w_perc", "mean", "perc"), outcome_statistic)[1])

    is_survey_data <- any(str_detect(class(data), "survey"))
    if (is.na(pairwise_test_statistic) ||
        (str_detect(pairwise_test_statistic, "^w_") && !is_survey_data)) next

    cur_data <- data
    if (pairwise_test_statistic %in% c("mean", "perc") && is_survey_data) {
      cur_data <- data[["variables"]]
    }

    if(is.null(all_outcome_levels)){cur_outcome_levels <- all_outcome_levels
    } else {
      cur_outcome_levels <- all_outcome_levels[[out]]
      if(length(cur_outcome_levels) == 0){cur_outcome_levels <- NULL}
    }

    for(pred in predictor){

      test_result <- NULL

      if(is.numeric(single_outcome) & grepl("mean", pairwise_test_statistic)){
        test_result <- tryCatch(
          list(pw_test_means(outcome = out, predictor = pred, data = cur_data) %>%
            mutate(stat = pairwise_test_statistic, o_cat = pairwise_test_statistic, crit_lev = crit_lev)),
          error = function(e) {
            warning("Pairwise test skipped for '", out, "' x '", paste(pred, collapse = "_X_"),
                    "': ", conditionMessage(e), call. = FALSE)
            NULL
          }
        )
      }

      if(is.factor(single_outcome) & grepl("perc", pairwise_test_statistic)){
        test_result <- tryCatch(
          list(pw_test_cats_multi(outcome = out, predictor = pred, data = cur_data,
                                   outcome_levels = cur_outcome_levels) %>%
            mutate(stat = pairwise_test_statistic, crit_lev = crit_lev)),
          error = function(e) {
            warning("Pairwise test skipped for '", out, "' x '", paste(pred, collapse = "_X_"),
                    "': ", conditionMessage(e), call. = FALSE)
            NULL
          }
        )
      }

      if (!is.null(test_result)) {
        names(test_result) <- paste0(out, " X ", paste(pred, collapse = "_X_"))
        results <- append(results, test_result)
      }
    }
  }
  results
}

add_pairwise_sig <- function(merged_tables, data, outcomes, predictors, statistics,
                              pw_outcome_levels = NULL, pw_crit_lev = 0.05) {

  merged_tables <- add_letters(merged_tables)

  if (is.null(predictors)) return(merged_tables %>% mutate(sig_diff = NA_character_))

  eligible_predictors <- predictors %>% discard(~ length(.x) > 2)
  if (length(eligible_predictors) == 0) return(merged_tables %>% mutate(sig_diff = NA_character_))

  pw_results <- pwtest_over_all_combinations(
    data = data, outcome = outcomes, predictor = eligible_predictors,
    statistics = statistics, all_outcome_levels = pw_outcome_levels, crit_lev = pw_crit_lev
  )

  if (length(pw_results) == 0) return(merged_tables %>% mutate(sig_diff = NA_character_))

  ready_to_merge <- map(pw_results, ~ merge_p_letters(.x, merged_tables, crit_lev = unique(.x$crit_lev))) %>%
    bind_rows()

  merged_tables %>%
    full_join(ready_to_merge) %>%
    mutate(sig_diff = case_when(cross_break != "Total" & is.na(sig_diff) ~ "-", TRUE ~ sig_diff))
}


