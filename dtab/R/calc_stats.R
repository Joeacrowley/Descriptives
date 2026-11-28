# =============================================================================
# calc_stats(): validates the inputs, then for each outcome runs every
# requested stat that fits it (via stat_registry), stacks the results, and
# adds p-values, pairwise letters, base descriptions and multicode handling.
#
# The pieces live in their own files: calc_stat_engine.R, stat_functions.R,
# pvalues.R, pairwise.R, bases.R, multicodes.R.
# =============================================================================

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
#' @param outcomes Character vector of outcome variable names. Can be `NULL`
#'   if `multicode` is given.
#' @param predictors `NULL` for no breakdown (Total only), a character
#'   vector/single variable name, a list of variable names for multiple
#'   side-by-side flat predictor sets, or `list(c(<outer>, <inner>))` for
#'   a nested predictor set. Predictors must be factors, characters,
#'   logicals or labelled variables (the last three become factors).
#'   Cases missing a predictor are left out of that predictor's breakdown
#'   but still count in Total, so its groups' bases can add up to less
#'   than Total's.
#' @param statistics Character vector of statistic codes to compute, e.g.
#'   `"perc"`, `"count"`, `"mean"`, `"median"`, `"sum"`, `"min"`, `"max"`,
#'   `"range"`, `"iqr"`, `"sd"`, and their weighted `w_`-prefixed
#'   equivalents (weighted versions require a survey design as `data`).
#'   Each is only run on outcomes it fits (perc/count on factors, the rest
#'   on numerics).
#' @param conf `NULL`, `"se"`, or `"ci"` - adds a standard error or
#'   confidence interval alongside each estimate.
#' @param base Optional base description for the footnote: a single string
#'   used for every variable (e.g. `"All respondents"`), or an object from
#'   `base_information()` for variable-specific bases.
#' @param pval If `TRUE`, adds p-values testing association between each
#'   outcome and predictor: chi-square for categorical outcomes (Rao-Scott
#'   when weighted), and for numeric outcomes the test `numeric_test` sets.
#'   One p-value per flat predictor set, and one per outer level of a nested
#'   set. Groups with fewer than 10 cases aren't tested. Default `FALSE`.
#' @param numeric_test For numeric outcomes when `pval = TRUE`: `"means"`
#'   (default) compares group means - Welch's ANOVA, or a Wald test when
#'   weighted. `"ranks"` uses Kruskal-Wallis (design-based when weighted).
#' @param multicode `NULL` (default), or a list of multicoded
#'   (select-all-that-apply) questions, each a character vector of its
#'   yes/no option variables, e.g. `list(c("q1a", "q1b"), c("q2a", "q2b", "q2c"))`.
#'   Each question is shown as one table with a row per option. Options need
#'   labels like "<question>: <option>", and must not also be predictors.
#'   They're added to `outcomes` if not already there - list them in
#'   `outcomes` too to control where the question appears.
#' @param pairwise If `TRUE`, adds `sig_letter`/`sig_diff`/`sig_untested`/
#'   `sig_method` columns from pairwise significance testing between
#'   predictor levels. Pairs with too few cases to test (fewer than 10 in a
#'   group, or fewer than 3 in or out of the category) are skipped and
#'   listed in `sig_untested`; the rest are still tested.
#' @param pw_outcome_levels Optional named list restricting which outcome
#'   categories get pairwise-tested - only used when `pairwise = TRUE`.
#' @param pw_crit_lev Significance threshold for `sig_diff` flagging when
#'   `pairwise = TRUE`. Default `0.05`.
#' @param pw_adjust How pairwise p-values are adjusted for multiple
#'   comparisons when `pairwise = TRUE` - any method `p.adjust()` takes
#'   (`"BH"`, `"holm"`, `"bonferroni"`, `"none"`, ...). Adjusted across all
#'   comparisons for an outcome and predictor together. Default `"BH"`.
#' @param drop_empty_levels If `TRUE`, categories of a factor outcome that
#'   nobody in `data` picked are left out of the results. Default `FALSE`:
#'   they're kept as 0 rows, since a 0 can be a real finding.
#'
#' @return A long-format tibble with one row per (outcome, category,
#'   predictor level, statistic): `cross_break` (`"Total"` when there's no
#'   breakdown), `predictorN`/`p_labN`/`p_catN` per predictor in the set,
#'   `outcome`/`o_lab`/`o_cat`, `stat`, `outcome_type`, `estimate` (plus
#'   `estimate_se`, or `estimate_low`/`estimate_upp`/`estimate_ci`, if
#'   `conf` was requested), `base`, `unweighted_n`, `o_base`/`p_baseN` (each
#'   variable's base description, when `base` is given),
#'   `p_method`/`p_value` (`p_method` says "Not tested: ..." and why, when
#'   a test couldn't run), `table_order`/`cat_order`/`row_order` for
#'   sorting, and, when `pairwise = TRUE`: `sig_letter` (each predictor
#'   level's letter), `sig_diff` (letters of levels it differs from
#'   significantly, `"-"` if none), `sig_untested` (letters of levels it
#'   couldn't be tested against) and `sig_method` (the test used).
#'
#' @seealso [pivot_summary()], [pivot_crosstab()],
#'   [pivot_nested_crosstab()], [pivot_battery()], [make_table()]
#' @export
calc_stats <- function(
    data,
    outcomes = NULL,
    predictors = NULL,
    statistics = c("count", "mean"),
    conf = NULL,
    base = NULL,
    pval = FALSE,
    numeric_test = "means",
    multicode = NULL,
    pairwise = FALSE,
    pw_outcome_levels = NULL,
    pw_crit_lev = 0.05,
    drop_empty_levels = FALSE,
    pw_adjust = "BH"
) {

  # ---- validate inputs ----
  # Note, validation helper functions are at the end of this script.
  # Note, no validation on `pw_outcome_levels`.
  # Validation on `predictors` could be improved. 
  
  # Data must be a data frame or a survey design.
  if (!is.data.frame(data) && !any(str_detect(class(data), "survey"))) {
    stop("data must be a data frame or a survey design object.")
  }
  
  # Designs made with the survey package work once turned into srvyr designs.
  if (inherits(data, "survey.design") && !inherits(data, "tbl_svy")) {
    data <- srvyr::as_survey(data)
  }
  
  # Any grouping from group_by() would be added to every calculation, so drop it.
  data <- ungroup(data)

  # Outcomes: NULL, or a character vector of variable names.
  if (!is.null(outcomes) && (!is.character(outcomes) || length(outcomes) == 0)) {
    stop("outcomes must be a character vector of variable names.")
  }

  # multicode: NULL, or a list of character vectors - one per question,
  # each with 2+ options, and no option in two questions.
  if (!is.null(multicode)) {
    if (!is.list(multicode) || is.data.frame(multicode) || length(multicode) == 0) {
      stop("multicode must be a list of character vectors, e.g. list(c(\"q1a\", \"q1b\")).")
    }
    for (question in multicode) {
      if (!is.character(question) || length(question) < 2) {
        stop("Each multicode question must be a character vector of 2 or more variable names.")
      }
    }
    repeated <- unique(unlist(multicode)[duplicated(unlist(multicode))])
    if (length(repeated) > 0) {
      stop("These appear more than once in multicode: ", paste(repeated, collapse = ", "), ".")
    }
  }

  # Need something to work out.
  if (is.null(outcomes) && is.null(multicode)) {
    stop("Give outcomes, multicode, or both.")
  }

  # Multicode options are outcomes too. Ones already in outcomes keep their place.
  outcomes <- unique(c(outcomes, unlist(multicode)))

  # conf: NULL, "se" or "ci". match.arg() gives an error listing the options.
  if (!is.null(conf)) conf <- match.arg(conf, c("se", "ci"))

  # numeric_test: "means" or "ranks".
  numeric_test <- match.arg(numeric_test, c("means", "ranks"))

  # Switches must be TRUE or FALSE.
  switches <- list(
    pval = pval,
    pairwise = pairwise,
    drop_empty_levels = drop_empty_levels
    )
  for (arg in names(switches)) {
    if (!isTRUE(switches[[arg]]) && !isFALSE(switches[[arg]])) {
      stop(arg, " must be TRUE or FALSE.")
    }
  }

  # pw_crit_lev: a single number between 0 and 1.
  if (!is.numeric(pw_crit_lev) || length(pw_crit_lev) != 1 ||
      pw_crit_lev <= 0 || pw_crit_lev >= 1) {
    stop("pw_crit_lev must be a single number between 0 and 1.")
  }

  # pw_adjust: one of p.adjust()'s methods, e.g. "BH", "holm", "none".
  pw_adjust <- match.arg(pw_adjust, p.adjust.methods)

  # base: NULL, a single string, or made with base_information(). Checked
  # here so a mistake fails now, not after every table has been calculated.
  if (!is.null(base) &&
      !(is.character(base) && length(base) == 1 && !is.na(base)) &&
      !(is.list(base) && "specific_bases" %in% names(base))) {
    stop("base must be a single character string or made with base_information().")
  }
  
  # statistics: a character vector of at least one statistic name.
  if (!is.character(statistics) || length(statistics) == 0) {
    stop("statistics must be a character vector of statistic names, e.g. c(\"perc\", \"mean\"). ",
         "Available: ", paste(names(stat_registry), collapse = ", "), ".")
  }

  # Stop if all requested statistics are unknown (not in stats_registry)
  unknown_stats <- setdiff(statistics, names(stat_registry))
  if (length(unknown_stats) == length(unique(statistics))) {
    stop("None of the requested statistics are recognised: ",
         paste(unknown_stats, collapse = ", "),
         ". Available stats include: ", paste(names(stat_registry), collapse = ", "), ".")
  }
  
  # Warning if more than one statistic requested is not in stats_registry
  if (length(unknown_stats) > 0) {
    warning("Ignoring unrecognised statistics: ", paste(unknown_stats, collapse = ", "),
            ". Available: ", paste(names(stat_registry), collapse = ", "), ".")
  }
  
  # Keep known requested statistics. 
  statistics <- intersect(statistics, names(stat_registry))

  # Predictors should be a list. 
  # If predictors are a plain character vector, put in a list.
  if (is.character(predictors)) predictors <- list(predictors)

  # By now predictors should be NULL or a list (and a data frame counts as a list in R).
  if (!is.null(predictors) && (!is.list(predictors) || is.data.frame(predictors))) {
    stop("predictors must be a character vector or a list of character vectors.")
  }
  
  # The same predictor set twice would give every one of its rows twice.
  if (any(duplicated(predictors))) {
    repeated <- predictors[duplicated(predictors)] %>% map_chr(~ paste(.x, collapse = " x "))
    stop("These predictor sets are given more than once: ",
         paste(repeated, collapse = ", "), ".")
  }

  # Check outcome/predictor variables in the data. 
  if (!isTRUE(vars_exist(data = data, variable_list = c(outcomes, unlist(predictors))))) {
    stop("Some variables you want to use do not exist in the data frame.")
  }
  
  # Variable names can't match calc_stats()'s own output columns, or the
  # two get mixed up (e.g. a predictor called "estimate" gives wrong rows).
  reserved <- c("cross_break", "outcome", "multicode_group", "o_lab", "o_cat",
                "o_base", "stat", "outcome_type", "estimate", "estimate_se",
                "estimate_low", "estimate_upp", "estimate_ci", "base",
                "unweighted_n", "table_order", "cat_order", "row_order",
                "p_method", "p_value", "sig_letter", "sig_diff",
                "sig_untested", "sig_method")
  
  clashes <- intersect(c(outcomes, unlist(predictors)), reserved)
  if (length(clashes) > 0) {
    stop("These variable names are used by calc_stats()'s own output - rename them first: ",
         paste(clashes, collapse = ", "), ".")
  }

  # "Total" fills the predictor columns of the Total table, so a predictor
  # can't be called that: its rows would look like the Total table's.
  if ("Total" %in% unlist(predictors)) {
    stop("A predictor can't be called \"Total\" - please rename it.")
  }

  if (!is.null(predictors)) {
    if (!isTRUE(outcomes_not_in_predictors(outcomes = outcomes, predictors = predictors))) {
      stop("Outcome appears in predictor list.")
    }
    
    if (list_depth(predictors) != 1) {
      stop("Predictor list should not contained further lists.")
    }
    if (any(lengths(predictors) == 0)) {
      stop("Predictor list contains an empty predictor set.")
    }
  }

  # ---- outcome types ----
  # Characters and logicals become factors. Anything else that isn't
  # ---- outcome and predictor types ----
  # Labelled (haven), character and logical outcomes and predictors become
  # factors. Outcomes that are anything else not numeric or a factor (e.g.
  # dates) stop below; so do predictors that still aren't factors.
  df_to_check <- if (any(str_detect(class(data), "survey"))) data[["variables"]] else data
  
  to_convert <- c(outcomes, unlist(predictors))
  
  # SPSS/Stata-style labelled variables (haven_labelled) become factors,
  # using their value labels. Otherwise they'd count as numeric, and
  # perc/count would skip them.
  data <- data %>%
    mutate(across(all_of(to_convert) & where(haven::is.labelled), haven::as_factor))
  
  # factor() orders categories alphabetically; structure() puts the
  # variable label back, as factor() drops it. Works on survey designs too.
  data <- data %>%
    mutate(across(all_of(to_convert) & where(~ is.character(.x) || is.logical(.x)),
                  ~structure(factor(.x), label = attr(.x, "label"))))
  
  # Ordered factors become plain ones (same levels, same order, label kept).
  # Stacking an ordered factor with any other factor fails, and nothing
  # here needs them ordered - category order is kept in cat_order.
  data <- data %>%
    mutate(across(all_of(to_convert) & where(is.ordered),
                  ~structure(factor(.x, levels = levels(.x), ordered = FALSE),
                             label = attr(.x, "label"))))
  
  
  bad_outcomes <- df_to_check %>%
    select(all_of(outcomes)) %>%
    select(where(~ !is.numeric(.x) && !is.factor(.x) && !is.character(.x) && !is.logical(.x))) %>%
    names()
  
  if (length(bad_outcomes) > 0) {
    stop("Outcomes must be numeric, factor, character or logical. Not usable: ",
         paste(bad_outcomes, collapse = ", "), ".")
  }
  
  # Predictors must be factors by now - numbers can't be broken down by.
  if (!is.null(predictors) && !isTRUE(check_all_factors(data = data, variable_list = predictors))) {
    stop("Predictors must be factors, characters, logicals or labelled variables. ",
         "Group numeric predictors into categories first.")
  }
  
  # No rows: nothing to calculate.
  if (nrow(df_to_check) == 0) {
    stop("data has no rows.")
  }
  
  # Outcomes where every value is missing can't be tabled.
  empty_outcomes <- df_to_check %>%
    select(all_of(outcomes)) %>%
    select(where(~ all(is.na(.x)))) %>%
    names()
  if (length(empty_outcomes) > 0) {
    stop("These outcomes have no answers (every value is missing): ",
         paste(empty_outcomes, collapse = ", "), ". Remove them from outcomes or multicode.")
  }

  # ---- unweighted and weighted copies of the data ----

  is_survey       <- any(str_detect(class(data), "survey"))
  unweighted_data <- if (is_survey) data[["variables"]] else data
  weighted_data   <- if (is_survey) data else NULL

  # Drop empty factor levels on outcome factors. 
  if (isTRUE(drop_empty_levels)) {
    factor_outcomes <- unweighted_data %>% 
      select(all_of(outcomes)) %>% 
      select(where(is.factor)) %>% 
      names()    
    unweighted_data <- unweighted_data %>% mutate(across(all_of(factor_outcomes), fct_drop))
    if (is_survey) {
      weighted_data <- weighted_data %>% mutate(across(all_of(factor_outcomes), fct_drop))
    }
  }
  
  # Weighted statistics need a survey design.
  weighted_stats <- statistics[map_lgl(stat_registry[statistics], "weighted")]
  if (!is_survey && length(weighted_stats) > 0) {
    stop("Weighted statistics need a survey design as data: ",
         paste(weighted_stats, collapse = ", "), ".")
  }
  
  stat_types <- map_chr(stat_registry[statistics], "type")
  is_factor  <- map_lgl(unweighted_data[outcomes], is.factor)
  
  # Factor outcomes need perc or count; numeric outcomes need e.g. mean.
  if (any(is_factor) && !any(stat_types == "categorical")) {
    stop("These outcomes are categorical, but no perc/count statistic was requested: ",
         paste(outcomes[is_factor], collapse = ", "), ".")
  }
  if (any(!is_factor) && !any(stat_types == "numeric")) {
    stop("These outcomes are numeric, but no numeric statistic (e.g. mean) was requested: ",
         paste(outcomes[!is_factor], collapse = ", "), ".")
  }
  
  # Multicode options must be categorical (yes/no) - numbers can't be folded.
  not_categorical <- unlist(multicode)[!map_lgl(unweighted_data[unlist(multicode)], is.factor)]
  if (length(not_categorical) > 0) {
    stop("Multicode options must be yes/no (factor, character, logical or labelled) variables. ",
         "Not usable: ", paste(not_categorical, collapse = ", "), ".")
  }

  # ---- Estimates for each outcome in turn ----
  merged_tables <- map(outcomes, function(cur_outcome) {
    outcome_type <- if (is.factor(unweighted_data[[cur_outcome]])) "categorical" else "numeric"
    
    # For one outcome, calculate estimates for each statistic requested
    map(statistics, function(stat) {
      spec <- stat_registry[[stat]]
      if (spec$type != outcome_type) return(NULL) # e.g. mean on a factor
      if (spec$weighted && !is_survey) return(NULL) # weighted stat, no weights
      calc_stat_engine(
        data = if (spec$weighted) weighted_data else unweighted_data,
        outcome = cur_outcome, predictors = predictors, 
        stat = stat, conf = conf)
    }) %>% bind_rows() # bind the different statistics together, NULLs dropped
    
  }) %>% bind_rows() # bind the outcomes together.

  # ---- p-values ----
  # Tests are in pvalues.R. Total rows have no predictor, so stay NA.

  merged_tables <- merged_tables %>% mutate(p_method = NA_character_, p_value = NA_real_)

  if (isTRUE(pval) && !is.null(predictors)) {
    # Test the unweighted data, the survey design, or both - whichever the
    # requested stats use.
    any_unweighted <- any(!map_lgl(stat_registry[statistics], "weighted"))
    any_weighted   <- is_survey && any(map_lgl(stat_registry[statistics], "weighted"))
    test_data <- c(if (any_unweighted) list(unweighted_data),
                   if (any_weighted)   list(weighted_data))

    p_values <- map_df(test_data, ~ map_return_p_values(outcomes, predictors, data = .x,
                                                        numeric_test = numeric_test))

    # Flat and nested p-values share one shape, so one merge. unmatched =
    # "ignore": skip p-values for groups with no row in the results.
    merged_tables <- merged_tables %>%
      rows_update(p_values, by = c("outcome", "cross_break", "p_cat1", "stat"),
                  unmatched = "ignore")
  }

  # ---- pairwise letters ----
  # pairwise.R.

  if (isTRUE(pairwise)) {
    merged_tables <- suppressMessages(suppressWarnings(
      add_pairwise_sig(merged_tables, data = if (is_survey) weighted_data else unweighted_data,
                       outcomes = outcomes, predictors = predictors, statistics = statistics,
                       pw_outcome_levels = pw_outcome_levels, pw_crit_lev = pw_crit_lev, 
                       pw_adjust = pw_adjust)
    ))
  }

  # ---- base descriptions ----
  # o_base, p_base1... alongside o_lab, p_lab1... (bases.R).
  merged_tables <- add_base_descriptions(merged_tables, base_info = base)

  # ---- multicodes ----
  # multicodes.R. Its warning (unequal bases) is
  # worth seeing, so not suppressed.
  if (!is.null(multicode)) {
    merged_tables <- convert_multicodes(data = merged_tables, multicode = multicode)
  } else {
    # Same column either way: every outcome is its own group.
    merged_tables <- merged_tables %>% mutate(multicode_group = outcome, .after = outcome)
  }

  # Row order of the finished result, so it can always be put back.
  merged_tables %>% mutate(row_order = row_number())
}


# calc_stats helpers -----------------------------------------------------------

# pluck_depth treats a character string as depth 1, 
# so a list with 1 character string in it is depth 2.
# I want a depth of 1 for that, hence the minus 1. 
# Not sure what would happen if you say, added a data frame in. 
# So don't put data frames in the predictors arguments. 
list_depth <- function(x) pluck_depth(x) - 1


# Checking functions used in calc_stats

# Do variables exist? 
vars_exist <- function(variable_list, data) {
  df_to_check <- if (any(grepl("survey", class(data)))) data[["variables"]] else data
  variables_to_check <- variable_list %>% unlist() %>% unique()
  variables_in_df <- names(df_to_check)
  variables_in_both <- intersect(variables_to_check, variables_in_df)
  length(variables_to_check) == length(variables_in_both)
}

check_all_factors <- function(data, variable_list) {
  df_to_check <- if (any(grepl("survey", class(data)))) data[["variables"]] else data
  df_to_check %>% 
    select(all_of(unlist(variable_list))) %>%
    map_lgl(is.factor) %>%
    all()
}

# Used by pivot_numeric_summary(): are all the variables numeric?
check_all_numeric <- function(data, variable_list) {
  df_to_check <- if (any(grepl("survey", class(data)))) data[["variables"]] else data
  df_to_check %>% 
    select(all_of(unlist(variable_list))) %>%
    map_lgl(is.numeric) %>%
    all()
}

outcomes_not_in_predictors <- function(outcomes, predictors) {
  predictor_vector <- unlist(predictors) %>% unique()
  overlap_of_predictors_and_outcomes <- intersect(outcomes, predictor_vector)
  length(overlap_of_predictors_and_outcomes) == 0
}
