# =============================================================================
# calc_stat_engine(): the outer function for one statistic. Looks the stat up
# in stat_registry (stat_functions.R) and wraps its plain maths function.
# =============================================================================

# ---- engine -------------------------------------------------------------------

calc_stat_engine <- function(data, outcome, predictors = NULL, stat,
                             conf = NULL) {

  spec <- stat_registry[[stat]]
  if (is.null(spec)) stop("Unknown statistic: ", stat)

  # NULL first = the Total table, then one table per predictor set.
  breaks <- c(list(NULL), predictors)

  result <- breaks %>%
    map(~ one_table(data, outcome, predictor = .x, stat, spec, conf)) %>%
    bind_rows()

  # Stats without SE/CI (min, max, iqr...) get NA columns, so every stat
  # comes back the same shape and the CI step can't fail.
  for (col in c("estimate_se", "estimate_low", "estimate_upp")) {
    if (!col %in% names(result)) result[[col]] <- NA_real_
  }

  result <- result %>%
    mutate(stat = .env$stat, outcome_type = .env$spec$type) %>%
    
    # Total rows have no predictor columns; bind_rows() leaves them NA.
    # Set these rows to "Total"
    mutate(across(contains(c("cross_break", "predictor", "p_lab", "p_cat")),
                  ~ if_else(is.na(.x), "Total", as.character(.x)))) %>%
    
    # Set tibble column order.
    select(contains(c("cross_break", "predictor", "p_lab", "p_cat")),
           outcome, o_lab, o_cat, stat, outcome_type, contains("estimate"),
           base, any_of("unweighted_n"))

  # Keep only the uncertainty columns `conf` asked for.
  if (is.null(conf)) {
    result <- result %>% select(-estimate_se, -estimate_low, -estimate_upp)
  } else if (conf == "se") {
    result <- result %>% select(-estimate_low, -estimate_upp)
  } else {
    result <- result %>%
      mutate(estimate_ci = if_else(is.na(estimate_low) | is.na(estimate_upp), NA_character_,
                                   paste0(estimate_low, " - ", estimate_upp))) %>%
      select(-estimate_se)
  }

  # Numeric stats have one row per group, so unweighted_n is just the base.
  # Categorical stats set their own per-category unweighted_n.
  if (spec$type == "numeric") result <- result %>% mutate(unweighted_n = base)

  # Returns result + the rder of rows within each table 
  # Order for one outcome X predictor set X one statistic. 
  result %>%
    group_by(cross_break) %>%
    mutate(table_order = row_number()) %>%
    ungroup() %>%
    # Category order:
    # Factor level order, or 
    # Order that numeric statistics appear in stat_registry.
    mutate(cat_order = if (spec$type == "categorical") as.integer(o_cat)
                       else match(.env$stat, names(stat_registry)))
}


# One table: a single outcome, broken down by one predictor set (or none).
one_table <- function(data, outcome, predictor, stat, spec, conf) {

  categorical <- spec$type == "categorical" # TRUE if categorical variable. 
  vars_needed <- c(predictor, outcome) # One outcome + predictors pair. 

  # Drop all rows with any missing values. 
  filtered <- data %>% filter(rowSums(across(all_of(vars_needed), is.na)) == 0)

  # Categorical stats group on the outcome too. Empty levels and empty nested
  # cells are dropped for every stat (group_by()'s default .drop = TRUE).
  group_vars <- if (categorical) vars_needed else predictor
  grouped <- if (length(group_vars) > 0) {
    group_by(filtered, across(all_of(group_vars)))
  } else {
    filtered
  }

  tbl <- spec$fn(grouped, outcome, conf)

  # Bring back outcome categories nobody picked as 0 rows, within each
  # predictor group only - so empty predictor cells stay out.
  if (categorical) {
    tbl <- tbl %>%
      group_by(across(all_of(predictor))) %>%
      complete(!!sym(outcome),
               # Fill missing factor levels with these values...
               fill = list(
                 estimate = 0, 
                 estimate_se = 0, 
                 estimate_low = 0,
                 estimate_upp = 0, 
                 unweighted_n = 0)
               ) %>%
      mutate(base = max(base, na.rm = TRUE)) %>%   # new rows take their group's base
      ungroup()
  }

  # Unweighted percentages: the 0% rows just added get a Wilson interval
  # too (0 up to a small upper limit), rather than "0 - 0".
  if (categorical && stat == "perc") {
    tbl <- tbl %>%
      mutate(estimate_low = wilson_ci(unweighted_n, base)$low,
             estimate_upp = wilson_ci(unweighted_n, base)$upp)
  }

  # Numeric stats have no real category, so o_cat holds the stat name.
  if (!categorical) tbl <- tbl %>% mutate(o_cat = .env$stat)

  # Base descriptions are added later, by calc_stats() (add_base_descriptions()).
  tbl %>% standardise_names(data, outcome, predictor)
}


# Renames the outcome/predictor columns to the package's standard names
# (o_cat, p_cat1, p_cat2, ...) and adds their variable names and labels.
standardise_names <- function(tbl, data, outcome, predictor) {
  vars <- if (any(str_detect(class(data), "survey"))) data[["variables"]] else data

  # AIism. 
  # `x %||% y` means use x, unless it's NULL, then use y. 
  # So: returns the variable's label here.
  # or the variable name if it has no label.
  o_lab <- attr(vars[[outcome]], "label") %||% outcome
  
  # Another AIism. 
  # .env$ essentially means, don't look at column names in the data frame
  # when looking for this variable. Look for it in the environment, first 
  # within objects defined within this function, and so on until you reach the 
  # global environment. 
  tbl <- tbl %>%
    rename(any_of(c(o_cat = outcome))) %>%
    mutate(outcome = .env$outcome, o_lab = .env$o_lab)

  for (i in seq_along(predictor)) {
    p     <- predictor[i]
    p_lab <- attr(vars[[p]], "label") %||% p   # label, or name if none (as above)
    tbl <- tbl %>%
      rename(!!paste0("p_cat", i) := all_of(p)) %>%
      mutate(!!paste0("predictor", i) := .env$p,
             !!paste0("p_lab", i)     := .env$p_lab)
  }

  # Always present: "Total" when there's no predictor, so downstream code
  # never has to check whether the column exists.
  cross_break <- if (length(predictor) > 0) paste0(predictor, collapse = "_X_") else "Total"
  tbl %>% mutate(cross_break = .env$cross_break)
}
