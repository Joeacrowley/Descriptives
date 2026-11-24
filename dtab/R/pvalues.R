# =============================================================================
# P-values for calc_stats().
#
# return_pvalues() is the outer function and calls a plain test function -
# test_categorical() or test_numeric(). Each test works for weighted and
# unweighted data, checks the data first, and returns one row: p_method and
# p_value.
#
#   - Numeric outcomes compare means (Welch's ANOVA, or a Wald test when
#     weighted); Kruskal-Wallis only if asked for.
#   - Groups smaller than min_n aren't tested; p_method says why.
#   - Which stats a p-value applies to comes from stat_registry.
#   - map_return_p_values() handles flat and nested predictor sets, and every
#     p-value comes back in one shape, so calc_stats() needs one merge.
# =============================================================================

# ---- shared steps ------------------------------------------------------------

# Complete cases only, empty predictor groups dropped. Works on survey
# designs too (filter/mutate keep the design).
prepare_test_data <- function(data, outcome, predictor) {
  data %>%
    filter(if_all(all_of(c(outcome, predictor)), ~ !is.na(.x))) %>%
    mutate(across(all_of(predictor), fct_drop))
}

# NULL if the predictor groups are fine to test, otherwise the reason not to.
group_size_problem <- function(data, predictor, min_n) {
  plain_data <- if (any(str_detect(class(data), "survey"))) data[["variables"]] else data
  group_n    <- table(plain_data[[predictor]])

  if (length(group_n) < 2) return("fewer than 2 groups")

  small <- group_n[group_n < min_n]
  if (length(small) > 0) {
    return(paste0(predictor, " = ", names(small), ": n = ", small, " (< ", min_n, ")",
                  collapse = "; "))
  }
  NULL
}

# The row every test returns when it can't run.
not_tested <- function(reason) {
  tibble(p_method = paste("Not tested:", reason), p_value = NA_real_)
}


# ---- categorical outcome -----------------------------------------------------
# Chi-square; the design-adjusted version (svychisq) when weighted.

test_categorical <- function(data, outcome, predictor, min_n = 10) {
  weighted <- any(str_detect(class(data), "survey"))
  filtered <- prepare_test_data(data, outcome, predictor) %>%
    mutate(across(all_of(outcome), fct_drop))   # empty outcome categories too

  # Check first.
  problem <- group_size_problem(filtered, predictor, min_n)
  if (!is.null(problem)) return(not_tested(problem))

  plain_data <- if (weighted) filtered[["variables"]] else filtered
  if (n_distinct(plain_data[[outcome]]) < 2) return(not_tested("outcome has only one category"))

  # Then test.
  tryCatch({
    if (weighted) {
      frmla <- as.formula(paste0("~", outcome, " + ", predictor))
      # Rao-Scott F: survey's default, and steadier than the Wald tests
      # (which give too-small p-values) when cells are small.
      test  <- survey::svychisq(frmla, design = filtered, statistic = "F")
      tibble(p_method = "Rao-Scott Chi-Square test", p_value = as.numeric(test$p.value))
    } else {
      # chisq.test() warns about small expected counts; say so in p_method instead.
      test   <- suppressWarnings(chisq.test(plain_data[[predictor]], plain_data[[outcome]]))
      method <- if (any(test$expected < 5)) "Chi-Square test (some expected counts < 5)" else "Chi-Square test"
      tibble(p_method = method, p_value = test$p.value)
    }
  }, error = function(e) not_tested(conditionMessage(e)))
}


# ---- numeric outcome ---------------------------------------------------------
# numeric_test = "means" (default): do the group means differ?
#   Welch's ANOVA, or a Wald test from svyglm() when weighted.
# numeric_test = "ranks": does one group tend to score higher?
#   Kruskal-Wallis, or its design-based version (svyranktest) when weighted.

test_numeric <- function(data, outcome, predictor, numeric_test = "means", min_n = 10) {
  numeric_test <- match.arg(numeric_test, c("means", "ranks"))
  weighted <- any(str_detect(class(data), "survey"))
  filtered <- prepare_test_data(data, outcome, predictor) %>%
    mutate(across(all_of(outcome), haven::zap_labels))   # tests want plain numbers

  # Check first.
  problem <- group_size_problem(filtered, predictor, min_n)
  if (!is.null(problem)) return(not_tested(problem))

  # Then test.
  frmla <- as.formula(paste(outcome, "~", predictor))
  tryCatch({
    if (weighted && numeric_test == "means") {
      model <- survey::svyglm(frmla, design = filtered)
      tibble(p_method = "Wald Test",
             p_value  = as.numeric(survey::regTermTest(model, predictor)$p))
    } else if (weighted && numeric_test == "ranks") {
      tibble(p_method = "Kruskal-Wallis",
             p_value  = as.numeric(survey::svyranktest(frmla, design = filtered)$p.value))
    } else if (numeric_test == "means") {
      tibble(p_method = "Welch's ANOVA",
             p_value  = oneway.test(frmla, data = filtered, var.equal = FALSE)$p.value)
    } else {
      tibble(p_method = "Kruskal-Wallis",
             p_value  = kruskal.test(frmla, data = filtered)$p.value)
    }
  }, error = function(e) not_tested(conditionMessage(e)))
}


# ---- one outcome x one predictor ---------------------------------------------
# Picks the test by outcome type, then copies the result onto every stat it
# applies to - the stats in stat_registry with the same type and weighting.

return_pvalues <- function(data, outcome, predictor, numeric_test = "means") {
  weighted   <- any(str_detect(class(data), "survey"))
  plain_data <- if (weighted) data[["variables"]] else data
  type       <- if (is.factor(plain_data[[outcome]])) "categorical" else "numeric"

  result <- if (type == "categorical") {
    test_categorical(data, outcome, predictor)
  } else {
    test_numeric(data, outcome, predictor, numeric_test = numeric_test)
  }

  stats <- stat_registry %>%
    keep(~ .x$type == type && .x$weighted == weighted) %>%
    names()

  tibble(outcome = outcome, cross_break = predictor, stat = stats,
         p_method = result$p_method, p_value = result$p_value)
}


# ---- nested predictor set ----------------------------------------------------
# One p-value per outer level ("among women, is age associated with this
# outcome?"), not one test across every cell. Runs return_pvalues() on the
# subset of data for each outer level.

nested_pvalues <- function(data, outcomes, outer_var, inner_var, numeric_test = "means") {
  plain_data   <- if (any(str_detect(class(data), "survey"))) data[["variables"]] else data
  outer_levels <- plain_data %>% pull(outer_var) %>% unique() %>% na.omit() %>% as.character()

  map_df(outer_levels, function(lvl) {
    subset_data <- data %>% filter(.data[[outer_var]] == lvl)
    map_df(outcomes, function(out) {
      return_pvalues(data = subset_data, outcome = out, predictor = inner_var,
                     numeric_test = numeric_test) %>%
        mutate(cross_break = paste0(outer_var, "_X_", inner_var), p_cat1 = lvl)
    })
  })
}


# ---- every outcome x predictor set -------------------------------------------
# predictors: the list of predictor sets, as calc_stats() has it.
# Returns one row per outcome x cross_break x p_cat1 x stat, so calc_stats()
# can merge every p-value on the same four columns.

map_return_p_values <- function(outcomes, predictors, data, numeric_test = "means") {
  plain_data <- if (any(str_detect(class(data), "survey"))) data[["variables"]] else data

  # Sets of 3+ variables have no test yet.
  # `keep` from `purrr` filters a vector or list to elements that pass a criteria. 
  too_deep <- predictors %>% keep(~ length(.x) > 2)
  if (length(too_deep) > 0) {
    warning("No p-values for predictor sets of more than 2 variables: ",
            paste(map_chr(too_deep, ~ paste(.x, collapse = "_X_")), collapse = ", "), ".")
  }

  p_values <- map_df(predictors, function(pred_set) {

    # Flat set: one p-value for the whole table, copied to every predictor
    # level so it matches on p_cat1 like the nested ones. fct_drop(): only
    # levels someone actually has, so no rows for empty groups.
    if (length(pred_set) == 1) {
      flat_p <- map_df(outcomes, ~ return_pvalues(data = data, outcome = .x, predictor = pred_set,
                                                  numeric_test = numeric_test))
      return(flat_p %>% crossing(p_cat1 = levels(fct_drop(plain_data[[pred_set]]))))
    }

    # Nested set: one p-value per outer level, p_cat1 already set.
    if (length(pred_set) == 2) {
      return(nested_pvalues(data, outcomes, outer_var = pred_set[1], inner_var = pred_set[2],
                            numeric_test = numeric_test))
    }

    NULL # 3+ variables: skipped (warned above)
  })

  # Nothing tested (e.g. every set was 3+ variables): an empty tibble with
  # the right columns, so the merge in calc_stats() still works.
  if (nrow(p_values) == 0) {
    return(tibble(outcome = character(), cross_break = character(), p_cat1 = character(),
                  stat = character(), p_method = character(), p_value = numeric()))
  }

  p_values %>% select(outcome, cross_break, p_cat1, stat, p_method, p_value)
}
