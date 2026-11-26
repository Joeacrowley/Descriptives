# =============================================================================
# Pairwise significance testing for calc_stats(pairwise = TRUE).
#
#   - empty predictor levels are dropped before pairing (fct_drop)
#   - an untestable pair is kept with an NA p-value and the reason in
#     `method`, rather than skipping the whole outcome x predictor
#   - missing answers are left out of the size checks
#   - the p-value adjustment method is an argument (pw_adjust in calc_stats)
# =============================================================================

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

  # Leave out people with no answer - the test leaves them out anyway.
  answered <- !is.na(resp)
  resp <- resp[answered]
  grp  <- grp[answered]

  # Size checks for each group. split() keeps a group even if no one in it
  # answered, so it gets n = 0 and fails the check.
  group_checks <- map(split(resp, factor(grp, levels = c(0, 1))), function(x) {
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
                        paste(reasons, collapse = "; ")),
                 class = "untestable_comparison")
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
      mutate("{predictor_var}" := fct_cross(!!!syms(predictor), sep = "__:__", keep_empty = FALSE))
  }

  if (is.null(level_comparisons)) {
    plain_data <- if (is_survey_data) data$variables else data
    level_comparisons <- combn(levels(fct_drop(plain_data[[predictor_var]])), 2, simplify = FALSE)  
    }

  decorate_level <- function(level_string) {
    parts <- str_split_1(level_string, fixed("__:__"))
    paste(paste(predictor, parts, sep = "___"), collapse = "__:__")
  }

  prop_tests <- map_dfr(level_comparisons, \(pr) {

    # This pair's data: grp is 0 for the first level, 1 for the second;
    # out_num is 1 if they gave outcome_level, 0 if not.
    pair_data <- data |>
      filter(!!sym(predictor_var) %in% pr) |>
      mutate(grp = as.numeric(!!sym(predictor_var) == pr[2]),
             out_num = as.numeric(!!sym(outcome) == outcome_level))

    # Test the pair. If safe_ttest() finds it too small to test, keep a
    # row anyway: no estimate or p-value, and the reason in `method`.
    pair_test <- tryCatch(
      safe_ttest(out_num ~ grp, pair_data, is_categorical = TRUE),
      untestable_comparison = function(e) {
        tibble(estimate = NA_real_, p.value = NA_real_, method = conditionMessage(e))
      }
    )

    pair_test |>
      mutate(comparison = paste(pr, collapse = "__vs__"), .before = 1)
  }) |>
    mutate(p_adj = p.adjust(p.value, method = p_adjust_method)) |>
    select(comparison, estimate, p.value, p_adj, method) |>
    mutate(
      outcome   = outcome,
      o_cat     = outcome_level,
      predictor = predictor_label,
      .before = 1
    ) |>
    mutate(
      comparison_group_1 = map_chr(str_split_i(comparison, fixed("__vs__"), 1), decorate_level),
      comparison_group_2 = map_chr(str_split_i(comparison, fixed("__vs__"), 2), decorate_level),
      .after = comparison
    ) |>
    mutate(
      g1_p_cat1 = str_split_i(str_split_i(comparison, fixed("__vs__"), 1), fixed("__:__"), 1),
      g1_p_cat2 = str_split_i(str_split_i(comparison, fixed("__vs__"), 1), fixed("__:__"), 2),
      g2_p_cat1 = str_split_i(str_split_i(comparison, fixed("__vs__"), 2), fixed("__:__"), 1),
      g2_p_cat2 = str_split_i(str_split_i(comparison, fixed("__vs__"), 2), fixed("__:__"), 2),
      .after = comparison_group_2
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
    # Missing answers aren't an outcome category to test.
    outcome_levels <- outcome_levels[!is.na(outcome_levels)]
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
      mutate("{predictor_var}" := fct_cross(!!!syms(predictor), sep = "__:__", keep_empty = FALSE))
  }

  if (is.null(level_comparisons)) {
    plain_data <- if (is_survey_data) data$variables else data
    level_comparisons <- combn(levels(fct_drop(plain_data[[predictor_var]])), 2, simplify = FALSE)
    }

  decorate_level <- function(level_string) {
    parts <- str_split_1(level_string, fixed("__:__"))
    paste(paste(predictor, parts, sep = "___"), collapse = "__:__")
  }

  mean_tests <- map_dfr(level_comparisons, \(pr) {

    # This pair's data: grp is 0 for the first level, 1 for the second;
    # out_val is the outcome's value.
    pair_data <- data |>
      filter(!!sym(predictor_var) %in% pr) |>
      mutate(grp = as.numeric(!!sym(predictor_var) == pr[2]),
             out_val = !!sym(outcome))

    # Test the pair. If safe_ttest() finds it too small to test, keep a
    # row anyway: no estimate or p-value, and the reason in `method`.
    pair_test <- tryCatch(
      safe_ttest(out_val ~ grp, pair_data, is_categorical = FALSE),
      untestable_comparison = function(e) {
        tibble(estimate = NA_real_, p.value = NA_real_, method = conditionMessage(e))
      }
    )

    pair_test |>
      mutate(comparison = paste(pr, collapse = "__vs__"), .before = 1)
  }) |>
    mutate(p_adj = p.adjust(p.value, method = "holm")) |>
    select(comparison, estimate, p.value, p_adj, method) |>
    mutate(
      outcome   = outcome,
      o_cat     = "mean",
      predictor = predictor_label,
      .before = 1
    ) |>
    mutate(
      comparison_group_1 = map_chr(str_split_i(comparison, fixed("__vs__"), 1), decorate_level),
      comparison_group_2 = map_chr(str_split_i(comparison, fixed("__vs__"), 2), decorate_level),
      .after = comparison
    ) |>
    mutate(
      g1_p_cat1 = str_split_i(str_split_i(comparison, fixed("__vs__"), 1), fixed("__:__"), 1),
      g1_p_cat2 = str_split_i(str_split_i(comparison, fixed("__vs__"), 1), fixed("__:__"), 2),
      g2_p_cat1 = str_split_i(str_split_i(comparison, fixed("__vs__"), 2), fixed("__:__"), 1),
      g2_p_cat2 = str_split_i(str_split_i(comparison, fixed("__vs__"), 2), fixed("__:__"), 2),
      .after = comparison_group_2
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

merge_p_letters <- function(one_pw_test_set, calc_stats_df, crit_lev = 0.05,
                            p_adjust_method = "BH"){

  groups <- c(one_pw_test_set$comparison_group_1, one_pw_test_set$comparison_group_2) %>% unique

  merge_prep1 <- one_pw_test_set %>%
    mutate(p_value = p.adjust(p.value, method = p_adjust_method))

  merge_prep2 <- map(groups, function(x){
    merge_prep1 %>% filter(
      comparison_group_1 %in% x | comparison_group_2 %in% x
    ) %>%
      mutate(
        ref_group = x,
        comparison = if_else(comparison_group_1 == x, comparison_group_2, comparison_group_1),
        p_cat1           = if_else(comparison_group_1 == x, g1_p_cat1, g2_p_cat1),
        p_cat2           = if_else(comparison_group_1 == x, g1_p_cat2, g2_p_cat2),
        comparison_pcat1 = if_else(comparison_group_1 == x, g2_p_cat1, g1_p_cat1),
        comparison_pcat2 = if_else(comparison_group_1 == x, g2_p_cat2, g1_p_cat2),
        .after = predictor)
  }) %>% bind_rows()

  not_nested <- merge_prep2 %>% pull(nest) %>% is.na() %>% all

  if (not_nested) {

    merge_prep3 <- merge_prep2 %>%
      mutate(cross_break = predictor, .before = 1) %>%
      mutate(p_cat2 = "Total", comparison_pcat2 = "Total")
  } else {

    merge_prep3 <- merge_prep2 %>%
      mutate(cross_break = paste0(nest, "_X_", predictor), .before = 1)
  }

  has_p_cat2 <- "p_cat2" %in% names(calc_stats_df)

  letters_match <- calc_stats_df %>%
    filter(cross_break == merge_prep3$cross_break %>% unique) %>%
    mutate(p_cat2 = if (has_p_cat2) p_cat2 else "Total") %>%
    mutate(across(c(p_cat1, p_cat2), as.character)) %>%
    select(cross_break, outcome, o_cat, p_cat1, p_cat2, sig_letter, any_of("stat"))

  comparison_letters <- letters_match %>%
    distinct(comparison_pcat1 = p_cat1, comparison_pcat2 = p_cat2, comparison_letter = sig_letter)

  merge_prep4 <- full_join(merge_prep3, letters_match) %>%
    relocate(sig_letter, .after = cross_break) %>%
    left_join(comparison_letters, by = c("comparison_pcat1", "comparison_pcat2"))

  p_cats_in_cs_df <- calc_stats_df %>% select(contains("p_cat")) %>% names

  # One row per group (its letter), summing up its comparisons:
  #   sig_diff     - letters of groups it differs from significantly
  #   sig_untested - letters of groups it couldn't be tested against (too few cases)
  #   sig_method   - the test used for the comparisons that ran
  # Rows with no comparisons at all (method is NA) are left out, as before.
  merge_prep4 %>%
    filter(!is.na(method)) %>%
    mutate(tested      = !is.na(p_value),
           significant = tested & p_value < crit_lev) %>%
    group_by(cross_break, sig_letter, outcome, o_cat, predictor, p_cat1, p_cat2, across(any_of("stat"))) %>%
    summarise(
      sig_diff     = paste0(comparison_letter[significant], collapse = ", "),
      sig_untested = paste0(comparison_letter[!tested], collapse = ", "),
      sig_method   = paste0(unique(method[tested]), collapse = "; "),
      .groups = "drop"
    ) %>%
    # Nothing to list: NA rather than "".
    mutate(across(c(sig_diff, sig_untested, sig_method), ~ na_if(.x, ""))) %>%
    select(cross_break, outcome, sig_letter, any_of(p_cats_in_cs_df), o_cat,
           sig_diff, sig_untested, sig_method, any_of("stat"))
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
                              pw_outcome_levels = NULL, pw_crit_lev = 0.05, 
                             pw_adjust = "BH") {

  merged_tables <- add_letters(merged_tables)

  if (is.null(predictors)) return(merged_tables %>% mutate(sig_diff = NA_character_, sig_untested = NA_character_, sig_method = NA_character_))

  eligible_predictors <- predictors %>% discard(~ length(.x) > 2)
  if (length(eligible_predictors) == 0) return(merged_tables %>% mutate(sig_diff = NA_character_, sig_untested = NA_character_, sig_method = NA_character_))

  pw_results <- pwtest_over_all_combinations(
    data = data, outcome = outcomes, predictor = eligible_predictors,
    statistics = statistics, all_outcome_levels = pw_outcome_levels, crit_lev = pw_crit_lev
  )

  if (length(pw_results) == 0) return(merged_tables %>% mutate(sig_diff = NA_character_, sig_untested = NA_character_, sig_method = NA_character_))

  ready_to_merge <- map(pw_results, ~ merge_p_letters(.x, 
                                                      merged_tables, 
                                                      p_adjust_method = pw_adjust,
                                                      crit_lev = unique(.x$crit_lev)
                                                      )) %>%
    bind_rows()

  # Only one statistic per outcome is pairwise tested (see
  # pwtest_over_all_combinations()). Note which, so rows of any other
  # statistic (e.g. count alongside perc) aren't marked "-".
  tested_stats <- ready_to_merge %>% distinct(outcome, stat) %>% mutate(stat_tested = TRUE)

  merged_tables %>%
    full_join(ready_to_merge) %>%
    left_join(tested_stats, by = c("outcome", "stat")) %>%
    mutate(sig_diff = case_when(
      # Couldn't be tested against any other group (too few cases).
      !is.na(sig_untested) & is.na(sig_method) ~ "n/t",
      cross_break != "Total" & is.na(sig_diff) & stat_tested %in% TRUE ~ "-",
      TRUE ~ sig_diff
    )) %>%
    select(-stat_tested)
}
