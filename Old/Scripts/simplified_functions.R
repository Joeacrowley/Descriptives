# =============================================================================
# SIMPLIFIED / DE-DUPLICATED VERSIONS — now self-contained
#
# Draft only — not wired into source_folder_r() or the rest of the project.
# Nothing in the original Scripts/ folder has been touched. Read, test, and
# diff against current output before replacing anything.
#
# This file is now fully self-contained: everything calc_stats() needs to run
# end to end is defined below, in one script. See test_simplified_functions.R
# (same folder) for known-answer tests you can run yourself — see that file's
# header for why I can't run/verify them in this environment.
#
# This file REPLACES (if you choose to adopt it):
#   weighted_mean.R, unweighted_mean.R, weighted_median.R, unweighted_median.R,
#   weighted_sum.R, unweighted_sum.R, weighted_perc.R, unweighted_perc.R,
#   weighted_count.R, unweighted_count.R          (969 lines -> ~260 below)
#   calc_stats.R                                  (dispatch rewritten to use a registry)
#   unweighted_test_numeric_by_cat.R, weighted_test_numeric_by_cat.R,
#   unweighted_test_cat_by_cat.R, weighted_test_cat_by_cat.R, return_pvalues.R
#                                                  (122 lines -> ~70 below)
#   list_depth.R                                  (17 lines -> 1 line, see bottom)
#   grouped_medianse.R, grouped_medianci.R, standardise_names.R, create_bases.R,
#   base_information.R, prepare_base_for_table.R, common_prefix.R,
#   convert_multicodes.R, vars_exist.R, check_all_factors.R,
#   outcomes_not_in_predictors.R, map_return_p_values.R
#     — reproduced unchanged below (checked each one individually: already
#       single-purpose, no duplication to remove — just copied in so this file
#       doesn't depend on anything else being sourced).
#
# NOT reproduced, and NOT needed to run calc_stats():
#   df_to_dense_flextable.R  — display/formatting only (huxtable/flextable),
#                              not called by calc_stats() itself.
#   source_folder_r.R, write_function.R — dev-time utilities for the old
#                              multi-file workflow; moot now everything's here.
#   manage_predictors.R      — dead code per Overview.qmd (calc_stats() no
#                              longer calls it). Recommend deleting it from
#                              Scripts/ separately.
#
# SIX ISSUES FOUND WHILE PORTING — flagged, not silently kept or silently
# fixed without saying so:
#
#  1. weighted_perc.R currently ends with `mutate(unweighted_n = base)`,
#     which overwrites each category's real cell count with the predictor
#     group's total N. weighted_count.R, unweighted_perc.R and
#     unweighted_count.R do NOT do this — they correctly keep the per-category
#     count. This looks like copy-paste residue from weighted_mean/median/sum
#     (where unweighted_n legitimately equals base, since those stats have one
#     row per group, not one row per category). This draft does NOT reproduce
#     that overwrite for w_perc — check whether anything downstream currently
#     relies on the old (probably wrong) behaviour before adopting this.
#
#  2. unweighted_count.R's predictor-level SE currently computes
#     `prop = estimate / sum(base)`, where `base` was itself just created two
#     lines earlier in the same mutate() as a per-row-repeated group total.
#     Because mutate() evaluates sequentially, `sum(base)` sums that already-
#     repeated constant across every category row in the group — inflating
#     the denominator by roughly the number of categories, and biasing
#     estimate_se low for any outcome with more than one level. This draft
#     uses `prop = estimate / base` (the total, not its resummed value) below.
#     See test_simplified_functions.R for a worked example showing the size
#     of the difference (roughly 1.6x too small in that example).
#
#  3. Found via writing the tests, not just reading the code: when
#     grouped_medianci() drops a small subgroup (5 or fewer observations),
#     the original unweighted_median.R still does
#     `paste0(as.character(estimate_low), " - ", as.character(estimate_upp))`
#     on the resulting NA values. In R, paste0(NA, " - ", NA) returns the
#     literal string "NA - NA", not NA — so the original renders missing CIs
#     as a misleading-looking string rather than an actual missing value.
#     This is pre-existing behaviour, not something introduced by this
#     refactor, but apply_conf_columns() below fixes it (produces true NA
#     instead) since it doesn't cut any functionality to do so.
#
#  4. Found by the first real test run, not by reading the code: both this
#     file and the original attr(x, "label") / attributes(x)$label pattern
#     return NULL for a variable with no "label" attribute. That's not the
#     problem by itself — the problem is that `mutate(o_lab = NULL)` doesn't
#     set o_lab to NA, it skips creating the column entirely, so a later
#     `select(..., o_lab, ...)` fails outright with "Can't select columns
#     that don't exist." Real project data (loaded via haven/labelled)
#     always has a label attribute, which is why this never surfaced before
#     — it only shows up for unlabelled data, like the synthetic test
#     tibbles. label_or_var_name() below falls back to the variable's own
#     name in that case (matching standardise_names()'s existing p_lab
#     fallback, which the original never applied to o_lab). For labelled
#     data the result is identical to before either way.
#
#  5. A bug in this file's own first draft (not the original): run_assoc_test()
#     used bare `.` inside `%>% { map_df(stat_labels, ~ mutate(., stat = .x)) }`.
#     purrr's `~` lambda syntax treats `.` as an alias for `.x` *inside the
#     lambda*, shadowing the outer magrittr `.` from the `%>% {}` block — so
#     `.` resolved to the current stat label string, not the tibble, and
#     `mutate()` got called on a character scalar. Fixed by assigning the
#     tibble to a named variable first and referencing that name explicitly,
#     same approach the original functions already used to avoid this exact
#     ambiguity.
#
#  6. Pre-existing in the original, spotted by inspection (flagged by you,
#     not caught by the test suite — see the note on test 8/11 below):
#     weighted_median.R and weighted_sum.R both wrote `sym(zap_labels(cur_outcome))`
#     — zapping the labels off the outcome's *name string*, not its values.
#     zap_labels() only means anything applied to a vector; on a string it's
#     a no-op, which defeats the likely original intent (some survey_median()/
#     survey_total() computations misbehave on a haven_labelled numeric
#     column). Fixed by zapping the actual column via mutate() before
#     summarise() runs, namespaced as haven::zap_labels() since haven wasn't
#     otherwise attached. weighted_mean.R never had this line at all.
#
# Items 4 and 5 are bugs introduced while porting, caught by running the
# tests in test_simplified_functions.R against real R rather than by
# reading the code — 1, 2, 3 and 6 are pre-existing issues in the original.
# =============================================================================

library(tidyverse)   # dplyr/purrr/stringr/tibble — map(), reduce(), str_*(), etc.
library(srvyr)       # survey_mean(), survey_median(), survey_total(), survey_prop()
library(survey)      # svyglm(), svyranktest(), regTermTest(), svychisq()
library(haven)       # zap_labels()
library(labelled)    # var_label()
library(wrappedtools) # medianse(), median_cl_boot()


# ---- shared engine for the 10 mean/median/sum/perc/count functions ----------

# `compute` gets both a pre-grouped view of the data (for the common case: one
# group_by() %>% summarise() call) and the raw filtered-but-ungrouped data plus
# the current predictor names (for statistics — currently just
# unweighted_median — that can't be expressed as a single summarise() and need
# to do their own split/apply, e.g. calling grouped_medianse()/grouped_medianci()).
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
      grouped  <- if (length(group_vars) > 0) group_by(filtered, across(all_of(group_vars))) else filtered

      tbl <- compute(grouped = grouped, filtered = filtered,
                     outcome = cur_outcome, predictor = cur_predictor, conf = conf)

      # mean/median/sum: summarise() consumed cur_outcome into `estimate`, so
      # there's no column literally named after the outcome at this point.
      # This creates one, holding the stat name — the rename() a few lines
      # down (or standardise_names() for the predictor branch) then moves it
      # into o_cat, so e.g. a mean row's o_cat reads "mean" (there's no real
      # category to show for a single summary number). Matches the original
      # weighted_mean.R / unweighted_mean.R / *_median.R / *_sum.R.
      #
      # perc/count: DON'T do this. The column named after the outcome at this
      # point is the actual category being tabulated (e.g. "Yes"/"No", from
      # group_by(cur_outcome) before summarise()) — overwriting it with the
      # stat name destroys the real category data. The original
      # weighted_perc.R / unweighted_perc.R / weighted_count.R /
      # unweighted_count.R never had this line for exactly that reason.
      # Missing this distinction in the first draft of this file caused every
      # o_cat to read "perc" instead of "Yes"/"No" — caught by test C.
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
        mutate(stat = stat_label) %>%
        mutate(across(contains(c("cross_break", "predictor", "p_lab", "p_cat")),
                      ~ if_else(is.na(.x), "Total", as.character(.x)))) %>%
        select(contains(c("cross_break", "predictor", "p_lab", "p_cat")),
               outcome, o_lab, o_cat, stat, contains("estimate"),
               base, base_description, any_of("unweighted_n")) %>%
        apply_conf_columns(conf)
    })

    result <- output %>% bind_rows()
    # mean/median/sum have one row per group, so unweighted_n == base and is
    # created here (matches the original *_mean/*_median/*_sum tail step).
    # perc/count already set their own per-category unweighted_n inside
    # `compute` and must not have it overwritten — see discrepancy #1 above.
    if (!group_on_outcome) result <- result %>% mutate(unweighted_n = base)
    result
  }))
}

# Survey objects store variables under $variables; plain data frames don't.
# The original weighted_*.R / unweighted_*.R files each hardcode one or the
# other — this is the one place that decision needs to live.
pull_outcome_var <- function(data, cur_outcome) {
  if (inherits(data, "tbl_svy")) data$variables[[cur_outcome]] else data[[cur_outcome]]
}

# DISCREPANCY #4, found by the first test run rather than by reading the code:
# both this file and the original attr(x, "label") / attributes(x)$label
# pattern return NULL for a variable with no "label" attribute (e.g. plain
# unlabelled data, like the synthetic tibbles in the test file — real project
# data loaded via haven/labelled always has one, which is why this never
# showed up before). The bug isn't the NULL itself, it's what mutate() does
# with it: `mutate(o_lab = NULL)` doesn't set o_lab to NA, it *skips creating
# the column entirely* — so a later `select(..., o_lab, ...)` that treats it
# as a required column fails with "Can't select columns that don't exist."
#
# Fallback is the variable's own name, not NA — this matches
# standardise_names()'s existing p_lab handling a few lines below
# (`if (is.null(p_label)) p_label <- preds[number]`), which already falls
# back to the predictor's name. o_lab didn't have the equivalent fallback
# anywhere in the original code; giving it one here makes the two consistent
# rather than leaving o_lab blank while p_lab isn't. For labelled data,
# behaviour is unchanged either way (attr() finds the real label first).
label_or_var_name <- function(x, var_name) {
  lab <- attr(x, "label")
  if (is.null(lab)) var_name else lab
}

# Replaces two things that were previously separate: an inline per-outcome
# "if ci, paste the interval; else blank the SE" step, AND a second pass after
# the final bind_rows() (present only in the five unweighted_*.R files) that
# dropped whichever of se/low/upp wasn't actually requested. Both collapse
# into one step because the transformation is row-independent — applying it
# once per outcome, before the final bind_rows, gives the same result as
# applying it once to the whole stacked table afterward.
apply_conf_columns <- function(tbl, conf) {
  if (is.null(conf)) {
    tbl %>% mutate(estimate_se = "-", .after = estimate) %>%
      select(-any_of(c("estimate_low", "estimate_upp")))
  } else if (conf == "se") {
    tbl %>% select(-any_of(c("estimate_low", "estimate_upp")))
  } else { # "ci"
    # if_else (not paste0 directly on possibly-NA bounds) so a dropped small
    # subgroup — see grouped_medianci()'s note further down — renders as a
    # real NA rather than the literal string "NA - NA". See discrepancy #3
    # at the top of this file.
    tbl %>% mutate(estimate_ci = if_else(is.na(estimate_low) | is.na(estimate_upp),
                                          NA_character_,
                                          paste0(estimate_low, " - ", estimate_upp))) %>%
      select(-any_of("estimate_se"))
  }
}


# ---- mean --------------------------------------------------------------------

# DISCREPANCY #6 (pre-existing in the original, not introduced by this
# refactor): weighted_median.R and weighted_sum.R both wrap the SYMBOL in
# zap_labels() — `sym(zap_labels(cur_outcome))` — but cur_outcome is the
# variable-name *string*, and zap_labels() is meant to strip haven_labelled
# attributes off a *vector's values*. Applied to a string it's a no-op, which
# defeats what this was almost certainly meant to do: some survey_median()/
# survey_total() computations misbehave on a haven_labelled numeric column,
# so the outcome's actual values need zap_labels() applied before the
# statistic is computed, not its name. Fixed below by zapping the column
# itself via mutate() (namespaced as haven::zap_labels(), since haven isn't
# otherwise attached) before summarise() runs, then referencing the outcome
# normally with plain sym(outcome). weighted_mean.R never had this line at
# all (it uses `sym(cur_outcome)` directly), which is presumably fine only
# because its computation happens to be less sensitive to the labelled class.

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
# weighted_median is the easy one: srvyr's survey_median() has `vartype=` built
# in, same shape as weighted_mean. unweighted_median is the real special case —
# see the "why does it check list depth" / median discussion: there's no
# analytic SE/CI for a plain median, so wrappedtools::medianse()/median_cl_boot()
# do their own split-by-group work via grouped_medianse()/grouped_medianci(),
# which is why this compute closure needs `filtered` + `predictor` rather than
# just the pre-grouped tibble.

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
        # Total case: nothing to split on, call wrappedtools directly on the vector.
        if (conf == "se") {
          return(point_estimate %>% mutate(estimate_se = wrappedtools::medianse(filtered %>% pull(outcome))))
        } else {
          ci <- wrappedtools::median_cl_boot(filtered %>% pull(outcome))
          return(point_estimate %>% mutate(estimate_low = ci$CIlow, estimate_upp = ci$CIhigh))
        }
      }

      # Predictor case: delegate to the existing helpers, which do their own
      # splitting on the raw (ungrouped) data. NOTE — grouped_medianci() drops
      # any subset with 5 or fewer observations, so this full_join can leave
      # some rows with a median but NA CI. Existing behaviour, not something
      # this refactor changes — worth a known-answer test on a small subgroup
      # to confirm apply_conf_columns()'s paste0() produces NA, not "NA - NA".
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


# ---- percentage --------------------------------------------------------------
# group_on_outcome = TRUE for both perc and count: the outcome's own
# categories are part of what gets grouped on, since the outcome IS the thing
# being tabulated.

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

# See discrepancy #2 at the top of this file re: `prop`.
unweighted_count <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "count", group_on_outcome = TRUE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      counted <- grouped %>% summarise(estimate = n())
      if (!is.null(predictor)) counted <- counted %>% group_by(across(all_of(predictor)))
      counted %>%
        mutate(base         = sum(estimate, na.rm = TRUE),
               prop         = estimate / base,   # original: estimate / sum(base) — see note above
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
  w_mean   = weighted_mean,    w_median = weighted_median,
  w_sum    = weighted_sum,     w_perc   = weighted_perc,
  w_count  = weighted_count
)

calc_stats <- function(
    data,
    outcomes,
    predictors = NULL,
    statistics = c("count", "mean"),
    conf = NULL,
    base = NULL,
    pval = NULL,
    multicode = TRUE
) {

  if (is.character(predictors)) predictors <- list(predictors)

  # Error handling — unchanged from the original.
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

      # Same string-mangling type filter as the original: for a factor
      # outcome this strips "mean"/"median"/"sum" out of every element of
      # `statistics` (including out of "w_mean" -> "w_"), which isn't a real
      # registry key either — so it's excluded from the loop below just as
      # effectively as the original's `if ("w_mean" %in% statistic)` checks
      # were skipped. Preserved as-is rather than rewritten, since it already
      # produces the right result for the current fixed set of stat names.
      single_outcome <- unweighted_data %>% pull(cur_outcome)
      statistic <- statistics
      if (is.factor(single_outcome))  statistic <- str_remove_all(statistic, "mean|median|sum")
      if (is.numeric(single_outcome)) statistic <- str_remove_all(statistic, "perc|count")

      results <- list()
      for (stat in intersect(statistic, names(stat_registry))) {
        is_weighted <- str_starts(stat, "w_")
        if (is_weighted && is.null(weighted_data)) next

        # `fn` is just a variable name (short for "function") — nothing special
        # to R. Because functions are ordinary values, stat_registry[[stat]]
        # doesn't return the text "unweighted_mean", it returns that actual
        # function object, which gets stored in `fn`. From here, `fn(...)`
        # calls whatever function `fn` currently holds, exactly like typing
        # `unweighted_mean(...)` by hand — the only reason to go through a
        # variable at all is that which function that is changes every loop
        # iteration (unweighted_mean this pass, weighted_perc the next, etc.),
        # so one call site can stand in for all ten hard-coded calls.
        fn <- stat_registry[[stat]]

        # `data = if (is_weighted) ... else ...` uses if/else as an expression
        # (it evaluates to a value, A or B) rather than as a statement, so it
        # can sit directly inside the argument list here and route each
        # statistic to the right kind of data (survey object vs plain data
        # frame) without a separate if-block per statistic.
        #
        # `list(fn(...))` wraps the single tibble `fn(...)` returns before
        # handing it to append(). This isn't decoration — a data frame is
        # internally a list of its columns, so append(results, fn(...))
        # without the wrapper would splice those columns in as separate
        # top-level elements of `results` instead of adding the tibble as one
        # whole entry. list(fn(...)) makes a one-element list containing the
        # whole tibble, so append() adds exactly one thing.
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

      pvalues <- list()
      if (any(statistics %in% c("mean", "median", "sum", "perc", "count"))) {
        pvalues <- append(pvalues, list(
          map_return_p_values(data = unweighted_data, outcomes = outcomes, predictors = pvalue_preds)
        ))
      }
      if (!is.null(weighted_data) &&
          any(statistics %in% c("w_mean", "w_median", "w_sum", "w_perc", "w_count"))) {
        pvalues <- append(pvalues, list(
          map_return_p_values(data = data, outcomes = outcomes, predictors = pvalue_preds)
        ))
      }
      pvalues <- bind_rows(pvalues)
      merged_tables <- merged_tables %>% left_join(pvalues)
    } else {
      merged_tables <- merged_tables %>% mutate(p_method = NA, p_value = NA)
    }

    if (is.null(predictors)) merged_tables <- merged_tables %>% mutate(cross_break = "Total")

    if (isTRUE(multicode)) {
      merged_tables <- convert_multicodes(data = merged_tables, base_info = base, keep = "Yes")
    }

    merged_tables
  }))
}


# ---- significance tests: same scaffold-plus-closures fix ---------------------

# DISCREPANCY #5 — a bug in this file's first draft, not the original: using
# bare `.` inside `%>% { map_df(stat_labels, ~ mutate(., stat = .x)) }` is
# ambiguous. magrittr binds `.` to the piped-in tibble at the outer `%>% {}`
# block, but purrr's `~` lambda syntax *also* treats bare `.` as an alias for
# `.x` (the current stat_labels element) inside the lambda itself — and the
# inner purrr binding wins, shadowing the outer magrittr one. So `.` resolved
# to the current stat label string (e.g. "mean"), not the tibble, and
# `mutate()` was called on a character scalar. The original
# (unweighted_test_numeric_by_cat.R etc.) never had this problem because it
# assigns the tibble to a named variable (`result`) first and references that
# name explicitly inside the lambda, rather than relying on `.` at all — same
# fix applied here.
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
  run_assoc_test(data, outcome, predictor, weighted = FALSE, stat_labels = c("mean", "median", "sum"),
    test = function(data, outcome, predictor) {
      x <- data %>% pull(predictor); y <- data %>% pull(outcome)
      keep <- complete.cases(y, x); y <- y[keep]; x <- x[keep]
      model <- lm(y ~ x)
      if (shapiro.test(residuals(model))$p.value < 0.05) {
        list(method = "Kruskal–Wallis", p = kruskal.test(y ~ x)$p.value)
      } else {
        list(method = "Welch's ANOVA", p = oneway.test(y ~ x, var.equal = FALSE)$p.value)
      }
    })
}

weighted_test_numeric_by_cat <- function(data, outcome, predictor) {
  run_assoc_test(data, outcome, predictor, weighted = TRUE, stat_labels = c("w_mean", "w_median", "w_sum"),
    test = function(data, outcome, predictor) {
      filtered <- data %>% filter(if_all(c(outcome, predictor), ~ !is.na(.x)))
      frmla <- paste0(outcome, " ~ ", predictor)
      model <- survey::svyglm(frmla, design = filtered)
      if (shapiro.test(residuals(model))$p.value < 0.05) {
        list(method = "Kruskal–Wallis", p = survey::svyranktest(frmla, design = filtered)$p.value)
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
      filtered <- data %>% filter(if_all(c(outcome, predictor), ~ !is.na(.x)))
      frmla <- formula(paste0("~", outcome, " + ", predictor))
      list(method = "Chi-Square test",
           p = svychisq(frmla, design = filtered, statistic = "adjWald")$p.value %>% as.vector())
    })
}

# return_pvalues()'s 2x2 dispatch (weighted?, outcome is factor?) becomes a
# lookup instead of nested if/else — same idea as stat_registry above.
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

# map_return_p_values.R is unchanged — already minimal, keep sourcing the
# original file.


# ---- list_depth: no longer hand-written --------------------------------------
# purrr::pluck_depth() (previously called vctrs::vec_depth()) does the same
# job. It uses a different zero-point though: a plain vector has pluck_depth
# 1, where the original list_depth() gives 0 (and an empty list is 1 in both).
# The -1 keeps every existing call site — currently just
# `list_depth(predictors) == 1` in calc_stats() above — working unchanged.
list_depth <- function(x) pluck_depth(x) - 1


# =============================================================================
# EVERYTHING BELOW THIS LINE IS REPRODUCED UNCHANGED FROM THE ORIGINAL
# Scripts/ FILES — copied in (not rewritten) so this one file is enough to run
# calc_stats() without also sourcing the rest of Scripts/. Each was checked
# individually while working out what could be simplified and found to be
# already single-purpose, with no duplication worth removing.
# =============================================================================

# ---- input validation (called from calc_stats()) -----------------------------

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

outcomes_not_in_predictors <- function(outcomes, predictors) {
  predictor_vector <- unlist(predictors) %>% unique()
  overlap_of_predictors_and_outcomes <- intersect(outcomes, predictor_vector)
  length(overlap_of_predictors_and_outcomes) == 0
}


# ---- base descriptions (base_information -> create_bases -> prepare_base_for_table) ----

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

# Not called by calc_stats() itself — used downstream when formatting a
# table's base description for display. Kept here for completeness since it
# rounds out the base_information/create_bases trio.
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
    mutate(o_lab = label_or_var_name(data_to_look_in[[out_var]], out_var)) %>%   # see discrepancy #4
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
# See the comments on unweighted_median() above for why these exist and why
# they can't just be inlined into a summarise() call like everything else.

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

convert_multicodes <- function(data, base_info = NULL, keep = "Yes") {
  if (!is.null(base_info)) base_info_name_as_string <- deparse(substitute(base_info))

  multis <- data %>%
    filter(stat == "perc" | stat == "count" | stat == "w_count" | stat == "w_perc") %>%
    filter(grepl(": ", o_lab)) %>%
    group_by(across(contains(c("cross_break", "p_cat"))), outcome) %>%
    mutate(lev_num = max(row_number())) %>%
    group_by(outcome) %>%
    mutate(lev_relevant = case_when(any(o_cat == "Yes") ~ 1, TRUE ~ 0)) %>%
    filter(lev_num <= 2 & lev_relevant == 1) %>%
    mutate(left_stem = str_split_i(o_lab, ": ", 1)) %>%
    group_by(across(contains(c("crossbreak", "p_cat"))), left_stem) %>%
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
      filter(o_cat == "Yes") %>%
      mutate(o_cat = right_stem, outcome = common_prefix(outcome))

    if (is.null(base_info)) return(list(multis2))

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
    multis2 <- multis2 %>% mutate(base_description = revised_base_description)

    if (new_variable_name %in% names(base_info[[3]])) {
      keep_item_2 <- which(!(base_info[[2]] %>% names()) %in% new_variable_name)
      keep_item_3 <- which(!(base_info[[3]] %>% names()) %in% new_variable_name)
      keep_item_4 <- which(!(base_info[[4]] %>% names()) %in% new_variable_name)
      base_info[[2]] <- base_info[[2]][keep_item_2]
      base_info[[3]] <- base_info[[3]][keep_item_3]
      base_info[[4]] <- base_info[[4]][keep_item_4]
    }
    list(multis2, revised_base_label, new_variable_label)
  })

  tables <- bind_rows(map(result, ~ .x[[1]]))

  if (!is.null(base_info)) {
    new_base_descriptors <- map(result, ~ .x[[2]]) %>% unlist()
    new_base_descriptors <- new_base_descriptors[!duplicated(new_base_descriptors)]
    new_variable_labels <- map(result, ~ .x[[3]]) %>% unlist()
    new_variable_labels <- new_variable_labels[!duplicated(new_variable_labels)]

    example_of_base_info2_temp <- base_info
    base_info[[2]] <- base_info[[2]][which(!base_info[[2]] %in% new_base_descriptors)]
    base_info[[3]] <- base_info[[3]][which(!base_info[[3]] %in% new_variable_labels)]
    base_info[[4]] <- base_info[[4]][which(!base_info[[4]] %in% new_variable_labels)]
    example_of_base_info2_temp[[2]] <- c(base_info[[2]], new_base_descriptors)
    example_of_base_info2_temp[[3]] <- c(base_info[[3]], new_variable_labels)
    example_of_base_info2_temp[[4]] <- c(base_info[[4]], new_variable_labels)
    assign(base_info_name_as_string, example_of_base_info2_temp, envir = .GlobalEnv)
  }

  multis2 <- tables %>% ungroup() %>%
    select(-lev_num, -lev_relevant, -left_stem, -stem_count, -base_count, -left_stem2, -right_stem)
  bind_rows(single_codes, multis2)
}


# ---- p-value fan-out across outcomes x predictors -----------------------------
# Already minimal — nothing to simplify, reproduced as-is.

map_return_p_values <- function(outcomes, predictors, data) {
  map_df(predictors, function(pred) {
    map_df(outcomes, function(out) {
      return_pvalues(data = data, outcome = out, predictor = pred)
    })
  })
}
