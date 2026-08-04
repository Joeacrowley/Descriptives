# =============================================================================
# SIMPLIFIED / DE-DUPLICATED VERSIONS - now self-contained
#
# Draft only - not wired into source_folder_r() or the rest of the project.
# Nothing in the original Scripts/ folder has been touched. Read, test, and
# diff against current output before replacing anything.
#
# This file is now fully self-contained: everything calc_stats() needs to run
# end to end is defined below, in one script. See test_calc_stats.R (same
# folder) for known-answer tests you can run yourself - see that file's
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
#     - reproduced unchanged below (checked each one individually: already
#       single-purpose, no duplication to remove - just copied in so this file
#       doesn't depend on anything else being sourced).
#
# NOT reproduced, and NOT needed to run calc_stats():
#   df_to_dense_flextable.R  - display/formatting only (huxtable/flextable),
#                              not called by calc_stats() itself.
#   source_folder_r.R, write_function.R - dev-time utilities for the old
#                              multi-file workflow; moot now everything's here.
#   manage_predictors.R      - dead code per Overview.qmd (calc_stats() no
#                              longer calls it). Recommend deleting it from
#                              Scripts/ separately.
#
# SIX ISSUES FOUND WHILE PORTING - flagged, not silently kept or silently
# fixed without saying so:
#
#  1. weighted_perc.R currently ends with `mutate(unweighted_n = base)`,
#     which overwrites each category's real cell count with the predictor
#     group's total N. weighted_count.R, unweighted_perc.R and
#     unweighted_count.R do NOT do this - they correctly keep the per-category
#     count. This looks like copy-paste residue from weighted_mean/median/sum
#     (where unweighted_n legitimately equals base, since those stats have one
#     row per group, not one row per category). This draft does NOT reproduce
#     that overwrite for w_perc - check whether anything downstream currently
#     relies on the old (probably wrong) behaviour before adopting this.
#
#  2. unweighted_count.R's predictor-level SE currently computes
#     `prop = estimate / sum(base)`, where `base` was itself just created two
#     lines earlier in the same mutate() as a per-row-repeated group total.
#     Because mutate() evaluates sequentially, `sum(base)` sums that already-
#     repeated constant across every category row in the group - inflating
#     the denominator by roughly the number of categories, and biasing
#     estimate_se low for any outcome with more than one level. This draft
#     uses `prop = estimate / base` (the total, not its resummed value) below.
#     See test_calc_stats.R for a worked example showing the size of the
#     difference (roughly 1.6x too small in that example).
#
#  3. Found via writing the tests, not just reading the code: when
#     grouped_medianci() drops a small subgroup (5 or fewer observations),
#     the original unweighted_median.R still does
#     `paste0(as.character(estimate_low), " - ", as.character(estimate_upp))`
#     on the resulting NA values. In R, paste0(NA, " - ", NA) returns the
#     literal string "NA - NA", not NA - so the original renders missing CIs
#     as a misleading-looking string rather than an actual missing value.
#     This is pre-existing behaviour, not something introduced by this
#     refactor, but apply_conf_columns() below fixes it (produces true NA
#     instead) since it doesn't cut any functionality to do so.
#
#  4. Found by the first real test run, not by reading the code: both this
#     file and the original attr(x, "label") / attributes(x)$label pattern
#     return NULL for a variable with no "label" attribute. That's not the
#     problem by itself - the problem is that `mutate(o_lab = NULL)` doesn't
#     set o_lab to NA, it skips creating the column entirely, so a later
#     `select(..., o_lab, ...)` fails outright with "Can't select columns
#     that don't exist." Real project data (loaded via haven/labelled)
#     always has a label attribute, which is why this never surfaced before
#     - it only shows up for unlabelled data, like the synthetic test
#     tibbles. label_or_var_name() below falls back to the variable's own
#     name in that case (matching standardise_names()'s existing p_lab
#     fallback, which the original never applied to o_lab). For labelled
#     data the result is identical to before either way.
#
#  5. A bug in this file's own first draft (not the original): run_assoc_test()
#     used bare `.` inside `%>% { map_df(stat_labels, ~ mutate(., stat = .x)) }`.
#     purrr's `~` lambda syntax treats `.` as an alias for `.x` *inside the
#     lambda*, shadowing the outer magrittr `.` from the `%>% {}` block - so
#     `.` resolved to the current stat label string, not the tibble, and
#     `mutate()` got called on a character scalar. Fixed by assigning the
#     tibble to a named variable first and referencing that name explicitly,
#     same approach the original functions already used to avoid this exact
#     ambiguity.
#
#  6. Pre-existing in the original, spotted by inspection (flagged by you,
#     not caught by the test suite - see the note on test 8/11 below):
#     weighted_median.R and weighted_sum.R both wrote `sym(zap_labels(cur_outcome))`
#     - zapping the labels off the outcome's *name string*, not its values.
#     zap_labels() only means anything applied to a vector; on a string it's
#     a no-op, which defeats the likely original intent (some survey_median()/
#     survey_total() computations misbehave on a haven_labelled numeric
#     column). Fixed by zapping the actual column via mutate() before
#     summarise() runs, namespaced as haven::zap_labels() since haven wasn't
#     otherwise attached. weighted_mean.R never had this line at all.
#
# Items 4 and 5 are bugs introduced while porting, caught by running the
# tests in test_calc_stats.R against real R rather than by reading the code -
# 1, 2, 3 and 6 are pre-existing issues in the original.
#
# ADDED LATER, not part of the original porting effort: a `pairwise`
# argument, and a second weighted-percentage implementation
# (weighted_perc_svyby()/tidy_svyby_perc(), just above the "count" section
# below) that calc_stats() switches "w_perc" to when pairwise = TRUE, so it
# can retain a real joint covariance matrix across predictor levels (a
# `covmat` list-column) instead of add_pairwise_sig()'s current
# independent-SE approximation. weighted_perc() itself is untouched and
# stays the default - see the header comment directly above
# weighted_perc_svyby() for the full design rationale (why svyby() instead
# of srvyr, why a list-column instead of an attribute, current scope
# limits). See test_weighted_perc_svyby.R for the tests.
# =============================================================================


# ---- shared engine for the 10 mean/median/sum/perc/count functions ----------

# `compute` gets both a pre-grouped view of the data (for the common case: one
# group_by() %>% summarise() call) and the raw filtered-but-ungrouped data plus
# the current predictor names (for statistics - currently just
# unweighted_median - that can't be expressed as a single summarise() and need
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
      # This creates one, holding the stat name - the rename() a few lines
      # down (or standardise_names() for the predictor branch) then moves it
      # into o_cat, so e.g. a mean row's o_cat reads "mean" (there's no real
      # category to show for a single summary number). Matches the original
      # weighted_mean.R / unweighted_mean.R / *_median.R / *_sum.R.
      #
      # perc/count: DON'T do this. The column named after the outcome at this
      # point is the actual category being tabulated (e.g. "Yes"/"No", from
      # group_by(cur_outcome) before summarise()) - overwriting it with the
      # stat name destroys the real category data. The original
      # weighted_perc.R / unweighted_perc.R / weighted_count.R /
      # unweighted_count.R never had this line for exactly that reason.
      # Missing this distinction in the first draft of this file caused every
      # o_cat to read "perc" instead of "Yes"/"No" - caught by test C.
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
               # Authoritative numeric/categorical flag, stamped from the
               # same group_on_outcome fork that already decides everything
               # else about how this stat is computed - group_on_outcome is
               # TRUE for perc/count (the outcome's own categories are part
               # of the grouping) and FALSE for mean/median/sum (nothing to
               # categorise, one summary number per group). More direct than
               # comparing o_cat to stat downstream, which is only a
               # consequence of this same fork, not the fork itself.
               # convert_multicodes() overwrites this to "multicoded" for
               # whichever rows it collapses, as its own final step.
               outcome_type = if (group_on_outcome) "categorical" else "numeric") %>%
        mutate(across(contains(c("cross_break", "predictor", "p_lab", "p_cat")),
                      ~ if_else(is.na(.x), "Total", as.character(.x)))) %>%
        select(contains(c("cross_break", "predictor", "p_lab", "p_cat")),
               outcome, o_lab, o_cat, stat, outcome_type, contains("estimate"),
               base, base_description, any_of("unweighted_n")) %>%
        apply_conf_columns(conf)
    })

    result <- output %>% bind_rows()
    # mean/median/sum have one row per group, so unweighted_n == base and is
    # created here (matches the original *_mean/*_median/*_sum tail step).
    # perc/count already set their own per-category unweighted_n inside
    # `compute` and must not have it overwritten - see discrepancy #1 above.
    if (!group_on_outcome) result <- result %>% mutate(unweighted_n = base)
    result
  }))
}

# Survey objects store variables under $variables; plain data frames don't.
# The original weighted_*.R / unweighted_*.R files each hardcode one or the
# other - this is the one place that decision needs to live.
pull_outcome_var <- function(data, cur_outcome) {
  if (inherits(data, "tbl_svy")) data$variables[[cur_outcome]] else data[[cur_outcome]]
}

# DISCREPANCY #4, found by the first test run rather than by reading the code:
# both this file and the original attr(x, "label") / attributes(x)$label
# pattern return NULL for a variable with no "label" attribute (e.g. plain
# unlabelled data, like the synthetic tibbles in the test file - real project
# data loaded via haven/labelled always has one, which is why this never
# showed up before). The bug isn't the NULL itself, it's what mutate() does
# with it: `mutate(o_lab = NULL)` doesn't set o_lab to NA, it *skips creating
# the column entirely* - so a later `select(..., o_lab, ...)` that treats it
# as a required column fails with "Can't select columns that don't exist."
#
# Fallback is the variable's own name, not NA - this matches
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
# into one step because the transformation is row-independent - applying it
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
    # subgroup - see grouped_medianci()'s note further down - renders as a
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
# zap_labels() - `sym(zap_labels(cur_outcome))` - but cur_outcome is the
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


# ---- tidy_svyby_mean: parses svyby()'s coef()/vcov() output for a NUMERIC --
# ---- outcome into calc_stat_engine()'s expected shape ----------------------
# Simpler than tidy_svyby_perc() - confirmed by direct inspection
# (check_svyby_mean_naming.R), not assumed: svymean() on a single numeric
# variable returns exactly one coefficient per group, so there's no "outcome
# category" for svyby() to disambiguate with a colon suffix the way there is
# for a factor outcome. coef(props)'s names come back as just the bare
# predictor level ("E"), or - for a nested 2-variable by-formula - the same
# "."-joined pair tidy_svyby_perc() already has to split apart ("Low.E").
# split_nested_pred_level() is reused unchanged for that - it doesn't care
# whether there was ever a trailing outcome-category part to have been
# stripped off first, it just finds the "." position where both sides match
# known factor levels.
#
# No `out[[outcome]] <- ...` line here (unlike tidy_svyby_perc()) - there's
# no o_cat value to attach. calc_stat_engine() sets that generically for
# every group_on_outcome = FALSE stat (mean/median/sum, and now this) via its
# own `mutate("{cur_outcome}" := stat_label)` step, once compute() has
# already returned - see that function's own header note on this.
tidy_svyby_mean <- function(props, outcome, predictor, data = NULL) {
  est <- coef(props)
  # SE(props), not sqrt(diag(vcov(props))) - deliberately different from
  # tidy_svyby_perc(), which has to avoid SE() (it returns a differently-
  # shaped, non-flat object for a factor outcome - see that function's own
  # header note). Confirmed by direct inspection (check_svyby_mean_naming.R,
  # Section 1a) that SE() and sqrt(diag(vcov())) give IDENTICAL numbers for
  # a numeric outcome, so there was never a real need to go through vcov()
  # here at all - and a real bug caught by a real test run showed vcov()
  # actively breaks for the single-group case: calc_stat_engine() always
  # computes a Total row first (svyby() called with the dummy "..total.."
  # grouping, exactly one group), and vcov() on that single-group svyby
  # result doesn't come back as a proper matrix, so diag() errors
  # ("'dimnames' applied to non-array"). SE() doesn't have this problem.
  # weighted_mean_svyby()'s own separate vcov() call (for the real
  # cross-group covariance pairwise testing needs) is unaffected - it's
  # already guarded by `if (!is.null(predictor))` and so never runs for the
  # Total row in the first place.
  se  <- as.numeric(survey::SE(props))
  pred_level <- names(est)

  out <- tibble(estimate = as.numeric(est), estimate_se = se)

  if (length(predictor) == 2) {
    underlying <- if (inherits(data, "tbl_svy")) data$variables else data
    levels1 <- levels(factor(underlying[[predictor[1]]]))
    levels2 <- levels(factor(underlying[[predictor[2]]]))
    split_levels <- split_nested_pred_level(pred_level, levels1, levels2)
    out[[predictor[1]]] <- split_levels$level1
    out[[predictor[2]]] <- split_levels$level2
  } else {
    out[[predictor]] <- pred_level
  }

  out %>% mutate(estimate_low = estimate - 1.96 * estimate_se,
                 estimate_upp = estimate + 1.96 * estimate_se)
}


# ---- weighted_mean_svyby: exact-covariance twin of weighted_mean(), used ---
# ---- when calc_stats(pairwise = TRUE) needs a real joint covariance matrix -
# ---- to compare means across predictor levels ------------------------------
# A SEPARATE function from weighted_perc_svyby(), not an extension of it -
# Joe's explicit call. The two aren't interchangeable internally even though
# both are svyby()-based: weighted_perc_svyby() is registered with
# group_on_outcome = TRUE (the outcome's own categories are part of the
# grouping - one row per category); this is registered with
# group_on_outcome = FALSE, same as weighted_mean() itself (nothing to
# categorise - one summary number per group). That setting changes what
# calc_stat_engine() hands compute() and how it reshapes the result
# afterward, so folding both stats into one function would mean branching on
# group_on_outcome internally rather than adding one clean new case.
#
# Everything else is the same svyby()-based mechanism weighted_perc_svyby()
# already uses (see its own header note for the full design rationale, not
# re-derived here): covmat = TRUE, the covariance matrix retained via a
# closure-scoped list keyed by block_key, reattached as a covmat list-column
# once every outcome/predictor combination has been computed.
#
# na.rm = TRUE is passed explicitly into the svymean() call - confirmed
# necessary by direct inspection (check_svyby_mean_naming.R, Section 4):
# without it, svyby(FUN = svymean) silently returns NA/NaN for an ENTIRE
# group rather than erroring or dropping just the missing rows, if the
# outcome has any NA in it at all. In practice this is redundant when called
# through calc_stats() itself - calc_stat_engine() already filters out any
# row with an NA in the outcome or predictor before compute() ever runs (see
# its own `filtered <- data %>% filter(rowSums(across(all_of(vars_needed),
# is.na)) == 0)` line) - but this function is also called directly in its
# own tests, the same way weighted_perc_svyby() is, and should behave
# correctly standing alone rather than silently relying on a caller it isn't
# guaranteed to have.
weighted_mean_svyby <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {

  covariance_store <- list()

  result <- calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "w_mean", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {

      if (length(predictor) > 2) {
        stop("weighted_mean_svyby() (used when calc_stats(pairwise = TRUE)) ",
             "doesn't support more than 2 nested predictors - got ",
             paste(predictor, collapse = ", "), ". Request pairwise = FALSE ",
             "for this call, or split it into separate calls.")
      }

      outcome_formula <- as.formula(paste0("~", outcome))

      # No real predictor for this call - route through svyby() anyway, via
      # a constant dummy grouping column, same reason and same mechanism as
      # weighted_perc_svyby()'s identical block.
      predictor_for_svyby <- predictor
      if (is.null(predictor)) {
        filtered <- filtered %>% mutate(`..total..` = factor("Total"))
        predictor_for_svyby <- "..total.."
      }
      predictor_formula <- as.formula(paste0("~", paste0("`", predictor_for_svyby, "`", collapse = " + ")))

      # Same known-working base-object requirement as weighted_perc_svyby() -
      # see its own header note (a tbl_svy passed straight into svyby()
      # produced malformed coef()/vcov() output; a plain survey.design2 does
      # not).
      design_for_svyby <- filtered
      if (inherits(design_for_svyby, "tbl_svy")) {
        class(design_for_svyby) <- setdiff(class(design_for_svyby), "tbl_svy")
      }

      props <- survey::svyby(outcome_formula, predictor_formula, design = design_for_svyby,
                              FUN = survey::svymean, na.rm = TRUE,
                              keep.var = TRUE, vartype = "se", covmat = TRUE)

      if (!is.null(predictor)) {
        block_key <- paste(outcome, paste0(predictor, collapse = "_X_"), sep = "___")
        covariance_store[[block_key]] <<- vcov(props)
      }

      tidy <- tidy_svyby_mean(props, outcome = outcome, predictor = predictor_for_svyby, data = filtered)

      # unweighted per-predictor-level N - counted by predictor alone, not
      # crossed with the outcome the way weighted_perc_svyby() crosses with
      # outcome CATEGORY. There's no category dimension for a numeric
      # outcome to break the count out by - tidy_svyby_mean()'s own output
      # has no outcome-named column at all (see its header note), so the
      # join below has to key on predictor_for_svyby alone, not
      # c(predictor, outcome) the way weighted_perc_svyby() does. The join
      # happens BEFORE the "..total.." dummy column is dropped below - it's
      # still needed here as the join key for the no-predictor case.
      unweighted_data <- if (inherits(filtered, "tbl_svy")) filtered$variables else filtered
      unweighted_counts <- unweighted_data %>%
        count(across(all_of(predictor_for_svyby)), name = "unweighted_n")

      tidy <- tidy %>% left_join(unweighted_counts, by = predictor_for_svyby)

      if (is.null(predictor)) {
        tidy %>% mutate(base = sum(unweighted_n, na.rm = TRUE)) %>%
          select(-`..total..`)
      } else {
        tidy %>% group_by(across(all_of(predictor))) %>%
          mutate(base = sum(unweighted_n, na.rm = TRUE)) %>%
          ungroup()
      }
    })

  # Re-attach the accumulated covariance matrices - identical mechanism to
  # weighted_perc_svyby()'s own tail step, see its header note.
  if ("cross_break" %in% names(result)) {
    result %>%
      mutate(.block_key = paste(outcome, cross_break, sep = "___")) %>%
      mutate(covmat = covariance_store[.block_key]) %>%
      select(-.block_key)
  } else {
    result %>% mutate(covmat = list(NULL))
  }
}


# ---- median --------------------------------------------------------------------
# weighted_median is the easy one: srvyr's survey_median() has `vartype=` built
# in, same shape as weighted_mean. unweighted_median is the real special case -
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
      # splitting on the raw (ungrouped) data. NOTE - grouped_medianci() drops
      # any subset with 5 or fewer observations, so this full_join can leave
      # some rows with a median but NA CI. Existing behaviour, not something
      # this refactor changes - worth a known-answer test on a small subgroup
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


# ---- min / max / range --------------------------------------------------------
# Unweighted only, deliberately - no w_min/w_max/w_range. A sample's observed
# minimum/maximum/range are facts about the data actually collected, not
# population estimates that sampling weights would adjust - unlike mean/sum/
# proportion, which genuinely change once under- or over-sampled groups get
# weighted up or down, the smallest/largest VALUE present in the sample
# doesn't change depending on how much each row is meant to represent. No
# analytic SE/CI either, for the same reason: these aren't estimates with
# sampling variability to quantify, so conf is accepted (for a consistent
# function signature with everything else calc_stat_engine() drives) but has
# nothing to act on - apply_conf_columns() still runs, it just never finds an
# estimate_se/estimate_low/estimate_upp column these compute() closures never
# created, so requesting conf alongside min/max/range simply produces no conf
# column for those rows (NA-filled by bind_rows() if mixed with stats that do
# have one, rather than "-" - a deliberate, if subtle, distinction: "-" means
# conf was requested but not available for this row for a known reason within
# apply_conf_columns() itself, NA here means the concept doesn't apply at all).

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
# Point estimate only, both weighted and unweighted - agreed directly rather
# than building CI/SE support now. Unlike mean/sum, IQR has no ready-made
# bootstrap helper the way median does (wrappedtools::medianse()/
# median_cl_boot()) - a real CI here would mean writing that bootstrap logic
# from scratch, deferred rather than done as a side effect of this pass.

unweighted_iqr <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {
  calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "iqr", group_on_outcome = FALSE,
    compute = function(grouped, filtered, outcome, predictor, conf) {
      grouped %>% summarise(estimate = IQR(!!sym(outcome), na.rm = TRUE),
                             base     = n())
    })
}

# survey_quantile() is srvyr's design-based quantile estimator - same family
# as the survey_median() weighted_median() already uses, just asked for the
# 0.25/0.75 points instead of 0.5. Zapping haven_labelled attributes off the
# outcome's values before summarise() runs, same as weighted_median()/
# weighted_sum() (discrepancy #6) - IQR is quantile-based like median, so the
# same risk of survey_quantile() misbehaving on a labelled column applies
# here, not just to survey_median() itself. weighted_sd() below, by contrast,
# doesn't do this - it's built on survey_var(), the same family as
# survey_mean(), which never needed the fix in the first place.
#
# BUG FIXED (caught by test 40 actually being run, not by hand-tracing):
# survey_quantile() doesn't just take on the name you assign it the way
# survey_median()/survey_mean() do - for each probability p it names its own
# output column "<assigned name>_q<p*100>", regardless of how many
# probabilities you ask for. `summarise(q25 = survey_quantile(x, 0.25))`
# therefore creates a column called "q25_q25", not "q25" - the original
# version here referenced "q25"/"q75" directly and errored with "object
# 'q75' not found" the first time it actually ran. Fixed by requesting both
# quantiles from ONE survey_quantile() call under a single stem ("quantiles"),
# whose two resulting columns are then predictably named "quantiles_q25" and
# "quantiles_q75" by the same suffixing rule. vartype = NULL (rather than
# leaving it at survey_quantile()'s own default) skips the extra *_se columns
# it would otherwise add per quantile - consistent with weighted_median()'s
# existing `vartype = conf` pattern above, and with this being a point-
# estimate-only stat in this first pass.
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
# Point estimate only (same reasoning as IQR above). The unweighted case is
# exact (base R sd()); the weighted case takes sqrt() of survey_var()'s point
# estimate - a legitimate weighted SD, but worth flagging that if this ever
# grows a CI, sqrt() of survey_var()'s CI bounds is a widely-used
# approximation, not an exact transformation (the delta method would be the
# rigorous way to do it) - moot for now since this pass is point-estimate-only
# regardless, but worth knowing before anyone reaches for that shortcut later.

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


# ---- weighted percentage via svyby() - used when pairwise = TRUE ------------
#
# A second implementation of the weighted percentage stat, alongside
# weighted_perc() above (kept, unchanged - calc_stats()'s `pairwise` argument
# below picks between the two, rather than this replacing it). Computed via
# survey::svyby()/vcov() instead of srvyr's grouped survey_prop(), so it can
# retain the joint covariance matrix across predictor levels - the piece
# weighted_perc()'s own engine never computes, which is why add_pairwise_sig()
# currently has to approximate pairwise SEs as sqrt(SE1^2 + SE2^2) rather than
# use the real covariance. weighted_perc() stays the default (pairwise =
# FALSE) since svyby() has more per-call overhead than srvyr's grouped
# summarise() and there's no reason to pay that on an ordinary table nobody's
# running pairwise tests against.
#
# Confirmed against weighted_perc() on real output (test_weighted_perc_svyby.R)
# before this integration - same estimates/SEs, within floating-point
# tolerance, across unclustered/clustered designs, with/without a predictor,
# and conf = NULL/"se"/"ci".
#
# SCOPE: single or 2-variable (nested) predictor sets - more than 2 errors
# out explicitly rather than silently building a malformed svyby() formula.
# For a nested set, svyby()'s by-formula crosses both variables (joined with
# "+"), and its combined row/coefficient label is "level1.level2" (confirmed
# by direct inspection - see check_svyby_nested_naming.R) - split back into
# (p_cat1, p_cat2) by tidy_svyby_perc() via split_nested_pred_level() below,
# matched against each predictor's own known levels rather than a blind
# first/last-"." split, since a level value could itself contain a literal
# "." (e.g. "Q1.2024"). The covariance block-key uses the same "outer_X_inner"
# join cross_break itself already uses, so the two line up.
#
# add_pairwise_sig() itself isn't scoped to nested comparisons yet -
# assign_sig_letters()/pairwise_test_one_group() key and pair only on
# p_cat1, ignoring p_cat2 - so a nested set's covmat/estimates now come
# through correctly, but the pairwise significance testing on top of them is
# a separate, not-yet-addressed gap.
#
# COVARIANCE STORAGE: a `covmat` list-column, not an attribute. calc_stats()
# itself calls bind_rows() at several separate points (the
# `reduce(results, bind_rows)` a few dozen lines below, and the top-level
# `individual_tables %>% bind_rows()`), and bind_rows() doesn't reliably
# preserve arbitrary attributes across multiple inputs - anything attached
# that way would silently vanish the moment this table gets combined with
# another. A list-column is just ordinary tibble data as far as dplyr's
# concerned, so it survives bind_rows() at any stage with no special
# handling needed anywhere. One matrix per row, duplicated across every row
# of the same (outcome, cross_break) block - cheap, since R shares the
# underlying object by reference until something modifies it - and NULL for
# rows with no real predictor (the Total block: nothing to compare a single
# estimate against).
#
# Mechanics: `covariance_store`, a plain list local to this function, gets
# written into via <<- inside compute() every time calc_stat_engine()'s
# internal loop calls it (once per outcome x predictor-set block) - a side
# channel that never touches the tibble pipeline itself, so none of
# calc_stat_engine()'s own dplyr steps (mutate/rename/bind_rows, none of
# which reliably preserve custom attributes) can strip it. Re-attached as a
# real column only once, after calc_stat_engine() has fully returned.
# ---- split_nested_pred_level: reverses svyby()'s "." join of two nested ----
# ---- predictor levels back into (level1, level2) ---------------------------
# svyby()'s combined row/coefficient label for a nested (2-variable)
# by-formula is "level1.level2" (confirmed by direct inspection - see
# check_svyby_nested_naming.R output). A blind split on the first or last
# "." would mis-parse a level value that itself contains a literal "." (e.g.
# a label like "Q1.2024") - instead this tries every "." position in turn
# and keeps the one where the left part is a real level of predictor 1 and
# the right part is a real level of predictor 2. Errors loudly if no
# position matches, rather than silently returning a wrong split; warns if
# more than one position matches (a genuinely ambiguous case) and uses the
# first, rather than pretending it's unambiguous.
split_nested_pred_level <- function(combined, levels1, levels2) {
  split_one <- function(x) {
    dot_positions <- gregexpr(".", x, fixed = TRUE)[[1]]
    if (dot_positions[1] == -1) {
      stop("split_nested_pred_level(): no '.' found in \"", x, "\" - expected ",
           "svyby()'s \"level1.level2\" format for a nested by-formula.")
    }
    candidates <- lapply(dot_positions, function(pos) {
      list(left = substr(x, 1, pos - 1), right = substr(x, pos + 1, nchar(x)))
    })
    matches <- Filter(function(cand) cand$left %in% levels1 && cand$right %in% levels2, candidates)
    if (length(matches) == 0) {
      stop("split_nested_pred_level(): couldn't match \"", x, "\" against ",
           "predictor 1/2's known levels - check for a '.' inside a factor level.")
    }
    if (length(matches) > 1) {
      warning("split_nested_pred_level(): \"", x, "\" matches more than one ",
              "possible split point across the known levels - using the first. ",
              "Check for an ambiguous/overlapping level name containing '.'.")
    }
    matches[[1]]
  }
  results <- lapply(combined, split_one)
  tibble(level1 = vapply(results, `[[`, character(1), "left"),
         level2 = vapply(results, `[[`, character(1), "right"))
}

tidy_svyby_perc <- function(props, outcome, predictor, data = NULL) {
  # Confirmed by direct inspection, not assumed: coef(props) is a flat named
  # vector, one element per (predictor level, outcome level) pair, e.g.
  # "North:responseNo". survey::SE(props) does NOT return the matching flat
  # vector for a svyby object - it returns the wide svyby-shaped data.frame
  # instead, which is where an earlier version of this function broke
  # (as.numeric() on that data.frame raised "'list' object cannot be
  # coerced to type 'double'"). vcov(props) is used instead - guaranteed to
  # align with coef()'s own flat names/order, unlike SE().
  est <- coef(props)
  se  <- sqrt(diag(vcov(props)))
  full_names <- names(est)

  colon_pos  <- regexpr(":", full_names, fixed = TRUE)
  pred_level <- substring(full_names, 1, colon_pos - 1)
  o_coefname <- substring(full_names, colon_pos + 1)
  o_level    <- substring(o_coefname, nchar(outcome) + 1)

  out <- tibble(estimate = as.numeric(est), estimate_se = se)

  if (length(predictor) == 2) {
    # `data` is whatever compute() had in scope as `filtered` - a design
    # object (tbl_svy) in every real caller, but `$variables` (the
    # underlying plain data.frame) is what actually holds the factor
    # columns needed for their levels.
    underlying <- if (inherits(data, "tbl_svy")) data$variables else data
    levels1 <- levels(factor(underlying[[predictor[1]]]))
    levels2 <- levels(factor(underlying[[predictor[2]]]))
    split_levels <- split_nested_pred_level(pred_level, levels1, levels2)
    out[[predictor[1]]] <- split_levels$level1
    out[[predictor[2]]] <- split_levels$level2
  } else {
    out[[predictor]] <- pred_level
  }

  out[[outcome]] <- o_level

  out %>% mutate(estimate_low = estimate - 1.96 * estimate_se,
                 estimate_upp = estimate + 1.96 * estimate_se)
}

weighted_perc_svyby <- function(data, outcomes, predictors = NULL, conf = NULL, base = NULL) {

  covariance_store <- list()

  result <- calc_stat_engine(data, outcomes, predictors, conf, base,
    stat_label = "w_perc", group_on_outcome = TRUE,
    compute = function(grouped, filtered, outcome, predictor, conf) {

      if (length(predictor) > 2) {
        stop("weighted_perc_svyby() (used when calc_stats(pairwise = TRUE)) ",
             "doesn't support more than 2 nested predictors - got ",
             paste(predictor, collapse = ", "), ". Request pairwise = FALSE ",
             "for this call, or split it into separate calls.")
      }

      # svyby() does its own internal grouping from the formula below -
      # `grouped`'s dplyr-level group_by() (built for weighted_perc()'s own
      # summarise()-based approach) isn't needed here; `filtered` (the same
      # rows, not yet dplyr-grouped) is enough.
      outcome_formula <- as.formula(paste0("~", outcome))

      # No real predictor for this call - route through svyby() anyway, via
      # a constant dummy grouping column, so this goes through the same
      # tidy_svyby_perc() logic as the real-predictor branch rather than a
      # separate code path.
      predictor_for_svyby <- predictor
      if (is.null(predictor)) {
        filtered <- filtered %>% mutate(`..total..` = factor("Total"))
        predictor_for_svyby <- "..total.."
      }
      # Each term backtick-wrapped individually (safe against a predictor
      # name containing spaces/special characters), joined with "+" for the
      # nested (2-variable) case - a single-element vector collapses to
      # just that one backtick-wrapped term, no separator needed.
      predictor_formula <- as.formula(paste0("~", paste0("`", predictor_for_svyby, "`", collapse = " + ")))

      # svyby() is proven to work reliably against a BASE survey.design
      # object (confirmed against real output in
      # compare_pairwise_vs_svycontrast.R); a tbl_svy (srvyr's dplyr-verb
      # wrapper on that same base design) produced malformed coef()/vcov()
      # output when passed straight in. Stripping the tbl_svy class tag
      # here - after the mutate() above, which does need it - drops back to
      # the exact object type the known-working case used, without altering
      # any of the actual design (weights/ids/strata/data all live in the
      # same underlying fields either way).
      design_for_svyby <- filtered
      if (inherits(design_for_svyby, "tbl_svy")) {
        class(design_for_svyby) <- setdiff(class(design_for_svyby), "tbl_svy")
      }

      # covmat = TRUE - the whole point of this implementation - vcov(props)
      # needs the full covariance matrix retained to be reliable, not just
      # the per-group variances vartype = "se" alone asks for.
      props <- survey::svyby(outcome_formula, predictor_formula, design = design_for_svyby,
                              FUN = survey::svymean, keep.var = TRUE, vartype = "se", covmat = TRUE)

      if (!is.null(predictor)) {
        # Same "outer_X_inner" join calc_stat_engine() already uses to build
        # cross_break itself (line ~190) - paste(outcome, predictor, ...)
        # alone would vectorize element-wise for a length-2 predictor and
        # silently produce two malformed keys instead of one that matches.
        block_key <- paste(outcome, paste0(predictor, collapse = "_X_"), sep = "___")
        covariance_store[[block_key]] <<- vcov(props)
      }

      tidy <- tidy_svyby_perc(props, outcome = outcome, predictor = predictor_for_svyby, data = filtered)
      if (is.null(predictor)) tidy <- tidy %>% select(-`..total..`)

      # unweighted per-cell N - svyby() only ever returns weighted estimates
      # and SEs, never a raw count, so this comes from the underlying data
      # directly rather than from `props`.
      unweighted_data <- if (inherits(filtered, "tbl_svy")) filtered$variables else filtered
      group_cols <- c(predictor, outcome)
      unweighted_counts <- unweighted_data %>%
        count(across(all_of(group_cols)), name = "unweighted_n")

      tidy <- tidy %>% left_join(unweighted_counts, by = group_cols)

      if (is.null(predictor)) {
        tidy %>% mutate(base = sum(unweighted_n, na.rm = TRUE))
      } else {
        tidy %>% group_by(across(all_of(predictor))) %>%
          mutate(base = sum(unweighted_n, na.rm = TRUE)) %>%
          ungroup()
      }
    })

  # Re-attach the accumulated covariance matrices as a list-column, keyed by
  # the same (outcome, cross_break) pairing compute() used to store them -
  # see the header comment above for why this happens here, as a plain
  # column, rather than as an attribute anywhere earlier in the pipeline.
  # `cross_break` won't exist at all if predictors = NULL for the whole
  # call (calc_stat_engine() never creates it in that case - calc_stats()'s
  # own outer code adds a literal "Total" cross_break column, but only
  # after this function has already returned) - covmat is just NULL for
  # every row in that case, which is the correct answer regardless (no
  # predictor was ever requested, so there's nothing to have covariance
  # information about).
  if ("cross_break" %in% names(result)) {
    result %>%
      mutate(.block_key = paste(outcome, cross_break, sep = "___")) %>%
      mutate(covmat = covariance_store[.block_key]) %>%
      select(-.block_key)
  } else {
    result %>% mutate(covmat = list(NULL))
  }
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
               prop         = estimate / base,   # original: estimate / sum(base) - see note above
               estimate_se  = sqrt(base * prop * (1 - prop)),
               estimate_low = estimate - 1.96 * estimate_se,
               estimate_upp = estimate + 1.96 * estimate_se) %>%
        select(-prop)
    })
}


# ---- add_pairwise_sig: pairwise significance testing between predictor -----
# ---- levels, run automatically by calc_stats() when pairwise = TRUE --------
#
# For a fixed outcome category, tests every pair of predictor levels against
# each other (not just "is there SOME association", which is what
# calc_stats()'s existing pval = TRUE omnibus test already answers), and
# marks which levels a given level differs significantly from. Started as a
# standalone post-hoc function (add_pairwise_sig.R) the way pval-testing
# used to be handled by a separate call too - folded in here to match how
# pval = TRUE already works: one flag, computed as part of calc_stats()
# itself, rather than a second function to remember to call afterward with
# the right upstream settings already in place.
#
# TWO WAYS THE PER-PAIR SE GETS COMPUTED, chosen per (outcome, cross_break,
# stat, o_cat) group depending on what's available:
#
# EXACT - when the group's rows carry a real `covmat` (calc_stats(pairwise =
#   TRUE) routes "w_perc" through weighted_perc_svyby(), which retains the
#   joint covariance matrix from svyby(covmat = TRUE) - see that function's
#   own header). Var(est_i - est_j) = Var_i + Var_j - 2*Cov_ij, read directly
#   off the matrix - the RIGOROUS way to handle correlation between
#   predictor-level estimates under a complex survey design (shared
#   clusters), same as svycontrast() would give.
#
# APPROXIMATE - everything else: unweighted perc/count (never touch survey
#   design machinery, so there's no covariance matrix to have), w_count (no
#   svyby()-based implementation built for it yet), or w_perc computed
#   without pairwise = TRUE. Treats predictor levels as independent:
#   SE_diff = sqrt(SE1^2 + SE2^2) - exactly correct for unclustered designs
#   or predictor levels that don't share clusters (confirmed by Monte Carlo
#   earlier - Cov is genuinely zero there, not just assumed to be), a
#   reasonable approximation otherwise.
#
# Detected via is.matrix(rows$covmat[[1]]) - not is.null()/is.na() - because
# "no real covariance available" can show up two different ways: NULL (the
# Total block, or a stat with no svyby()-based path) or NA (dplyr::bind_rows()
# filling a gap when a table gets combined with one that never had a covmat
# column at all - e.g. requesting perc and w_perc together). is.matrix() is
# FALSE for both without needing to know which one occurred.
#
# SCOPE: perc/count/w_perc/w_count only - matches pivot_crosstab()'s own
# "one statistic per categorical variable" scope. No multiple-comparison
# correction - raw pairwise p < alpha. Both inherited unchanged from the
# original standalone version.
#
# WHAT GETS ADDED:
#   sig_letter - one letter per DISTINCT predictor level, assigned once per
#     (outcome, cross_break) block, continuing globally across every block
#     in the table rather than resetting. NA for a row that wasn't eligible
#     (wrong stat, or cross_break == "Total" - nothing to compare a single
#     unbroken-down estimate against).
#   sig_diff - the sorted, comma-separated letters of every OTHER predictor
#     level this row's estimate is significantly different from. ""
#     (not NA) when tested but nothing significant found - NA means "not
#     applicable", not "tested, no result".
#
# `covmat` is left in the returned table, not dropped - keeps the option
# open to re-run pairwise tests at a different alpha, or inspect the raw
# covariance, without recomputing svyby(). select(-covmat) afterward if it's
# not wanted in whatever gets presented downstream.

# ---- derive_se: a numeric per-row SE from whichever conf calc_stats() ------
# ---- was called with - the fallback source for the approximate path --------
# calc_stats() always includes estimate_se (a literal "-" placeholder string
# when conf wasn't requested at all) - checked first. For conf = "ci",
# apply_conf_columns() drops estimate_se but keeps estimate_low/estimate_upp
# as real numeric columns even after building the estimate_ci display string
# from them - so a CI half-width recovers the SE without needing conf = "se"
# specifically.
#
# Warns and returns all-NA (not stop()) when neither is available, rather
# than erroring the whole call - calc_stats(pairwise = TRUE) can be run
# without conf set at all and still get exact results for any block that has
# a real covmat (which doesn't need this fallback), so an outright error here
# would take down pairwise testing entirely over a setting that only the
# approximate path actually needs. Rows that fall through to the approximate
# path with an NA SE get skipped by pairwise_test_one_group()'s existing
# NA-SE handling - same as any other "not enough information for this pair"
# case - not treated as significant or forced non-significant.
derive_se <- function(data) {
  if ("estimate_se" %in% names(data) && !all(data$estimate_se == "-")) {
    return(as.numeric(data$estimate_se))
  }
  if (all(c("estimate_low", "estimate_upp") %in% names(data))) {
    return((data$estimate_upp - data$estimate_low) / (2 * 1.96))
  }
  warning("add_pairwise_sig(): no estimate_se or CI columns found (calc_stats() ",
          "wasn't called with conf = \"se\" or conf = \"ci\") - rows without a ",
          "real covariance matrix will have no SE to fall back on and will be ",
          "skipped rather than tested.")
  rep(NA_real_, nrow(data))
}


# ---- assign_sig_letters: one letter per distinct predictor level, ----------
# ---- shared identically across every outcome ------------------------------
# Deliberately keyed on (cross_break, p_cat1[, p_cat2]) only - NOT outcome.
# The pivot stage (pivot_crosstab()) needs a column's reference letter to be
# a fixed, one-time lookup value - each predictor level becomes one table
# column, shown once as a legend, and that legend has to hold for every
# outcome's row under it. Keying on outcome too (an earlier version of this
# function did) would let the SAME level pick up a DIFFERENT letter in a
# different outcome's block - fine for a letter shown inline next to that
# block's own estimate, but wrong once a single legend row is meant to apply
# table-wide. Earlier considered resetting to "a" at the start of each
# outcome instead (relying on every outcome requesting the same predictor
# sets in the same order to keep letters aligned across outcomes) - dropped
# in favour of this: an outcome that happens to be missing one predictor
# level entirely (e.g. NA-filtered out for that specific outcome) would
# silently shift that outcome's own first-appearance order out of step with
# every other outcome's, under the reset-per-outcome version. Keying
# directly on (cross_break, p_cat1) sidesteps that - a level's letter is
# fixed the first time it's SEEN, in whichever outcome that happens to be,
# and every other outcome just reuses that same fixed value via the join,
# whether or not that particular outcome's own eligible rows happen to
# include every level.
#
# Originally just `distinct(across(all_of(group_cols)))` in eligible's own
# row order, trusting that order to already read "block order, then level
# order within a block" - true for a flat predictor set (calc_stat_engine()'s
# group_by() on a single factor respects its levels, same reasoning
# pivot_crosstab()'s reshape_one_predictor_set() relies on for its own
# level_order). NOT true for a nested (2-predictor) set computed via the
# svyby()-based path (weighted_perc_svyby()/tidy_svyby_perc()) - Joe caught
# this directly (a 2x2 sex x age_group legend read "a, c, b, d" instead of
# "a, b, c, d"). svyby()'s by-formula follows R's own interaction()
# convention, where the FIRST-listed predictor (p_cat1) varies FASTEST - so
# eligible's combined row order is effectively p_cat2-primary,
# p_cat1-secondary, the opposite of what a p_cat1-primary legend needs to
# read correctly (and of what pivot_nested_crosstab()'s own col_order uses -
# arrange(p_cat1, p_cat2)).
#
# Fixed by re-deriving p_cat1's (and p_cat2's, if present) own display order
# from ITS OWN first-appearance sequence within its cross_break block, rather
# than trusting the COMBINED row order - each dimension's own marginal
# first-appearance order is still correct even when the combined order isn't
# (p_cat1's distinct values still first-appear Male-then-Female under the
# "p_cat1 varies fastest" convention; p_cat2's still first-appear
# Young-then-Old). Re-levelling each as a factor on that basis and then
# arrange()-ing (which sorts by FACTOR LEVEL, not alphabetically) recovers
# "block order, then p_cat1 order, then p_cat2 order" - arrange()-ing the
# raw character columns directly, without this step, would just trade one
# bug for another (alphabetical order instead of declared level order, e.g.
# "Female" before "Male"). cross_break itself gets the same treatment so
# multi-block calls (a flat set and a nested set requested together) keep
# their existing block-to-block order rather than sorting alphabetically by
# cross_break name.
#
# sig_letter_codes: 260 unique lowercase codes for assign_sig_letters() -
# the 26 single letters a-z, then two-letter codes prefixed b-j (ba-bz,
# ca-cz, ... ja-jz - 9 prefixes x 26 = 234, so 26 + 234 = 260 total).
# Deliberately skips aa-az as the second block - starting the two-letter
# run at "b" keeps every code visually distinct from a bare single letter
# (no "a" vs "aa" confusion in a printed table), at the cost of a less
# familiar-looking sequence than Excel-style column naming. Lowercase to
# match the existing sig_letter/sig_diff convention ("a", "b", "a, c", ...).
sig_letter_codes <- c(letters, unlist(lapply(letters[2:10], function(p) paste0(p, letters))))

# More than 260 (predictor level x block) combinations in one call runs out
# of codes - warns rather than erroring, since the pairwise tests
# themselves are still valid; only the display labelling degrades (NA
# codes for the overflow, same as any other out-of-range lookup would give).
#
# p_cat2 included whenever present (a nested/2-variable predictor set) -
# without it, two rows sharing the same p_cat1 but different p_cat2 (e.g.
# "Low.E" and "Low.H") would collapse onto a single distinct() row and
# wrongly get treated as the same level, sharing one letter between two
# genuinely different predictor-level combinations.
assign_sig_letters <- function(eligible) {
  has_p_cat2 <- "p_cat2" %in% names(eligible)
  group_cols <- c("cross_break", "p_cat1")
  if (has_p_cat2) group_cols <- c(group_cols, "p_cat2")

  # See the header note above - re-level cross_break/p_cat1(/p_cat2) as
  # factors, each ordered by its OWN first-appearance sequence (cross_break
  # globally; p_cat1/p_cat2 within their own cross_break block), then
  # arrange() by factor level rather than trusting eligible's raw combined
  # row order or falling back to a plain alphabetical sort of the raw
  # character columns.
  ordered <- eligible %>%
    mutate(cross_break = factor(cross_break, levels = unique(cross_break))) %>%
    group_by(cross_break) %>%
    mutate(p_cat1 = factor(p_cat1, levels = unique(p_cat1)))
  if (has_p_cat2) {
    ordered <- ordered %>% mutate(p_cat2 = factor(p_cat2, levels = unique(p_cat2)))
  }
  ordered <- ordered %>% ungroup()

  lookup <- ordered %>%
    distinct(across(all_of(group_cols))) %>%
    arrange(across(all_of(group_cols))) %>%
    mutate(across(all_of(group_cols), as.character))   # back to the plain
    # character type eligible's own p_cat1/p_cat2 columns already carry, so
    # add_pairwise_sig()'s later left_join() (character-to-character) isn't
    # disrupted by a factor/character type mismatch.

  if (nrow(lookup) > length(sig_letter_codes)) {
    warning("add_pairwise_sig(): ", nrow(lookup), " distinct predictor levels ",
            "across this table's blocks - more than the ", length(sig_letter_codes),
            " letter codes available. The overflow will show as NA in sig_letter/sig_diff.")
  }
  lookup %>% mutate(sig_letter = sig_letter_codes[row_number()])
}


# ---- exact_se_diff: SE of a pairwise difference from a real joint ----------
# ---- covariance matrix --------------------------------------------------
# Var(est_i - est_j) = Var_i + Var_j - 2*Cov_ij - the cross term
# approx_se_diff() implicitly assumes is zero. Coefficient names built to
# match whichever svyby()-based tidy_*() function actually produced this
# covmat - "level:outcomeCategory" for weighted_perc_svyby()'s
# tidy_svyby_perc() (a factor outcome, one coefficient per category), or the
# bare level alone for weighted_mean_svyby()'s tidy_svyby_mean() (a numeric
# outcome, exactly one coefficient per group - confirmed by direct
# inspection, check_svyby_mean_naming.R, that svyby() doesn't append any
# suffix when there's no category to disambiguate). `stat` decides which -
# threaded through from pairwise_test_one_group()'s own group_keys, which
# already carries it (grouped by outcome, cross_break, STAT, o_cat
# specifically so this is always unambiguous). If either name isn't found in
# this matrix (shouldn't happen in practice, given both rows come from the
# same block, but a mismatch would otherwise silently index the wrong cell
# rather than fail visibly), returns NA rather than guessing. Also NA for a
# negative var_diff - shouldn't happen for a valid covariance matrix
# (positive semi-definiteness guarantees Var(diff) >= 0), but floating-point
# error on a near-zero true value is possible, and sqrt() of a negative
# number is worse (NaN, with its own warning) than just skipping that pair.
exact_se_diff <- function(covmat, level_i, level_j, outcome, o_cat, stat) {
  is_mean_stat <- stat %in% c("mean", "w_mean")
  key_i <- if (is_mean_stat) level_i else paste0(level_i, ":", outcome, o_cat)
  key_j <- if (is_mean_stat) level_j else paste0(level_j, ":", outcome, o_cat)
  if (!(key_i %in% rownames(covmat)) || !(key_j %in% rownames(covmat))) return(NA_real_)
  var_diff <- covmat[key_i, key_i] + covmat[key_j, key_j] - 2 * covmat[key_i, key_j]
  if (is.na(var_diff) || var_diff < 0) return(NA_real_)
  sqrt(var_diff)
}


# ---- pairwise_test_one_group: all C(n,2) pairwise z-tests within one -------
# ---- (outcome, cross_break, stat, o_cat) group of predictor-level rows -----
# `rows` must already have a numeric `.se` and a `sig_letter` column, and may
# or may not have a `covmat` column (see is.matrix() check below).
# `group_keys` is the group_modify()-supplied `.y` - a one-row tibble with
# this group's outcome/o_cat (among others) - used instead of reading
# rows$outcome/rows$o_cat directly. Two reasons: those values are constant
# within a group by definition, so pulling them once from the group's own
# key is more direct than re-reading them per row; and group_modify() strips
# the grouping columns out of `.x` by default (reattaching them to the
# OUTPUT afterward) - `.x` never having them in the first place, rather than
# keeping them in via .keep = TRUE and then having to strip them back out of
# what this function returns (group_modify() errors if the grouping columns
# come back in the output), is the simpler contract to work with.
#
# Builds sig_diff directly while looping over pairs (appending the OTHER
# level's letter to both sides of a significant pair).
#
# A pair with an NA or zero SE_diff (either method) is skipped, not treated
# as significant OR forced non-significant - it simply contributes nothing
# to either side's sig_diff, same as if that comparison had never been
# attempted.
pairwise_test_one_group <- function(rows, alpha, group_keys = NULL) {
  n <- nrow(rows)
  if (n < 2) return(rows %>% mutate(sig_diff = ""))

  # All rows in one group share the same matrix (weighted_perc_svyby()'s
  # list-column duplicates it per row, not per unique block) - checking the
  # first row is enough to know whether this whole group has one.
  has_covmat <- "covmat" %in% names(rows) && is.matrix(rows$covmat[[1]])

  # exact_se_diff()'s lookup key needs to match covmat's own rownames -
  # "p_cat1:outcomeCat" for a flat predictor, "p_cat1.p_cat2:outcomeCat" for
  # a nested one (svyby()'s own "."-join for a 2-variable by-formula - see
  # tidy_svyby_perc()'s header note). Built once per row here rather than
  # per pair, since it doesn't depend on which pair is being compared -
  # every row is compared against every other row in this group regardless
  # of whether they share a p_cat1 (e.g. "women 18-24" vs "men 18-24" is
  # just as valid a comparison as "women 18-24" vs "women 25-34").
  #
  # "p_cat2" %in% names(rows) alone isn't enough - a calc_stats() call
  # mixing a flat predictor set with a nested one puts a real p_cat2 COLUMN
  # into the whole merged table (from the nested block), and calc_stat_
  # engine() unconditionally fills every NA p_cat/cross_break/predictor
  # cell with the literal string "Total" - so a flat block's own rows pick
  # up p_cat2 == "Total" rather than staying absent. Without this check,
  # those rows would build "North.Total" instead of "North", which never
  # matches covmat's real "North:responseYes"-style rownames - the lookup
  # silently fails and sig_diff comes back empty for every row in that
  # block. p_cat2 is constant within a group (one cross_break block is
  # either flat or nested, never mixed), so checking the first row is safe.
  has_real_p_cat2 <- "p_cat2" %in% names(rows) && !identical(rows$p_cat2[1], "Total")
  level_ids <- if (has_real_p_cat2) {
    paste0(rows$p_cat1, ".", rows$p_cat2)
  } else {
    rows$p_cat1
  }

  sig_diff_letters <- vector("list", n)
  for (k in seq_len(n)) sig_diff_letters[[k]] <- character(0)

  for (pair in utils::combn(n, 2, simplify = FALSE)) {
    i <- pair[1]; j <- pair[2]

    se_diff <- if (has_covmat) {
      exact_se_diff(rows$covmat[[i]], level_ids[i], level_ids[j], group_keys$outcome, group_keys$o_cat,
                    group_keys$stat)
    } else {
      sqrt(rows$.se[i]^2 + rows$.se[j]^2)
    }

    if (is.na(se_diff) || se_diff == 0) next
    z <- (rows$estimate[i] - rows$estimate[j]) / se_diff
    p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
    if (!is.na(p) && p < alpha) {
      sig_diff_letters[[i]] <- c(sig_diff_letters[[i]], rows$sig_letter[j])
      sig_diff_letters[[j]] <- c(sig_diff_letters[[j]], rows$sig_letter[i])
    }
  }

  rows$sig_diff <- vapply(sig_diff_letters, function(x) paste(sort(x), collapse = ", "), character(1))
  rows
}


# ---- add_pairwise_sig: the main entry point ---------------------------------
# Still fully usable standalone (on any calc_stats()-shaped tibble, however
# produced) - calc_stats() calling this internally when pairwise = TRUE is
# just one more caller, not the only way to reach it.

# "mean" included alongside "w_mean" even though only "w_mean" gets a real
# covmat (via weighted_mean_svyby(), swapped in below when pairwise = TRUE) -
# unweighted_mean() never touches survey design machinery and has no
# clustering to worry about, so it stays on pairwise_test_one_group()'s
# independent-SE fallback, which is exact (not approximate) for that case.
# "mean" still needs to be in THIS list regardless, purely so
# add_pairwise_sig() picks its rows up for testing at all - is.matrix()
# on its (absent) covmat then correctly routes it to that fallback path with
# no other code change needed.
#
# "median"/"w_median" added the same way, but for a different reason than
# "mean" was: there IS no svyby()/svycontrast()-based exact path available
# for either of them, ever - confirmed via check_svyby_mean_naming.R's
# direct diagnostic run (survey::svyby(FUN = survey::svyquantile, covmat =
# TRUE) errors with "FUN does not return influence functions" - svyquantile()
# was rewritten in survey 4.1 to compute its CI via Woodruff's method rather
# than a Taylor-linearized variance, and doesn't return influence functions
# the way svymean()/svytotal() do, so covmat=TRUE has nothing to build from).
# No weighted_median_svyby() twin exists or is planned - the approximate
# (independent-SE, Cov assumed 0) fallback is the ONLY option here, not a
# stopgap chosen over a better one. Importantly, that fallback still uses
# each group's own REAL design-based SE for w_median - weighted_median()
# already computes that correctly via srvyr::survey_median()'s vartype = "se"
# (svyquantile()'s own Woodruff-CI-based SE, which does account for
# weighting and clustering) - derive_se() just reads whatever estimate_se
# calc_stats() already produced for that row, generically, the same as it
# does for every other stat here. Only the cross-group COVARIANCE is
# approximated as zero; the per-group SEs feeding into that approximation
# are never downgraded to an unweighted/design-naive formula.
pairwise_eligible_stats <- c("perc", "w_perc", "count", "w_count",
                              "mean", "w_mean", "median", "w_median")

#' Add letter-based pairwise significance testing to `calc_stats()` output
#'
#' Compares every pair of predictor levels within each (outcome, stat,
#' o_cat) group and assigns significance letters (columns sharing a
#' letter are not significantly different at `alpha`) plus a `sig_diff`
#' column listing which other levels a given level IS significantly
#' different from. Only rows whose `stat` is in `pairwise_eligible_stats`
#' (`perc`, `w_perc`, `count`, `w_count`, `mean`, `w_mean`, `median`,
#' `w_median`) are tested; an exact covariance-aware test is used where
#' one is available (`w_perc`, `w_count`, `w_mean`, via `svyby()`/
#' `svycontrast()`), and an approximate independent-SE test (zero
#' cross-group covariance, but each group's own real design-based SE)
#' everywhere else. Fully usable standalone on any `calc_stats()`-shaped
#' tibble - `calc_stats(pairwise = TRUE)` calling this internally is just
#' one caller among others.
#'
#' @param data A tibble as returned by [calc_stats()], with at least one
#'   predictor (a `cross_break` column with more than just `"Total"`).
#' @param alpha Significance threshold. Default 0.05.
#'
#' @return `data`, with two columns added: `sig_letter` (this row's own
#'   assigned letter) and `sig_diff` (the letters of levels it's
#'   significantly different from). Both `NA` for rows whose `stat` isn't
#'   pairwise-eligible.
#'
#' @seealso [calc_stats()]
#' @keywords internal
add_pairwise_sig <- function(data, alpha = 0.05) {

  if (!"cross_break" %in% names(data)) {
    stop("add_pairwise_sig() needs at least one predictor in the calc_stats() ",
         "call it's given - nothing to compare pairwise without one.")
  }

  data <- data %>% mutate(.se = derive_se(data))

  eligible <- data %>% filter(cross_break != "Total", stat %in% pairwise_eligible_stats)

  if (nrow(eligible) == 0) {
    return(data %>% mutate(sig_letter = NA_character_, sig_diff = NA_character_) %>% select(-.se))
  }

  # No "outcome" in this join, deliberately - assign_sig_letters() keys its
  # lookup on (cross_break, p_cat1[, p_cat2]) only now, so the SAME letter
  # joins onto every outcome's rows for a given predictor level, not a
  # per-outcome one (see assign_sig_letters()'s own header note for why).
  # p_cat2 included whenever present - without it, this join would match
  # every p_cat2 variant sharing a p_cat1 onto the same (wrong) letter.
  join_cols <- c("cross_break", "p_cat1")
  if ("p_cat2" %in% names(eligible)) join_cols <- c(join_cols, "p_cat2")

  letter_lookup <- assign_sig_letters(eligible)
  eligible <- eligible %>% left_join(letter_lookup, by = join_cols)

  # Grouped by stat too, not just (outcome, cross_break, o_cat) - a single
  # calc_stats() call can request more than one eligible stat at once (e.g.
  # perc AND count together), which would otherwise put two rows per
  # predictor level into the same comparison set and, worse, make the final
  # join below ambiguous (more than one eligible row per join key).
  # .x deliberately stays free of the grouping columns (group_modify()'s
  # default, .keep = FALSE) - pairwise_test_one_group() gets outcome/o_cat
  # from `.y` (this group's own key values) instead. An earlier attempt used
  # .keep = TRUE so exact_se_diff() could read rows$outcome/rows$o_cat
  # directly, but group_modify() then errors: it won't accept the grouping
  # columns coming back in the function's OUTPUT, since it reattaches them
  # itself - and pairwise_test_one_group() just returns `rows` (by then
  # including them) unchanged. Passing `.y` through sidesteps the whole
  # problem, and is arguably more correct anyway - outcome/o_cat are
  # constant within a group by definition, so reading them once from the
  # group's own key is more direct than re-reading identical values off
  # every row.
  pairwise_results <- eligible %>%
    group_by(outcome, cross_break, stat, o_cat) %>%
    group_modify(~ pairwise_test_one_group(.x, alpha, .y)) %>%
    ungroup()

  # p_cat2 included whenever present, same reason as the join above - two
  # rows sharing a p_cat1 but differing on p_cat2 must each get their own
  # sig_letter/sig_diff back, not the same one.
  final_join_cols <- c("outcome", "cross_break", "stat", "o_cat", "p_cat1")
  if ("p_cat2" %in% names(data)) final_join_cols <- c(final_join_cols, "p_cat2")

  data %>%
    left_join(pairwise_results %>% select(all_of(final_join_cols), sig_letter, sig_diff),
              by = final_join_cols) %>%
    select(-.se)
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
#' sets; optional SE/CI, p-values, and letter-based pairwise significance
#' testing.
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
#' @param pairwise If `TRUE`, runs [add_pairwise_sig()] internally and
#'   adds `sig_letter`/`sig_diff` columns (only for predictor levels, and
#'   only for statistics in `pairwise_eligible_stats`).
#'
#' @return A long-format tibble with one row per (outcome, category,
#'   predictor level, statistic): `outcome`/`o_lab` (variable name/label),
#'   `o_cat` (category, for categorical/multicoded outcomes),
#'   `cross_break`/`p_cat1`/`p_lab` (predictor variable/level/label, and
#'   `p_cat2`/`p_lab2`/`predictor2` when a nested predictor set was used),
#'   `stat` (the statistic code), `estimate` (and `estimate_se`/
#'   `estimate_ci_low`/`estimate_ci_high` if `conf` was requested), `base`
#'   (and `base_description`), plus `p_value`/`sig_letter`/`sig_diff` when
#'   requested. Feed this straight into [pivot_summary()],
#'   [pivot_crosstab()], [pivot_nested_crosstab()], [pivot_battery()], or
#'   [add_pairwise_sig()] - or use one of the `make_*()` wrappers to skip
#'   calling `calc_stats()` directly.
#'
#' @seealso [add_pairwise_sig()], [pivot_summary()], [pivot_crosstab()],
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
    pairwise = FALSE
) {

  if (is.character(predictors)) predictors <- list(predictors)

  # Error handling - unchanged from the original.
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

    # pairwise = TRUE swaps "w_perc" and "w_mean" over to their svyby()-based
    # twins (weighted_perc_svyby()/weighted_mean_svyby() - each adds a
    # `covmat` list-column, see their own header comments above), everything
    # else in the registry is untouched - including "mean" itself, which
    # deliberately stays on unweighted_mean() (see pairwise_eligible_stats'
    # own comment on why that's still correct, not an oversight). A local
    # override rather than mutating stat_registry itself, so this never
    # affects a call that doesn't ask for it.
    active_stat_registry <- stat_registry
    if (isTRUE(pairwise)) {
      active_stat_registry$w_perc <- weighted_perc_svyby
      active_stat_registry$w_mean <- weighted_mean_svyby
    }

    individual_tables <- map(outcomes, function(cur_outcome) {

      # Same string-mangling type filter as the original: for a factor
      # outcome this strips "mean"/"median"/"sum" out of every element of
      # `statistics` (including out of "w_mean" -> "w_"), which isn't a real
      # registry key either - so it's excluded from the loop below just as
      # effectively as the original's `if ("w_mean" %in% statistic)` checks
      # were skipped. Preserved as-is rather than rewritten, since it already
      # produces the right result for the current fixed set of stat names.
      single_outcome <- unweighted_data %>% pull(cur_outcome)
      statistic <- statistics
      # min/max/range/iqr/sd added to the numeric-only side of this filter
      # alongside mean/median/sum - a factor outcome can't have a numeric
      # summary computed on it any more than it can have a mean. None of the
      # five is a substring of any existing registry key (or vice versa), so
      # this stays safe the same way the original five names already were -
      # e.g. "w_iqr" loses just the "iqr" part here, leaving "w_" (not a
      # registry key), the same fate "w_mean" already had for "mean".
      if (is.factor(single_outcome))  statistic <- str_remove_all(statistic, "mean|median|sum|min|max|range|iqr|sd")
      if (is.numeric(single_outcome)) statistic <- str_remove_all(statistic, "perc|count")

      results <- list()
      for (stat in intersect(statistic, names(stat_registry))) {
        is_weighted <- str_starts(stat, "w_")
        if (is_weighted && is.null(weighted_data)) next

        # `fn` is just a variable name (short for "function") - nothing special
        # to R. Because functions are ordinary values, stat_registry[[stat]]
        # doesn't return the text "unweighted_mean", it returns that actual
        # function object, which gets stored in `fn`. From here, `fn(...)`
        # calls whatever function `fn` currently holds, exactly like typing
        # `unweighted_mean(...)` by hand - the only reason to go through a
        # variable at all is that which function that is changes every loop
        # iteration (unweighted_mean this pass, weighted_perc the next, etc.),
        # so one call site can stand in for all ten hard-coded calls.
        fn <- active_stat_registry[[stat]]

        # `data = if (is_weighted) ... else ...` uses if/else as an expression
        # (it evaluates to a value, A or B) rather than as a statement, so it
        # can sit directly inside the argument list here and route each
        # statistic to the right kind of data (survey object vs plain data
        # frame) without a separate if-block per statistic.
        #
        # `list(fn(...))` wraps the single tibble `fn(...)` returns before
        # handing it to append(). This isn't decoration - a data frame is
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

      # No single-variable predictor set to test against - e.g. every
      # predictor set requested is nested (2+ variables, as
      # pivot_nested_crosstab() requires), which map_return_p_values() has
      # nothing to compute a p-value FOR. Left to run anyway,
      # map_df(character(0), ...) - map_return_p_values()'s own inner loop,
      # in the legacy block below - returns a genuinely EMPTY tibble (zero
      # rows AND zero columns, not just zero rows), which then crashes
      # left_join() just below with "x and y have no common variables"
      # rather than harmlessly leaving every row's p_value NA. Caught by a
      # real test run (test 9, test_pivot_nested_crosstab.R) requesting
      # pval = TRUE with predictors = list(c("sex", "age_group")) - a single
      # nested set, nothing else. Short-circuited here to the same NA-filled
      # outcome pval = NULL already produces on its own, since that's
      # genuinely what this case means: pval = TRUE was requested, but
      # there's nothing valid for it to test.
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

      # ---- nested (2-variable) predictor sets: one p-value per OUTER ----
      # ---- level, testing the inner variable against the outcome WITHIN ----
      # ---- that level - see nested_pvalues()'s header note for the design ----
      # ---- call behind this. Runs independently of the flat pvalue_preds ----
      # ---- branch above - a call can mix flat and nested predictor sets ----
      # ---- in one go, each getting its own kind of p-value. -----------------
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
          # coalesce(), not a plain overwrite - a call mixing flat and
          # nested predictor sets already gave the flat rows their own
          # p_value above (non-NA); this only fills in the nested rows,
          # which are still NA at this point (never matched anything in the
          # flat join, since their cross_break is "outer_X_inner", not one
          # of pvalue_preds' bare predictor names).
          #
          # `stat` MUST be part of the join key. return_pvalues() (reused by
          # nested_pvalues()) fans out to one row per numeric stat_label
          # (mean, median, sum, min, max, range, iqr, sd) - same p-value
          # repeated across all of them, since the test doesn't depend on
          # which summary stat you asked for. The flat pvalue_preds join
          # above gets this for free (plain left_join(pvalues), no explicit
          # by =, so it auto-joins on every shared column including stat).
          # This join needs it spelled out explicitly: without it, a single
          # outer level's 8 stat-label rows each matched every p_cat2 row
          # for that outer level - an 8x fan-out that read as `estimate`
          # duplication downstream in reshape_nested_predictor_set() (caught
          # via debug_nested_estimate_duplication.R, not by hand-tracing -
          # the join looked like clean many-to-one on paper).
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

    # Runs before convert_multicodes(), not after - pairwise testing compares
    # predictor levels WITHIN each row's own (outcome, o_cat) at this point,
    # which is still true post-multicode-collapse (each row is still one
    # (predictor level, item) pair, just relabelled) - but the covmat/
    # sig_letter/sig_diff columns are keyed to outcome names that
    # convert_multicodes() is about to change (collapsing several original
    # outcome variables into one, via common_prefix()), so testing first
    # avoids a key mismatch rather than relying on convert_multicodes()'s
    # mutate()/filter()-based reshape happening to carry these columns
    # through unchanged (it should, in principle - not explicitly tested
    # together yet, so flagged rather than assumed).
    if (isTRUE(pairwise)) {
      merged_tables <- add_pairwise_sig(merged_tables)
    }

    if (isTRUE(multicode)) {
      merged_tables <- convert_multicodes(data = merged_tables, base_info = base, keep = "Yes")
    }

    merged_tables
  }))
}


# ---- significance tests: same scaffold-plus-closures fix ---------------------

# DISCREPANCY #5 - a bug in this file's first draft, not the original: using
# bare `.` inside `%>% { map_df(stat_labels, ~ mutate(., stat = .x)) }` is
# ambiguous. magrittr binds `.` to the piped-in tibble at the outer `%>% {}`
# block, but purrr's `~` lambda syntax *also* treats bare `.` as an alias for
# `.x` (the current stat_labels element) inside the lambda itself - and the
# inner purrr binding wins, shadowing the outer magrittr one. So `.` resolved
# to the current stat label string (e.g. "mean"), not the tibble, and
# `mutate()` was called on a character scalar. The original
# (unweighted_test_numeric_by_cat.R etc.) never had this problem because it
# assigns the tibble to a named variable (`result`) first and references that
# name explicitly inside the lambda, rather than relying on `.` at all - same
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

# return_pvalues()'s 2x2 dispatch (weighted?, outcome is factor?) becomes a
# lookup instead of nested if/else - same idea as stat_registry above.
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

# ---- nested_pvalues: one p-value per OUTER level, testing the INNER ----------
# ---- variable's association with the outcome WITHIN that level --------------
# Design call (Joe's, not a default assumption): for a nested predictor set
# (outer, inner) - e.g. sex x age_group - the p-value that belongs to each
# outer level is "among women, is age associated with this outcome", not a
# single omnibus test across every outer x inner cell, and not a per-cell
# test against Total. Reuses return_pvalues() as-is (same test-selection
# logic a flat predictor already gets - chi-square/ANOVA/Kruskal-Wallis/
# Wald, weighted or not) against a SUBSET of the data for each outer level
# in turn, rather than inventing a new test. `data` is whichever of
# unweighted_data/weighted_data the caller passes in; filter() works
# directly on a tbl_svy survey design object the same way it does on a plain
# data frame (srvyr supports dplyr verbs on survey designs), matching how
# calc_stat_engine() already filters both kinds of `data` elsewhere.
#
# Only ever called for exactly-2-variable predictor sets - matches
# pivot_nested_crosstab()'s own single-nesting-level scope. A 3+ variable
# predictor set (out of scope for that function too) never reaches here.
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

# map_return_p_values.R is unchanged - already minimal, keep sourcing the
# original file.


# ---- list_depth: no longer hand-written --------------------------------------
# purrr::pluck_depth() (previously called vctrs::vec_depth()) does the same
# job. It uses a different zero-point though: a plain vector has pluck_depth
# 1, where the original list_depth() gives 0 (and an empty list is 1 in both).
# The -1 keeps every existing call site - currently just
# `list_depth(predictors) == 1` in calc_stats() above - working unchanged.
list_depth <- function(x) pluck_depth(x) - 1


# =============================================================================
# EVERYTHING BELOW THIS LINE IS REPRODUCED UNCHANGED FROM THE ORIGINAL
# Scripts/ FILES - copied in (not rewritten) so this one file is enough to run
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

# Same job as check_all_factors() above (added for pivot_numeric_summary()'s
# "outcomes must all be numeric" validation), but can't reuse that function's
# class()-string-collapsing trick: check_all_factors() relies on every factor
# column's class() being the single string "factor", so unique() collapses
# them all down to one value it can compare against "factor" with plain `==`.
# A numeric column's class() isn't that uniform - integer columns report
# "integer", double columns report "numeric" - so the same trick would see
# two different strings and (via isTRUE() on a length>1 vector) always report
# FALSE even when every column genuinely is numeric. is.numeric() handles
# both without caring which one it is, so this checks that directly instead.
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

  multis <- data %>%
    filter(stat == "perc" | stat == "count" | stat == "w_count" | stat == "w_perc") %>%
    filter(grepl(": ", o_lab)) %>%
    group_by(across(contains(c("cross_break", "p_cat"))), outcome) %>%
    mutate(lev_num = max(row_number())) %>%
    group_by(outcome) %>%
    mutate(lev_relevant = case_when(any(o_cat == "Yes") ~ 1, TRUE ~ 0)) %>%
    filter(lev_num <= 2 & lev_relevant == 1) %>%
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
      filter(o_cat == "Yes") %>%
      # outcome_type overwritten here, not left as the "categorical" it was
      # stamped with in calc_stat_engine() - this is the one place that
      # actually knows a set of originally-separate columns is being
      # collapsed into one variable's levels, so it's the one place that can
      # set this authoritatively rather than leaving it to be inferred
      # downstream from p-value variation.
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

  # A real latent bug removed here, not just a refactor: this used to also
  # try to sync the CALLER's own base_info object with the newly-collapsed
  # multicode variable's consolidated entry, via
  # assign(deparse(substitute(base_info)), ..., envir = .GlobalEnv). That
  # never actually worked, even before this was packaged: convert_multicodes()
  # is called from calc_stats() as convert_multicodes(..., base_info = base,
  # ...), so substitute(base_info) inside this function always captures the
  # literal symbol `base` (calc_stats()'s OWN parameter name at that call
  # site) - never the name of whatever object the top-level caller actually
  # passed in as calc_stats(base = ...). The assign() therefore always wrote
  # to a global variable literally called "base", regardless of what the
  # real base_info object was named, silently clobbering (or creating) it in
  # the caller's session with no connection back to the actual variable -
  # and inside a package call stack it doesn't even reach the caller's
  # environment at all (assign(envir = .GlobalEnv) is always the global env,
  # never a calling frame). R CMD check's "assignments to the global
  # environment" check correctly flagged this - packages must not have
  # unexpected environment-mutating side effects like this one, and this
  # one was never doing anything meaningful to begin with.
  # None of this affected the actual DATA this function returns - the real,
  # working part (each collapsed variable's own base_description text,
  # rewritten above via revised_base_description) is untouched here, this
  # only removes the disconnected, always-broken bookkeeping that fed the
  # assign() call and nothing else.
  tables <- bind_rows(result)

  multis2 <- tables %>% ungroup() %>%
    select(-lev_num, -lev_relevant, -left_stem, -stem_count, -base_count, -left_stem2, -right_stem)
  bind_rows(single_codes, multis2)
}


# ---- p-value fan-out across outcomes x predictors -----------------------------
# Already minimal - nothing to simplify, reproduced as-is.

map_return_p_values <- function(outcomes, predictors, data) {
  map_df(predictors, function(pred) {
    map_df(outcomes, function(out) {
      return_pvalues(data = data, outcome = out, predictor = pred)
    })
  })
}
