# =============================================================================
# pivot_crosstab() — reshape calc_stats() output into a crosstab (predictor
# levels become table columns), the pivot2()/pivot2h() successor.
#
# THIS PASS adds multiple predictor sets side by side in one table (e.g. a
# Sex block next to an AgeGroup block), on top of the single-predictor-set
# slice already confirmed working. It also adds multi-code p-value support:
# a genuine multi-select variable (outcome_type == "multicoded") has each of
# its levels carry its OWN independent p-value, rather than one p-value
# shared across the whole variable. That's not a display choice - it falls
# straight out of how calc_stats() computes p-values: for a multicoded
# variable, each level was still a separate raw outcome column at the point
# map_return_p_values() ran (before convert_multicodes() collapsed them), so
# each level's p-value comes from its own independent significance test
# against the predictor. convert_multicodes() only mutate()s the collapsed
# rows afterwards - it never aggregates - so each level's own p-value
# survives the collapse untouched. The only thing that needed to change here
# is the join key used to reattach p-values to the reshaped table: keying on
# (outcome, o_cat, stat_type) instead of (outcome, stat_type) lets each
# level's row find its own matching p-value instead of every level fanning
# out to match all of them (a real risk once multiple options share one
# collapsed `outcome` name). See reshape_one_predictor_set() and the
# p-value attachment loop in pivot_crosstab() below for exactly how the two
# cases (blank-after-row-1 vs shown-on-every-row) are told apart, via
# outcome_type.
#
# How multiple predictor sets combine: each set is reshaped independently
# (reshape_one_predictor_set(), below), then the sets' estimate/conf tables
# and base tables are each full_join()'d together on the (outcome, o_lab,
# o_cat, stat) / (outcome, o_lab) identifiers that mean the same thing
# regardless of which predictor produced them - every set is reshaping the
# exact same underlying outcome/level/statistic structure, just against a
# different predictor, so this is the part I'd check most closely if
# something looks misaligned. "Total" is merged into the FIRST predictor set
# only (matching old pivot2), rather than repeated once per set.
#
# Column naming: when more than one predictor set is present, each set's
# level columns are prefixed "<predictor label>: " (e.g. "Sex: Male") so two
# different sets can never collide on a shared level name by coincidence
# (e.g. both happening to have a level called "Yes") - "Total" is the one
# exception, left bare, since it's merged from the first set and meant to
# read as a single shared column. With only ONE predictor set, level columns
# stay bare (no prefix, no collision risk) - unchanged from the previous
# pass, so the earlier tests still hold.
#
# Other scope, agreed directly:
#   - Categorical variables get exactly one statistic (validated, errors
#     otherwise). Numeric variables can still show more than one (e.g. mean
#     + median), same as pivot_summary() - same rowspan-merge behaviour will
#     apply once format_crosstab() exists.
#   - The old "wide" layout (SE/CI spread into their own columns per
#     predictor level) is gone. SE/CI live in a parallel "<level>__conf"
#     column here, alongside the estimate. Whether that becomes its own row
#     under Estimate, or gets concatenated into the same cell (e.g.
#     "35 [34-36]"), is a decision for format_crosstab() (not yet built) -
#     not a structural choice made here, since either way there's exactly
#     one estimate value and one confidence value per (variable level,
#     statistic, predictor level).
#   - Nested combined predictors (one block crossing two predictors
#     together, e.g. Sex x AgeGroup as one set of columns) are out of scope
#     - defensively dropped rather than silently mishandled, same edge case
#     old pivot2 only partially supported.
#
# Variable/Levels collapse into one "Variable" column, with a label row
# above a categorical variable's levels - same convention as pivot_summary(),
# so the two functions read the same way even though this one has several
# predictor-level columns instead of a single Estimate column.
#
# I haven't been able to run any of this myself - no R available in this
# environment. See test_pivot_crosstab.R for what's covered and how
# confident I am in each part.
# =============================================================================

library(tidyverse)
source(here::here("Scripts_new", "calc_stats.R"))
source(here::here("Scripts_new", "pivot_summary.R"))   # reuse tidy_statistic_description()

# Classifies a stat code into one of 4 "test types". Two statistics of the
# same type (e.g. mean and median, both "num_u") come from the SAME
# significance test in calc_stats() (see run_assoc_test()'s stat_labels
# fan-out in calc_stats.R) - they share one p-value, shown once, not one
# each. Deliberately returns NA (via case_when()'s default) for anything not
# recognised - e.g. a base row, where `stat` is NA - rather than erroring.
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

# ---- reshape_one_predictor_set: everything specific to ONE predictor's ----
# ---- slice of the data - estimate/conf/base wide on its own levels, plus ----
# ---- its own p-value data. Kept separate from pivot_crosstab() so each ----
# ---- set can be reasoned about (and unit-tested) in isolation before -------
# ---- being joined to the others. ------------------------------------------
#
# `prefix` is only TRUE when more than one predictor set is present in the
# table - see the header note on column naming above. When FALSE, level
# columns stay exactly as they were in the single-predictor-set pass (e.g.
# "Male"/"Female"), so nothing about that already-tested path changes here.
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

  # outcome_type carried through as an extra id column (alongside outcome/
  # o_lab/o_cat/stat) - it's constant per (outcome, o_cat, stat) regardless
  # of predictor level, so it rides along through pivot_wider() without
  # affecting row count, and needs to be part of the later full_join() key
  # too (see pivot_crosstab() below) so it doesn't pick up a ".x"/".y" suffix
  # when merging multiple predictor sets' tables back together.
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

  # sig_diff, when present (calc_stats(pairwise = TRUE) was used) - exactly
  # the same "<level>__sigdiff" parallel-column treatment as conf above, and
  # for the same reason: sig_diff varies per (outcome, o_cat, stat) row, not
  # just per level column (pairwise_test_one_group() runs separately per
  # (outcome, cross_break, stat, o_cat) group - see calc_stats.R), so it
  # needs the same "own column, read per row" mechanism conf already uses,
  # not something baked into a fixed column name. format_crosstab() reads
  # these off to build an inserted row under the estimate, same mechanic as
  # the conf row.
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

  # p_values is keyed on (outcome, o_cat, stat_type), not just (outcome,
  # stat_type) - for an ordinary categorical/numeric variable this is a
  # no-op (every o_cat within a block already shares the same p-value, so
  # distinct() just carries it along once per o_cat instead of once overall,
  # and the attachment loop below still blanks all but the first data row).
  # For a multicoded variable, o_cat (the option name, e.g. "Option A") is
  # what keeps each level's genuinely different p-value matched to its own
  # row rather than fanning out across all of them once outcome_type has
  # collapsed several options under one shared `outcome` name - see the
  # header note above.
  p_values <- rows_for_set %>%
    filter(!is.na(p_value)) %>%
    mutate(stat_type = stat_type_of(stat)) %>%
    distinct(outcome, o_cat, stat_type, p_value)

  # legend: one (level_col, sig_letter) row per real predictor level, when
  # sig_letter is present. NOT built via "paste the letter onto the pivot
  # key, then split it off the resulting column name after pivoting" (Joe's
  # first description of the mechanism) - that would need to embed the
  # letter into p_cat1_label itself, which is shared with the estimate/conf/
  # sigdiff pivots above. A predictor set that mixes a pairwise-eligible
  # stat (perc) with an ineligible one (mean) sharing the same predictor
  # would then see p_cat1_label take TWO different values for the same real
  # level ("North" for the mean rows, which never get a letter, vs
  # "North____a" for the perc rows) - pivot_wider() would create two
  # separate columns instead of collapsing into one "North". A plain
  # long-format lookup sidesteps that entirely, and is simpler besides -
  # format_crosstab() builds the actual display row from it directly, after
  # its own digit-formatting has already turned the level columns to
  # character (inserting a raw letter string into a still-numeric column
  # earlier in the pipeline would force the WHOLE column to character via
  # bind_rows()'s type coercion, corrupting every other row's formatting
  # downstream). p_cat1 (not p_cat1_label) is constant per (cross_break,
  # p_cat1) now - see assign_sig_letters()'s header note in calc_stats.R -
  # so this distinct() is guaranteed exactly one row per real level, not a
  # collision risk.
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

pivot_crosstab <- function(data) {

  # Nested combined predictors are out of scope for this function.
  data <- data %>%
    select(-any_of(grep("p_lab[2-9]|p_cat[2-9]|predictor[2-9]", names(data), value = TRUE)))

  bases <- data %>% pull(base_description) %>% unique() %>% paste0(collapse = " X ")
  if (any(!is.na(bases))) bases <- bases %>% prepare_base_for_table()
  data <- data %>% select(-base_description)

  # calc_stats() always includes estimate_se, filled with "-" when conf
  # wasn't requested - drop that placeholder here so it isn't mistaken for a
  # real (but empty) SE column below. Same fix as pivot_summary().
  if ("estimate_se" %in% names(data) && all(data$estimate_se == "-")) {
    data <- data %>% select(-estimate_se)
  }

  # conf_type records whether the "<level>__conf" columns reshape_one_
  # predictor_set() builds hold raw SE values or CI range strings -
  # information format_crosstab() will need (SE formats through
  # format_statistic() same as an estimate; a CI range string needs
  # format_ci_string() to split and format each bound) but that pivot_
  # crosstab()'s own output has no other way to signal, since both cases
  # produce a column with the same generic "__conf" suffix regardless.
  # pivot_summary()/format_summary() sidestep this by naming the column
  # itself "SE" vs "95% CI" - not an option here, since a crosstab's conf
  # column is per predictor level, not a single fixed column name. Computed
  # once, globally, since `conf` is one argument for the whole calc_stats()
  # call - every predictor set agrees on se vs ci vs neither.
  conf_type <- if ("estimate_ci" %in% names(data)) "ci" else if ("estimate_se" %in% names(data)) "se" else NA_character_

  # ---- validation: at least one predictor set, in first-appearance order ----
  predictor_sets <- data %>% filter(cross_break != "Total") %>%
    mutate(cross_break = fct_inorder(cross_break)) %>% pull(cross_break) %>% levels()
  if (length(predictor_sets) == 0) {
    stop("pivot_crosstab() needs at least one predictor - use pivot_summary() for a Total-only table.")
  }

  # ---- validation: categorical variables get exactly one statistic ----
  # Stat-invariant across predictor sets by construction (one calc_stats()
  # call applies the same `statistics` argument to every predictor set), so
  # this is checked once on the whole table rather than per set.
  # outcome_type %in% c("categorical", "multicoded") replaces the earlier
  # `o_cat != stat` comparison - same set of rows, checked directly instead
  # of via a side effect of how calc_stat_engine() happens to fill o_cat.
  cat_stat_counts <- data %>% filter(outcome_type %in% c("categorical", "multicoded")) %>%
    distinct(outcome, stat) %>% count(outcome)
  offending <- cat_stat_counts %>% filter(n > 1) %>% pull(outcome)
  if (length(offending) > 0) {
    stop("pivot_crosstab() only supports one statistic per categorical variable. ",
         "Multiple statistics were requested for: ", paste(offending, collapse = ", "))
  }

  # ---- split into one slice per predictor set - "Total" merged into the ----
  # ---- first set only, rather than repeated for every set -------------------
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

  # ---- merge all sets' estimate/conf tables and base tables together ----
  # This is the part I'd check most closely against real data - see the
  # header note above on why (outcome, o_lab, o_cat, stat) is a safe join
  # key. outcome_type rides along in the key too, purely so it doesn't pick
  # up a ".x"/".y" suffix from full_join() - it's the same value for a given
  # outcome/stat regardless of which predictor set produced it.
  estimate_wide <- reduce(map(set_results, "estimate_wide"), full_join,
                          by = c("outcome", "o_lab", "o_cat", "stat", "outcome_type"))
  base_wide <- reduce(map(set_results, "base_wide"), full_join, by = c("outcome", "o_lab"))

  all_level_cols <- map(set_results, "level_order") %>% unlist(use.names = FALSE) %>% unique()

  # ---- build final row structure: label rows, data rows - mirrors -----------
  # ---- pivot_summary()'s pivot_one_block(), just with several predictor- ----
  # ---- level (+ conf) columns instead of one Estimate column ----------------
  pivot_one_block <- function(block) {
    # outcome_type comes straight from calc_stats() (see the corresponding
    # note in pivot_summary.R) - replaces the earlier `o_cat == stat`
    # comparison. Kept in the select() below (it used to be dropped right
    # after this line) because the p-value attachment loop further down
    # needs it on every row of `pivoted` to tell a multicoded variable's
    # per-level p-values apart from an ordinary shared-p-value block.
    is_categorical <- block$outcome_type[1] %in% c("categorical", "multicoded")

    data_rows <- block %>%
      mutate(Variable = if (is_categorical) o_cat else o_lab[1],
             row_type  = "data") %>%
      select(Variable, row_type, outcome, o_lab, o_cat, stat, outcome_type,
             all_of(all_level_cols), any_of(paste0(all_level_cols, "__conf")),
             any_of(paste0(all_level_cols, "__sigdiff")))

    if (!is_categorical) return(data_rows)

    # Label row: `stat` is copied from the first level row for now (so
    # tidy_statistic_description() further down has a real value to match on
    # for every row it processes) - blanked back to NA for display via the
    # explicit Statistics override near the end of this function. The level/
    # conf columns are blanked here directly since there's nothing to copy
    # them FROM meaningfully (a label row spans no single predictor level).
    #
    # o_cat gets the same treatment, and this one isn't cosmetic: a label
    # row was previously left holding a copy of the FIRST level's o_cat
    # (e.g. "Option A"), which is actively wrong - a label row doesn't
    # belong to any single level, and once the p-value join below started
    # keying on (outcome, o_cat, stat_type) that stale copy would have
    # quietly matched the label row to the first level's p-value on the
    # join (harmless only because row_type != "data" always blanks it back
    # out regardless of what the join found - but relying on that rather
    # than just holding a correct value here was fragile). Blanked to
    # NA_character_ explicitly, not via the across() above, since o_cat
    # isn't one of the level/conf columns that mutate() covers.
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

  # ---- one Base row per variable, appended after its last label/data row ----
  outcomes_in_order <- pivoted %>% pull(o_lab) %>% unique()

  pivoted <- map_df(outcomes_in_order, function(this_lab) {
    variable_rows <- pivoted %>% filter(o_lab == this_lab)
    base_row <- base_wide %>% filter(o_lab == this_lab) %>%
      # Variable = this_lab, not "" - matches pivot_nested_crosstab()'s same
      # change (see its comment): once Base rows relocate to their own
      # section at the bottom (below), each one needs to carry its OWN
      # variable's label there, since it's no longer sitting directly under
      # that variable's own data rows to make the association visually
      # obvious.
      mutate(Variable = this_lab, stat_code = NA_character_, row_type = "base",
             o_cat = NA_character_, stat = NA_character_,
             # outcome_type is constant across a variable's whole block
             # (label/data rows and the base row all genuinely share one
             # type) - pulled from variable_rows rather than left to
             # bind_rows() auto-filling NA (base_wide never carried it),
             # since NA here isn't "correctly blank" the way it is for
             # o_cat/stat - there IS one right answer for this row.
             outcome_type = variable_rows$outcome_type[1])
    bind_rows(variable_rows, base_row)
  })

  # ---- attach one p-value column per predictor set ----
  # Two different display rules, chosen per row via outcome_type:
  #   - "categorical"/"numeric": one p-value shared across the whole
  #     (outcome, stat "type") block - shown once, on the first data row.
  #     cumsum(row_type == "data") only increments on data rows, so a
  #     categorical variable's label row (which shares its data rows'
  #     stat_type, since its `stat` was copied from them) doesn't consume
  #     "position 1" and push the real first data row to position 2 - the
  #     label row simply never matches data_row_number == 1 & row_type ==
  #     "data" at the same time.
  #   - "multicoded": each level has its own independent p-value (see the
  #     header note on why), so it's left in place on every data row rather
  #     than blanked after the first - there's no "block" to collapse here.
  # The join itself is keyed on (outcome, o_cat, stat_type) - see the
  # p_values comment in reshape_one_predictor_set() for why o_cat has to be
  # part of the key once a multicoded variable can reach this point. Each
  # set's p-value column is joined, blanked, and renamed independently in
  # its own pass, so one set's temporary "p_value" working column never
  # collides with another's.
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

    # Named "p_value (<predictor label>)" - underscore, not "p-value" - for
    # brevity and to match the underlying column's own name everywhere else
    # in this project (calc_stats() itself always calls it p_value).
    names(pivoted)[names(pivoted) == "p_value"] <- paste0("p_value (", p_lab_i, ")")
  }

  # Each set's p-value column is moved to sit right after that SET's own
  # level columns, rather than left wherever the loop above happened to
  # append it - which, since every set's column gets added in turn, meant
  # ALL predictor sets' p-value columns ended up clustered together at the
  # very end, after every set's level columns, not next to the data each
  # one actually tests. E.g. with sex and age_group: Total, sex: Female,
  # sex: Male, age_group: Old, age_group: Young, p_value (sex), p_value
  # (age_group) - reading "p_value (sex)" only after working through BOTH
  # sets' level columns is confusing; it belongs right after sex's own
  # columns instead. Uses each set's already-known level_order (from
  # set_results) to find where that set's columns end.
  for (i in seq_along(set_results)) {
    pval_col_i <- paste0("p_value (", p_labs[i], ")")
    if (pval_col_i %in% names(pivoted)) {
      last_level_col_i <- utils::tail(set_results[[i]]$level_order, 1)
      pivoted <- pivoted %>% relocate(all_of(pval_col_i), .after = all_of(last_level_col_i))
    }
  }

  # outcome/o_lab/o_cat were pure join/grouping scaffolding - needed up
  # through the full_join() that combines predictor sets, the base-row
  # lookup by o_lab, and the p-value join keyed on (outcome, o_cat,
  # stat_type) just above. outcome and o_cat are dropped here, same as
  # always: neither carries information the rest of the table doesn't
  # already have (o_cat duplicates Variable on every data row, and
  # duplicates stat_code on every row of a numeric variable - o_cat == stat
  # there, per calc_stat_engine()), and outcome is superseded by Variable +
  # row_type once o_cat's one real job here (disambiguating a multicoded
  # variable's p-value join) is done.
  #
  # o_lab is the one exception now (a change from this function's earlier
  # behaviour, made together with the Base-relocation feature below) - it's
  # kept all the way through to the returned table, because format_
  # crosstab() needs it downstream for block-boundary detection once a Base
  # row can no longer be assumed to sit directly under its own variable's
  # data. Matches pivot_nested_crosstab(), which keeps o_lab in scope for
  # exactly the same reason. pivot_summary()'s pivot_one_block() has no
  # equivalent need (format_summary() never had an inline-Base-row
  # assumption to begin with), so it's not a discrepancy worth reconciling.
  #
  # outcome_type is deliberately NOT dropped alongside them, unlike an
  # earlier version of this step. format_summary() (pivot_summary()'s
  # formatter) actually keeps FOUR metadata columns, not two: row_type,
  # stat_code, block_id, and is_variable_row - and is_variable_row is the
  # one that matters here. It's TRUE for a numeric variable's own data row
  # (bold it) and FALSE for a categorical/multicoded level row (don't) -
  # a distinction row_type alone can't make, since both are row_type ==
  # "data". outcome_type is what a future format_crosstab() would need to
  # rebuild that same distinction; dropping it would force falling back to
  # inferring numeric-vs-categorical from stat_code instead, which is
  # exactly the kind of fragile inference outcome_type replaced in
  # calc_stats() in the first place. block_id has no equivalent here (it's
  # unused even in pivot_summary() currently) and isn't added.
  #
  # o_lab, unlike outcome/o_cat, is NOT dropped here (a change from this
  # function's earlier behaviour) - format_crosstab()'s block-boundary
  # detection needs it once Base rows relocate to their own section below.
  # See that function's own new_block comment for why: the old proxy ("a
  # new block starts right after a base row") stops working once every base
  # row moves to the bottom, and o_lab is what replaces it - same fix,
  # same reasoning, as pivot_nested_crosstab()/format_nested_crosstab().
  pivoted <- pivoted %>% select(-outcome, -o_cat)

  # ---- Statistics column: tidy label for data rows, NA for label AND ----
  # ---- base rows --------------------------------------------------------
  # stat_code gets the same NA-on-label-row treatment as Statistics: it was
  # left holding a copy of the first level's stat code (e.g. "perc") purely
  # as a side effect of needing a real, non-NA `stat` value for
  # tidy_statistic_description()'s lookup to run cleanly on every row -
  # stat_code is split off from `stat` before that lookup happens, so it
  # doesn't actually need to keep that copy afterwards. Harmless in practice
  # (a label row's Estimate/level columns are already NA, and
  # format_statistic() returns NA for those regardless of stat_code), but
  # wrong to display. outcome_type does NOT need the same treatment - unlike
  # o_cat/stat_code, it's constant for every row of one variable's block
  # (label, data, AND - via the earlier full_join/bind_rows - base row all
  # genuinely share one outcome_type), so there's no "leftover from the
  # first row" value to fix.
  #
  # row_type == "base" now blanks to NA, not "Base" - same change, same
  # reasoning, as pivot_nested_crosstab(): the relocated section (below)
  # gets one "Sample sizes" header for the whole section, so repeating
  # "Base" on every individual row under it would be redundant. Note (per
  # the same correction already made in the nested file, caught there by a
  # real test run, applied here proactively rather than re-discovered): the
  # blanking isn't what makes format_crosstab()'s later Variable+Statistics
  # merge_cells() call safe - huxtable copies the anchor cell's content into
  # the merged cell rather than preserving whatever was already there, so
  # the merge would overwrite this regardless. Blanking is still correct in
  # its own right, independent of the merge.
  pivoted <- pivoted %>%
    tidy_statistic_description() %>%
    mutate(Statistics = case_when(
      row_type == "label" ~ NA_character_,
      row_type == "base"  ~ NA_character_,
      TRUE ~ Statistics
    ),
    stat_code = if_else(row_type == "label", NA_character_, stat_code)) %>%
    relocate(Variable, Statistics, .before = 1) %>%
    # All internal/metadata columns grouped into one block at the very end,
    # after every real data column (levels, conf, p-values) - not scattered
    # the way they fell out naturally otherwise (row_type right after
    # Variable, stat_code after the level columns, p-value columns tacked on
    # after THAT). Matches where pivot_summary()'s equivalent columns
    # (row_type, stat_code, block_id, is_variable_row) already sit - after
    # Variable/stat/Estimate/SE/CI/Base, not mixed in among them. o_lab
    # joins this group too now, for the same reason it's no longer dropped
    # above - pure metadata, not a real data column.
    relocate(row_type, stat_code, outcome_type, o_lab, .after = dplyr::last_col())

  # ---- relocate every variable's Base row into one consolidated "Sample ----
  # ---- sizes" section at the bottom, rather than directly under each -----
  # ---- variable's own data (Joe's call, applied here the same way it was --
  # ---- for the nested table - see pivot_nested_crosstab()'s longer note ----
  # ---- on this for the full reasoning). Always-applies, not a toggle - ----
  # ---- this is a layout choice, not something that changes what's ---------
  # ---- computed, matching the same principle already settled for the -----
  # ---- nested table. -------------------------------------------------------
  #
  # base_header's blanking covers all_level_cols, their "__conf" and
  # "__sigdiff" siblings (present when conf/pairwise were requested), AND
  # every p_value column - plural here in a way the nested table never had
  # to account for, since a flat crosstab can have more than one predictor
  # set side by side, each with its own "p_value (<p_lab>)" column.
  # starts_with("p_value") catches all of them regardless of how many sets
  # are present, rather than needing to know their exact names.
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

  # ---- legend: one combined (level_col, sig_letter) table across every ----
  # ---- predictor set present, or NULL if none had sig_letter at all --------
  # Kept as a SEPARATE returned element rather than inserted as a row into
  # `pivoted` here - see reshape_one_predictor_set()'s header note on legend
  # for why (type-coercion risk if a character letter value lands in a
  # still-numeric estimate column before format_crosstab()'s own digit
  # formatting has run). format_crosstab() builds the actual display row
  # from this, once its columns are already character.
  legend <- map(set_results, "legend") %>% compact() %>% bind_rows()
  if (nrow(legend) == 0) legend <- NULL

  # predictor_sets is returned as a NAMED vector - cross_break (the raw
  # predictor variable name, e.g. "sex") as the names, p_lab (the display
  # label used to prefix level columns, e.g. "Sex" if that's the var_label)
  # as the values - not just the bare cross_break vector this used to be.
  # format_crosstab() needs p_lab to build a spanning header over each
  # predictor set's level columns, and multi-set column names already carry
  # it (parseable from the "<p_lab>: <level>" prefix), but a SINGLE
  # predictor set's level columns stay unprefixed (e.g. bare "Male"/
  # "Female" - see reshape_one_predictor_set()'s `prefix` argument), so
  # there'd be nowhere else to recover the predictor's label from in that
  # case without this.
  list(pivoted, bases, all_level_cols, setNames(p_labs, predictor_sets), conf_type, legend)
}


# ---- format_crosstab: style pivot_crosstab() output as a huxtable ------------
#
# Takes pivot_crosstab()'s own output (list(pivoted, bases, all_level_cols,
# predictor_sets, conf_type)) and returns a styled huxtable, the crosstab
# equivalent of format_summary(). Reuses format_statistic()/format_ci_string()
# from pivot_summary.R directly - same digit-formatting rules, keyed the same
# way off stat_code - so a percentage/mean/count reads identically in either
# table type.
#
# Three things are genuinely new here, without a format_summary() precedent
# to lean on:
#
#   1. SE/CI display: chosen (directly, not inferred) to be a separate row
#      under the estimate row, not concatenated into the same cell - the
#      other option pivot_crosstab()'s header docs left open. Implemented
#      by inserting one extra physical row after every data row that has a
#      non-NA "<level>__conf" value on at least one level column, holding
#      those same level columns' formatted conf values instead of estimates,
#      with Variable blank and Statistics reading "SE" or "95% CI"
#      (conf_type says which). A numeric variable shown under more than one
#      statistic gets one conf row per statistic row, same as it gets one
#      estimate row per statistic.
#
#   2. Spanning headers: one header row above the normal column-name row,
#      with each predictor set's level columns merged under its p_lab (e.g.
#      "Sex" spanning "Male"/"Female"). Built from predictor_sets (the named
#      vector pivot_crosstab() now returns specifically for this) rather
#      than needing anything extra threaded through the data itself - in
#      the single-predictor-set case every level column except "Total"
#      belongs to that one set; with multiple sets, membership is read
#      straight off each column's "<p_lab>: " prefix. This is the part of
#      this function I'd check most carefully against real output - I don't
#      have R to run insert_row()/merge_cells() against and confirm the row/
#      column arithmetic actually lines up.
#
#   3. is_variable_row has no equivalent column here (pivot_crosstab() never
#      built one) - rebuilt inline from row_type == "label" OR (row_type ==
#      "data" & outcome_type == "numeric"), which is exactly why
#      outcome_type is kept in pivot_crosstab()'s output rather than dropped
#      alongside outcome/o_lab/o_cat (see that function's comments).
#
# Two more things, added for calc_stats(pairwise = TRUE) support (Joe's
# design, discussed directly - see calc_stats.R's assign_sig_letters() for
# the matching change on the data side):
#
#   4. sig_diff shown as its own inserted row, same mechanic as SE/CI (point
#      1 above) - reads "<level>__sigdiff" instead of "<level>__conf",
#      labelled "Sig. diff". SE/CI is suppressed entirely whenever sig_diff
#      is present (has_pairwise), rather than shown alongside it - the
#      letters already convey which levels differ, so showing both would be
#      redundant. A row only gets a sigdiff row inserted if at least one
#      level column has a real (non-NA, non-empty-string) value - a row that
#      WAS tested but found nothing significant doesn't get an empty,
#      uninformative row of its own.
#
#   5. A "legend" row - literally the first row of the whole table, one
#      letter per level column, built from pivot_result[[6]] (a separate
#      long-format lookup pivot_crosstab() returns rather than a row already
#      baked into pivot_result[[1]] - see reshape_one_predictor_set()'s own
#      header note on why: inserting a raw letter string into a still-
#      numeric level column before this function's own digit-formatting has
#      run would force that WHOLE column to character via bind_rows()'s type
#      coercion, corrupting every other row's formatting). Built here, after
#      the digit-formatting loop, so the columns it's landing in are already
#      character. Styled italic with a light grey background to read as a
#      lookup aid rather than a data row; Variable + Statistics merged into
#      one wide cell ("Column reference"), same treatment as the Sample
#      sizes section header gets.
#
# Row-type bookkeeping: pivot_crosstab() already produces row_type %in%
# c("label", "data", "base"); this function adds "conf"/"sigdiff" for the
# rows it inserts, and "legend" for the row it prepends. Variable-name block
# boundaries (for both the "extra top padding at the start of a variable"
# rule and the "merge a numeric variable's repeated Variable text" rule,
# both carried over from format_summary()) are derived from o_lab, which
# pivot_crosstab() now keeps
# (a change from this function's original behaviour, made together with the
# Base-relocation feature below): a categorical variable's own label row
# always starts a new block; a numeric variable (no label row of its own)
# starts one whenever o_lab just changed from the previous row. This
# replaced an earlier, simpler rule - "a new block starts at row 1, or
# immediately after a base row" - that relied on every variable's Base row
# sitting directly beneath its own data, which stopped being true once every
# Base row relocates to one consolidated section at the bottom instead (same
# fix, same reasoning, as format_nested_crosstab()).
format_pvalue <- function(p) {
  case_when(
    is.na(p)  ~ NA_character_,
    p < 0.001 ~ "<0.001",
    TRUE      ~ formatC(p, digits = 3, format = "f")
  )
}

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

  # calc_stats(pairwise = TRUE) drives both of these - sig_diff shown as its
  # own inserted row (below) is the point of that setting, and SE/CI would
  # be redundant clutter alongside it (the letters already convey which
  # levels differ), so it's suppressed entirely rather than shown alongside.
  has_pairwise <- length(sigdiff_cols) > 0

  # ---- digit formatting, as plain text, before the huxtable exists ----
  # (same approach and same reasoning as format_summary(): done in plain R
  # so format_statistic()/format_ci_string() stay testable on their own,
  # without needing a real huxtable object.)
  #
  # The base row is a special case that pivot_summary() never had to handle
  # this way: there, Base is its own dedicated column, formatted once with
  # a fixed digits=0/comma-separated rule, entirely separate from Estimate's
  # stat_code-keyed formatting. Here, the base row's counts live in the SAME
  # level columns as every other row's estimates - so running the whole
  # column through format_statistic() would key the base row's formatting
  # off its own stat_code too, which is NA on a base row (blanked earlier),
  # falling through to format_statistic()'s generic 6-decimal-place
  # fallback (e.g. "6.000000" instead of "6"). Base rows are branched to the
  # same digits=0/comma-separated rule pivot_summary() uses instead.
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

  # ---- insert one extra "conf" row under every data row that has a ----
  # ---- non-NA conf value on at least one level column - suppressed -----
  # ---- entirely when pairwise sig_diff is being shown instead (see ------
  # ---- has_pairwise above) ----------------------------------------------
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
      # A conf row doesn't carry a p-value of its own - blanked explicitly
      # rather than left holding whatever the estimate row's p-value column
      # had (which would otherwise just get copied along by `row[i, ]`).
      for (pc in pval_cols) conf_row[[pc]] <- NA_character_

      bind_rows(row %>% select(-all_of(conf_cols)),
                conf_row %>% select(-all_of(conf_cols)))
    })
  } else {
    data <- data %>% select(-any_of(conf_cols))
  }

  # ---- insert one extra "sigdiff" row under every data row that has a ----
  # ---- real (non-NA, non-empty) sig_diff value on at least one level -----
  # ---- column - same mechanic as the conf row above, reading from --------
  # ---- "__sigdiff" instead of "__conf" ------------------------------------
  # Empty string, not NA, is how pairwise_test_one_group() marks "tested,
  # nothing came out significant" (see calc_stats.R) - excluded here too, so
  # a row that WAS tested but found no real differences doesn't get an
  # empty, uninformative row inserted under it.
  if (has_pairwise) {
    data <- map_df(seq_len(nrow(data)), function(i) {
      row <- data[i, , drop = FALSE]
      diff_values <- unlist(row[, sigdiff_cols, drop = FALSE])
      has_diff <- row$row_type == "data" &&
        any(!is.na(diff_values) & diff_values != "")

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

  # ---- insert the legend row - literally the first row of the table, ----
  # ---- ahead of every variable's own data ---------------------------------
  # Built here, not in pivot_crosstab(), specifically so it lands AFTER the
  # digit-formatting loop above has already turned every level column to
  # character - see reshape_one_predictor_set()'s header note on legend for
  # why inserting a raw letter string any earlier would risk coercing an
  # otherwise-numeric column to character via bind_rows(), corrupting every
  # other row's formatting downstream.
  #
  # o_lab/outcome_type are copied from the table's own first row rather than
  # left NA - purely defensive: new_block (below) already forces its own
  # TRUE for the row right after a legend row via the explicit
  # lag(row_type) == "legend" clause, so this isn't strictly load-bearing,
  # but avoids relying on R's `TRUE | NA` short-circuit (itself correct, but
  # one more thing to have to reason about) to keep block_id's cumsum() from
  # ever seeing an NA in new_block.
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

  # ---- block boundaries, from row_type transitions (see header note) ----
  # Rebuilt from o_lab, not from "row right after a base row" - that proxy
  # (still visible in git history/the original header note above) relied on
  # a base row always sitting directly under its own variable's data, which
  # stopped being true once Base rows relocate to their own section at the
  # bottom (below). It would have silently broken for every numeric
  # variable transition (the only case that ever needed it - a categorical
  # variable's label row already triggers new_block on its own regardless).
  # Same fix, same reasoning, as format_nested_crosstab(): a categorical
  # label row always starts a block; a numeric variable's data row starts
  # one only when o_lab just changed from the previous row, so two adjacent
  # numeric variables are still told apart even with no base row - or
  # anything else - between them anymore. The relocated Base section still
  # gets exactly one gap, from its row_type == "label" header, via the
  # first clause; individual base rows never claim their own.
  # row_type == "legend" (if present) is always its own block start too - a
  # forced TRUE rather than something derived from o_lab, since it's row 1
  # and doesn't belong to any variable. The row immediately after it gets
  # its own forced TRUE too (lag(row_type) == "legend"), rather than relying
  # on the o_lab-mismatch clause below to fire correctly for it - needed
  # because that clause only ever triggers for a NUMERIC variable with no
  # label row of its own; a categorical variable's label row already
  # triggers new_block via the first clause regardless of what precedes it.
  new_block <- data$row_type %in% c("label", "legend") |
    dplyr::lag(data$row_type, default = "") == "legend" |
    (data$row_type == "data" & data$outcome_type == "numeric" &
       dplyr::lag(data$o_lab, default = "") != data$o_lab)
  block_id <- cumsum(new_block)

  # ---- pull metadata out before building the huxtable ----
  row_type     <- data$row_type
  outcome_type <- data$outcome_type

  ht <- data %>% select(-row_type, -stat_code, -outcome_type, -o_lab) %>%
    huxtable::as_hux(add_colnames = TRUE)

  header_offset <- 1L   # the as_hux(add_colnames = TRUE) column-name row

  variable_col  <- which(names(ht) == "Variable")
  level_col_idx <- which(names(ht) %in% all_level_cols)

  # ---- spanning header row over each predictor set's level columns ----
  # (and its own p_value column, if it has one - see below)
  spans <- purrr::imap(predictor_sets, function(p_lab, cb) {
    level_cols <- if (length(predictor_sets) == 1) {
      setdiff(all_level_cols, "Total")
    } else {
      all_level_cols[stringr::str_starts(all_level_cols, paste0(p_lab, ": "))]
    }
    # The set's p_value column, if calc_stats() was run with pval = TRUE,
    # is folded into the same span rather than left uncovered - it already
    # sits directly after this set's own level columns (see the p_value
    # column relocate() above), so the span just needs to run one column
    # further right; nothing here has to search for it or reorder anything.
    # Its DISPLAYED header text gets its own stripping rule below (not the
    # same "<p_lab>: " prefix rule the level columns use, since a p_value
    # column's own text - "p_value (sex)" - never starts with "sex: ") - the
    # "(sex)" part is just as redundant as the level columns' own prefix
    # once the span above already reads "sex".
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

      # Strip the "<p_lab>: " prefix from the DISPLAYED column-name row for
      # this set's LEVEL columns - now that the span above already reads
      # "sex", a column header repeating it ("sex: Female", "sex: Male") is
      # redundant and just harder to read; "Female"/"Male" alone is enough.
      # The p_value column gets its own rule right below it, for the same
      # reason but a different text pattern ("p_value (sex)" -> "p_value",
      # not a prefix strip). Only the header row's displayed TEXT changes
      # here - names(ht) (used for every column lookup in this function,
      # and the reason the prefix/suffix exists in pivot_crosstab() at all)
      # stays as-is, since that's still doing real work: keeping two
      # different sets that happen to share a level name (e.g. both having
      # a "Yes" column), or both requesting pval = TRUE, from colliding. A
      # no-op for the single-predictor-set case's level columns, which were
      # never prefixed to begin with (str_remove() on a non-matching prefix
      # just returns the string unchanged) - but the p_value stripping still
      # applies there too, a single predictor set gets a spanning header
      # same as a multi-set table does (see the header note above spans).
      prefix_i <- paste0(s$p_lab, ": ")
      pval_col_i <- paste0("p_value (", s$p_lab, ")")
      bare_labels <- dplyr::case_when(
        stringr::str_starts(s$cols, stringr::fixed(prefix_i)) ~ stringr::str_sub(s$cols, nchar(prefix_i) + 1),
        s$cols == pval_col_i ~ "p_value",
        TRUE ~ s$cols
      )
      ht[header_offset, col_idx] <- bare_labels
    }
    # Bottom border under row 1 covers every column that's actually part of
    # a span (level columns AND each set's own p_value column now that it's
    # folded into the span above) - using all_level_cols alone here would
    # leave a p_value column's border missing, breaking the visual "box"
    # around what's now a wider spanning header.
    spanned_cols <- unique(unlist(purrr::map(spans, "cols")))
    ht <- ht %>%
      huxtable::set_align(1, huxtable::everywhere, "center") %>%
      huxtable::set_bold(1, huxtable::everywhere, TRUE) %>%
      huxtable::set_bottom_border(1, which(names(ht) %in% spanned_cols),
                                   huxtable::brdr(1, "solid", "grey40"))
  }

  no_of_rows   <- nrow(ht)                     # includes header row(s)
  data_rows    <- which(row_type == "data") + header_offset
  conf_rows    <- which(row_type == "conf") + header_offset
  sigdiff_rows <- which(row_type == "sigdiff") + header_offset

  # ---- NA formatting: blank by default, "-" for a genuinely missing ----
  # ---- statistic on a data, conf, or sigdiff row ------------------------
  # sigdiff_rows included here for a genuinely missing (NA) comparison - not
  # for an empty-string one ("tested, nothing significant"), which already
  # renders as blank text on its own without needing set_na_string() at all
  # (it's a real zero-length string, not an NA huxtable has to substitute
  # for).
  ht <- ht %>%
    huxtable::set_na_string(value = "") %>%
    huxtable::set_na_string(row = c(data_rows, conf_rows, sigdiff_rows), col = level_col_idx, value = "-")

  # ---- alignment: Variable/Statistics left, everything else right ----
  # Right-alignment is scoped to header_offset:no_of_rows, NOT "everywhere" -
  # same fix, same reasoning, as format_nested_crosstab(). When a spanning
  # header row exists (multiple predictor sets - see the spans block above),
  # row 1 was already explicitly centered there; "everywhere" here would
  # immediately overwrite that center back to right, which is exactly the
  # bug Joe flagged in the nested table (there, rows 1:3; here, just row 1,
  # and only when a span row exists at all - a single-predictor-set table
  # has no row 1 to protect, so header_offset is 1 and this is a no-op).
  ht <- ht %>%
    huxtable::set_align(huxtable::everywhere, variable_col, "left") %>%
    huxtable::set_align(huxtable::everywhere, which(names(ht) == "Statistics"), "left") %>%
    huxtable::set_align(header_offset:no_of_rows,
                         which(!names(ht) %in% c("Variable", "Statistics")), "right")

  # ---- bold variable name rows ----
  # is_variable_row rebuilt here rather than carried as a column - see
  # header note. TRUE for a categorical/multicoded label row, and for a
  # numeric variable's own data row; FALSE for a categorical/multicoded
  # level row, a conf row, and a base row.
  is_variable_row <- row_type == "label" | (row_type == "data" & outcome_type == "numeric")
  variable_name_rows <- which(is_variable_row) + header_offset
  ht <- huxtable::set_bold(ht, row = variable_name_rows, col = variable_col, value = TRUE)

  # ---- merge repeated Variable text within a numeric variable's block ----
  # (its data rows AND their inserted conf rows all read Variable == "" or
  # the repeated variable name - see header note for why row_type == "data"
  # rows for a numeric variable, plus any "conf" rows immediately following
  # them, all merge into one cell, while everything else - categorical level
  # rows, base rows - keeps its own separate cell, merged or not).
  merge_group <- dplyr::if_else(
    row_type %in% c("data", "conf") & outcome_type == "numeric",
    paste0("numeric_", block_id),
    paste0("nomerge_", seq_along(row_type))
  )
  run_lengths <- rle(merge_group)$lengths
  run_starts  <- cumsum(c(1, utils::head(run_lengths, -1)))

  # No follow-up loop blanking ht[r, variable_col] here (an earlier version
  # of this code had one) - removed as dead code, confirmed by a real test
  # run against actual huxtable output, not assumed: per huxtable's own
  # docs (spans.Rd), setting rowspan() COPIES the anchor cell's content into
  # every cell it covers, and extracting a covered cell (via `[[`, same as
  # printing) always resolves back to that anchor content regardless of
  # what's assigned to the covered cell afterwards. So a blanking loop here
  # would run and change nothing observable - the covered rows already read
  # as the anchor's text ("age", etc.) on access, and were always DISPLAYED
  # as blank anyway since rowspan collapses them visually either way. Same
  # dead pattern removed from pivot_summary.R and pivot_nested_crosstab.R.
  for (i in seq_along(run_starts)) {
    if (run_lengths[i] > 1) {
      start_row <- run_starts[i] + header_offset
      span_len  <- run_lengths[i]
      ht <- huxtable::set_rowspan(ht, row = start_row, col = variable_col, value = span_len)
    }
  }

  # ---- merge Variable + Statistics into one wide cell within the Sample ----
  # ---- sizes section - same fix, same reasoning, as format_nested_ ----
  # ---- crosstab(). Statistics is blank there anyway on every row (the ----
  # ---- section header via row_type == "label", each individual row via ----
  # ---- pivot_crosstab()'s case_when - see its comment), so folding it ----
  # ---- into Variable gives the variable label the room a normally-narrow ----
  # ---- Statistics column would otherwise waste. ----
  # ---- ----
  # ---- Per huxtable's own docs (confirmed there for the nested table, ----
  # ---- not re-derived here): merging COPIES the anchor (top-left, i.e. ----
  # ---- Variable) cell's content into the other cell, rather than ----
  # ---- blanking it - invisible at the display level (only the anchor's ----
  # ---- position is ever shown once merged), but worth knowing if this ----
  # ---- table's Statistics column is ever inspected programmatically. ----
  base_data_rows <- which(row_type == "base")
  if (length(base_data_rows) > 0) {
    statistics_col <- which(names(ht) == "Statistics")
    # The row directly above the first base row - guaranteed to be the
    # "Sample sizes" section header, since relocation (pivot_crosstab())
    # always places exactly one row_type == "label" row immediately before
    # the whole relocated block, nothing else.
    section_header_row <- min(base_data_rows) - 1
    merge_rows <- c(section_header_row, base_data_rows) + header_offset
    for (r in merge_rows) {
      ht <- huxtable::merge_cells(ht, r, range(c(variable_col, statistics_col)))
    }
  }

  # ---- legend row: merge Variable + Statistics into one wide cell, ----
  # ---- same reasoning as the Sample sizes section header just above -----
  # ---- (Statistics is blank there too, so folding it into Variable gives ----
  # ---- "Column reference" more room), and mark it visually distinct from ----
  # ---- a real data row (italic, light background) since it's a lookup ----
  # ---- aid, not a value from the data. --------------------------------
  legend_hux_row <- which(row_type == "legend") + header_offset
  if (length(legend_hux_row) > 0) {
    statistics_col <- which(names(ht) == "Statistics")
    ht <- huxtable::merge_cells(ht, legend_hux_row, range(c(variable_col, statistics_col)))
    ht <- huxtable::set_italic(ht, legend_hux_row, huxtable::everywhere, TRUE)
    # Horizontal alignment stays right (the general "header_offset:no_of_rows
    # -> right" rule already covers it, correctly, further above) - no
    # override needed here. Vertical alignment (top of cell, not centered)
    # and the extra bottom padding are set further down instead, AFTER the
    # "minimal styling" block's set_valign("middle")/set_all_padding(1) -
    # setting them here would just get silently overwritten by those two
    # calls, the same reason block_start_rows' top-padding already waits
    # until after that block too.
  }

  # ---- alternating background, one row one colour, next row the other ----
  # No separate background override for the legend row (an earlier version
  # of this set one - removed per Joe's review: too dark relative to the
  # rest of the table) - it just takes whichever stripe colour stripe_rows()
  # assigns it, like any other row.
  ht <- huxtable::stripe_rows(ht, stripe1 = "#f5f7fa", stripe2 = "#ffffff")

  # ---- minimal styling: small font, tight padding, light grey borders ----
  # set_bold()/set_background_color() cover ALL header rows (seq_len(
  # header_offset)), not just the last one - with a spanning header row
  # present, that's rows 1:2, not just row 2. Row 1 already got its own
  # bold/align/border treatment in the spans block above, but not a
  # background colour - left to whatever stripe_rows() (just above) happened
  # to assign it, purely by virtue of its position in the alternating
  # sequence, which is wrong: a header row should look like a header row,
  # not accidentally match some ordinary data row's stripe colour.
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

  # ---- extra top padding at the start of each variable's block ----
  block_start_rows <- which(new_block) + header_offset
  ht <- huxtable::set_top_padding(ht, row = block_start_rows, col = huxtable::everywhere, value = 6)

  # ---- legend row: top-aligned within its cell (not vertically centered,
  # ---- the table-wide default), with extra bottom padding for separation
  # ---- from the content below it - all three set here, after set_valign(
  # ---- "middle")/set_all_padding(1) above, so they aren't immediately
  # ---- overwritten by those table-wide calls. ----
  if (length(legend_hux_row) > 0) {
    ht <- ht %>%
      huxtable::set_valign(legend_hux_row, huxtable::everywhere, "top") %>%
      # top_padding reset to 1 (not left at the 6 the block_start_rows step
      # just above just gave it, since the legend row IS a block start) -
      # with equal top/bottom padding, "top"-aligned text sits in the
      # middle of the available space in practice (the symmetric padding
      # cancels out the asymmetry valign is supposed to create) and reads
      # as no different from "middle". Tight on top, generous on the
      # bottom is what actually pushes the text visibly to the top edge.
      huxtable::set_top_padding(legend_hux_row, huxtable::everywhere, 1) %>%
      huxtable::set_bottom_padding(legend_hux_row, huxtable::everywhere, 6)
  }

  # ---- footnotes (unchanged from format_summary()) ----
  if (all(unlist(bases) != "NA")) {
    for (i in seq_along(bases)) {
      ht <- ht %>% huxtable::add_footnote(bases[[i]], border = NULL)
    }
  }

  ht
}
