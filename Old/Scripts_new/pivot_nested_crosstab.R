# =============================================================================
# pivot_nested_crosstab() / format_nested_crosstab() — a crosstab nested one
# level deeper than pivot_crosstab(): one outer predictor (e.g. sex) with one
# inner predictor (e.g. age_group) nested inside each of its levels, giving a
# 4-row header block per column:
#
#   row 1: outer variable's label,  spanning EVERY column
#   row 2: outer variable's levels, each spanning its own inner columns
#   row 3: inner variable's label,  repeated once per outer level
#   row 4: inner variable's levels, one per column, repeating under each
#          outer level
#
# Deliberately scoped to exactly one nesting level (2 variables: outer +
# inner) and exactly one nested set per table - no side-by-side flat sets
# alongside it (that's pivot_crosstab()'s job), no arbitrary N-level nesting.
# A NEW function pair, not an extension of pivot_crosstab()/format_crosstab():
# those two already juggle multiple side-by-side sets, multi-statistic
# merging, conf rows, and p-values - bolting a second header tier onto that
# would multiply the combinatorics rather than add one clean new dimension.
#
# The raw data side of this already existed before this file: calc_stats()
# has supported `predictors = list(c(outer_var, inner_var))` since the
# original nested-predictor pass (see standardise_names() in calc_stats.R) -
# it already returns predictor1/p_lab1/p_cat1 (outer) and predictor2/p_lab2/
# p_cat2 (inner) columns, one row per (outcome, stat, outer level, inner
# level) combination. pivot_crosstab() itself explicitly drops those columns
# and refuses to handle them ("Nested combined predictors are out of scope
# for this function" - see its own header note); this file is what actually
# reshapes them into a table.
#
# p-values: one per OUTER LEVEL, not one for the whole nested set. Joe's
# explicit design call - "among women, is there an association between age
# and this outcome" - i.e. subset to one outer level, then run exactly the
# test a flat predictor already gets, against the inner variable, on that
# subset. calc_stats.R's nested_pvalues() does the actual computation (it
# needs the raw respondent-level data, which this file never sees - only
# calc_stats() output); this file's job is just surfacing what's already
# attached to `data$p_value` for nested rows, one column per outer level
# (e.g. "p_value (age_group | Male)", "p_value (age_group | Female)"),
# shown once per outcome block the same "first data row only" way a flat
# crosstab's p-value column already works - except for a multicoded
# outcome, which keeps its own independent p-value on every level row, same
# reasoning as pivot_crosstab()'s multicode handling (see that file's header
# note - not re-derived here since the mechanism is identical).
#
# Rectangularity: if some (outer level, inner level) combination has zero
# respondents in the data, calc_stats() simply never produces a row for it -
# group_by()/summarise() don't invent empty groups. Left alone, that would
# silently narrow just that one outer level's span by one column, breaking
# the visual "box" the 4-row header depends on. Forced rectangular instead,
# via a full (outer, inner) grid inside reshape_nested_predictor_set() below,
# so a missing combination becomes a genuinely blank column rather than an
# absent one.
#
# This turned out to have a second, sharper-edged case a real test run
# caught: a categorical/multicoded outcome can have one COMBINATION fully
# populated but still be missing a row for one o_cat LEVEL within it (e.g.
# zero "Yes" respondents among Female/Old, when Female/Old itself has
# respondents) - group_by() on two factor columns together doesn't invent
# that unobserved level the way grouping by a single factor does. estimate
# genuinely needs completing for that case (a missing level needs a blank
# cell); base does NOT - it's a property of the (outer, inner) group, not of
# any one o_cat level, and completing it the same way as estimate manufactures
# a phantom NA that collides with the group's real base once o_cat drops out
# of the key. reshape_nested_predictor_set() computes base from the (outer,
# inner) grid directly, before that o_cat-crossed completion ever runs, so
# the two cases can't cross-contaminate each other.
#
# I haven't been able to run any of this myself - no R available in this
# environment. See test_pivot_nested_crosstab.R for what's covered and how
# confident I am in each part - the header-row insertion order (three
# insert_row(after = 0) calls, populated in a specific sequence so they land
# in the right final rows) is the piece I'd check most carefully by hand.
# =============================================================================

library(tidyverse)
source(here::here("Scripts_new", "calc_stats.R"))
source(here::here("Scripts_new", "pivot_summary.R"))    # reuse tidy_statistic_description(), format_statistic()
source(here::here("Scripts_new", "pivot_crosstab.R"))    # reuse stat_type_of() [unused here but kept for parity], format_statistic patterns


# ---- reshape_nested_predictor_set: the outer x inner column reshape -------
#
# `rows_for_set` is already filtered to the single nested cross_break's rows
# (Total excluded - see pivot_nested_crosstab() below). Builds one composite
# column per (outer level, inner level) combination - "Male: Young", "Male:
# Old", "Female: Young", "Female: Old" - since the bare inner level alone
# ("Young") isn't unique across outer groups and can't be a column name by
# itself. header_info carries the outer/inner label and level for each of
# those composite columns, in final display order, so format_nested_
# crosstab() never has to re-parse the composite name back apart.
reshape_nested_predictor_set <- function(rows_for_set) {

  # Level order locked in from the data's own first-appearance order (mirrors
  # reshape_one_predictor_set()'s level_order) - captured BEFORE any
  # completion runs, since completion can introduce/reorder rows and there'd
  # be nothing left to infer an order from afterwards.
  outer_order <- rows_for_set %>% distinct(p_cat1) %>% pull(p_cat1) %>% fct_inorder() %>% levels()
  inner_order <- rows_for_set %>% distinct(p_cat2) %>% pull(p_cat2) %>% fct_inorder() %>% levels()

  outer_label <- rows_for_set$p_lab1[1]
  inner_label <- rows_for_set$p_lab2[1]

  # The rectangular (outer, inner) grid - the single source of truth for
  # column order and header text, and (see below) built BEFORE base is ever
  # touched, specifically to keep base away from a real bug a test run
  # caught: for a categorical/multicoded outcome, if one o_cat level had
  # ZERO occurrences within an otherwise-populated (outer, inner) group (e.g.
  # zero "Yes" among Female/Old respondents), calc_stats() never produces a
  # row for that (o_cat, p_cat1, p_cat2) combination at all - group_by() on
  # two factor columns together doesn't invent unobserved combinations the
  # way grouping by a single factor does. Completing THAT gap (needed for
  # estimate_wide - a missing o_cat level legitimately needs a blank cell)
  # manufactures a new row with base = NA, since there's nothing to fill it
  # from - which then collided with the SAME group's real, non-NA base the
  # moment o_cat was dropped from the key, the actual "values not uniquely
  # identified" crash a real test run hit. Keeping base entirely separate
  # from that o_cat-crossed completion (below) avoids the collision outright
  # rather than papering over it with an arbitrary first() pick.
  grid <- tidyr::crossing(p_cat1 = factor(outer_order, levels = outer_order),
                           p_cat2 = factor(inner_order, levels = inner_order)) %>%
    arrange(p_cat1, p_cat2) %>%
    mutate(col_key = paste0(as.character(p_cat1), ": ", as.character(p_cat2)))

  col_order <- grid$col_key

  header_info <- grid %>%
    transmute(column_name = col_key,
              outer_label = outer_label, outer_level = as.character(p_cat1),
              inner_label = inner_label, inner_level = as.character(p_cat2))

  # ---- base: from the (outcome, outer, inner) grouping directly, never ----
  # ---- crossed with o_cat - see the header note above -----------------------
  # A (p_cat1, p_cat2) combination that's genuinely missing (zero
  # respondents overall, not just zero occurrences of one o_cat level) still
  # ends up NA here via the left_join() below - that's the real
  # rectangularity gap this file is meant to handle (see test 2 in
  # test_pivot_nested_crosstab.R); a combination that merely has one o_cat
  # level with zero occurrences never reaches this computation at all, so it
  # can't corrupt it.
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

  # ---- estimate/conf: THIS is where the o_cat-crossed completion belongs -
  # ---- a missing o_cat level genuinely needs a blank estimate cell, unlike base ----
  completed <- rows_for_set %>%
    tidyr::complete(tidyr::nesting(outcome, o_lab, outcome_type, stat, o_cat), p_cat1, p_cat2) %>%
    mutate(p_cat1 = factor(p_cat1, levels = outer_order),
           p_cat2 = factor(p_cat2, levels = inner_order)) %>%
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

  # ---- sig_diff: exact parallel to conf_wide above - one column per ----
  # ---- composite (outer, inner) column, "__sigdiff" suffix, read per row ----
  # ---- the same way SE/CI already is. Ported from pivot_crosstab.R's ----
  # ---- reshape_one_predictor_set() - see that file for the design behind ----
  # ---- the mechanism itself (not re-derived here). ----
  if ("sig_diff" %in% names(completed)) {
    sigdiff_wide <- completed %>%
      select(outcome, o_lab, o_cat, stat, outcome_type, col_key, value = sig_diff) %>%
      pivot_wider(names_from = col_key, values_from = value) %>%
      select(outcome, o_lab, o_cat, stat, outcome_type, all_of(col_order)) %>%
      rename_with(~ paste0(.x, "__sigdiff"), all_of(col_order))
    estimate_wide <- estimate_wide %>%
      left_join(sigdiff_wide, by = c("outcome", "o_lab", "o_cat", "stat", "outcome_type"))
  }

  # ---- legend: sig_letter is constant per (p_cat1, p_cat2) - see
  # calc_stats.R's assign_sig_letters() header note (dropped `outcome` from
  # its own key entirely so a letter never varies across outcomes) - so this
  # is built straight from rows_for_set, not from the o_cat-crossed
  # `completed` grid, exactly mirroring pivot_crosstab()'s own legend
  # construction. Keyed on the SAME col_key string the grid/col_order use
  # ("Male: Young"), so format_nested_crosstab() can look it up directly
  # against col_order with no re-parsing.
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


pivot_nested_crosstab <- function(data) {

  if (!all(c("p_cat2", "p_lab2", "predictor2") %in% names(data))) {
    stop("pivot_nested_crosstab() needs a nested predictor set - call calc_stats() with ",
         "predictors = list(c(<outer variable>, <inner variable>)). Use pivot_crosstab() ",
         "instead for a flat (single-variable) predictor.")
  }

  bases <- data %>% pull(base_description) %>% unique() %>% paste0(collapse = " X ")
  if (any(!is.na(bases))) bases <- bases %>% prepare_base_for_table()
  data <- data %>% select(-base_description)

  # calc_stats() always includes estimate_se, filled with "-" when conf
  # wasn't requested - same fix pivot_summary()/pivot_crosstab() both need.
  if ("estimate_se" %in% names(data) && all(data$estimate_se == "-")) {
    data <- data %>% select(-estimate_se)
  }

  conf_type <- if ("estimate_ci" %in% names(data)) "ci" else if ("estimate_se" %in% names(data)) "se" else NA_character_

  # Total rows (p_cat1 == "Total") aren't shown as their own column here -
  # unlike pivot_crosstab(), which merges Total into its first predictor set,
  # there's no natural place for a single "Total" column inside a 2-tier
  # nested header without it reading as a stray third outer level with only
  # one (unlabelled) inner column under it. Dropped rather than hidden.
  nested_rows <- data %>% filter(p_cat1 != "Total")

  nested_sets <- nested_rows %>% pull(cross_break) %>% unique()
  if (length(nested_sets) == 0) {
    stop("No nested predictor breakdown found in this data - only Total rows were present.")
  }
  if (length(nested_sets) > 1) {
    stop("pivot_nested_crosstab() only supports one nested predictor set at a time - found: ",
         paste(nested_sets, collapse = ", "))
  }

  # ---- validation: categorical variables get exactly one statistic (same rule as pivot_crosstab()) ----
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

  # ---- build final row structure: label rows, data rows - mirrors ----------
  # ---- pivot_crosstab()'s pivot_one_block(), just against col_order ---------
  # ---- (the composite outer:inner columns) instead of a flat level set -----
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

  # ---- one Base row per variable, appended after its last label/data row ----
  # (relocated into a single consolidated section at the very bottom of the
  # table further down - see that section's header note - so `Variable` is
  # set to the variable's own label here, not left blank, since that's the
  # only point in the function this label is still in scope as `this_lab`)
  outcomes_in_order <- pivoted %>% pull(o_lab) %>% unique()

  pivoted <- map_df(outcomes_in_order, function(this_lab) {
    variable_rows <- pivoted %>% filter(o_lab == this_lab)
    base_row <- base_wide %>% filter(o_lab == this_lab) %>%
      mutate(Variable = this_lab, stat_code = NA_character_, row_type = "base",
             o_cat = NA_character_, stat = NA_character_,
             outcome_type = variable_rows$outcome_type[1])
    bind_rows(variable_rows, base_row)
  })

  # ---- attach nested p-values, one column per OUTER LEVEL ----
  # See the header note above for the design behind this: one p-value per
  # outer level, testing the inner variable against the outcome within that
  # level - not one for the whole nested set. calc_stats() already computed
  # and attached these to `data$p_value`, keyed by (outcome, cross_break,
  # p_cat1) - broadcast to every row of that outer level's block regardless
  # of o_cat/stat, the same way a flat predictor's p_value already is (see
  # nested_pvalues()'s header note in calc_stats.R). Shown once per outcome
  # block (its first data row) here too - EXCEPT a multicoded outcome, whose
  # levels each keep their own independent p-value on every row, matching
  # pivot_crosstab()'s existing multicode treatment exactly (not re-derived
  # here, same mechanism).
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

      # Right after that outer level's own last inner column - not left
      # wherever the loop happened to append it (which would cluster every
      # outer level's p-value column at the very end, after ALL outer
      # levels' inner columns) - same relocate() flat crosstabs already do
      # for their own per-set p-value column.
      last_col_for_lvl <- header_info %>% filter(outer_level == lvl) %>%
        pull(column_name) %>% utils::tail(1)
      pivoted <- pivoted %>% relocate(all_of(p_col_name), .after = all_of(last_col_for_lvl))
    }
  }

  # o_lab (NOT outcome/o_cat) is kept, unlike before - format_nested_crosstab()
  # needs it to detect block boundaries between two adjacent NUMERIC
  # variables now that Base rows no longer sit inline between them (see that
  # function's header note on why). Same treatment row_type/stat_code/
  # outcome_type already get: carried in the raw pivoted output, dropped
  # only once format_nested_crosstab() is done needing them, right before
  # the huxtable itself is built.
  pivoted <- pivoted %>% select(-outcome, -o_cat)

  pivoted <- pivoted %>%
    tidy_statistic_description() %>%
    mutate(Statistics = case_when(
      row_type == "label" ~ NA_character_,
      # Blank, not "Base" - the relocated section header (below) already
      # says "Sample sizes" once for the whole section, so repeating "Base"
      # on every individual row under it read as redundant.
      #
      # Correction (caught by test 13 failing against real huxtable output,
      # not hand-traced): I originally assumed merge_cells() in
      # format_nested_crosstab() would keep the anchor (Variable) cell's own
      # text and blank/discard the other cell (Statistics), which is why
      # blanking here seemed load-bearing for that merge to look right.
      # huxtable's actual documented behaviour is the opposite - merging
      # COPIES the top-left (anchor) cell's content into the other cell(s)
      # in the range, specifically so subsetting/reordering rows and columns
      # later doesn't silently lose data. So after the merge, Statistics no
      # longer holds NA at these rows - it holds whatever Variable holds
      # ("Sample sizes" at the header row, each variable's own label at the
      # individual base rows), copied over. That's harmless for display
      # (only the anchor cell's position is shown once cells are merged),
      # but it does mean blanking Statistics here is NOT what makes the
      # merge correct - the merge overwrites this regardless of what was
      # here beforehand. Blanking it is still worth doing in its own right
      # though: it's what satisfies Joe's actual ask ("don't show 'Base' in
      # the Statistics column") independent of whether a merge happens to
      # run afterwards.
      row_type == "base"  ~ NA_character_,
      TRUE ~ Statistics
    ),
    stat_code = if_else(row_type == "label", NA_character_, stat_code)) %>%
    relocate(Variable, Statistics, .before = 1) %>%
    relocate(row_type, stat_code, outcome_type, .after = dplyr::last_col())

  # ---- relocate every variable's Base row into one consolidated section ----
  # ---- at the bottom, rather than directly under each variable's own data ----
  # Joe's call, not pivot_crosstab()'s convention (which keeps Base inline)
  # - a single "Sample sizes" section at the end, one row per variable, that
  # row's Variable column already carrying the variable's own label (set
  # above, where each base row was originally built - see the comment
  # there). Titled "Sample sizes" rather than "Base" (Joe's wording) - only
  # the section header's own displayed text, not the internal row_type
  # ("base") anything else in this file keys off.
  # A fixed, always-on layout choice, not an optional argument: every other
  # toggle in this file/calc_stats.R (weighted, conf, pval, multicode)
  # changes what's actually computed or included, not just how it's laid
  # out, and there's no live need yet for both layouts side by side - cheap
  # to gate behind an argument later if that changes, since the whole thing
  # is this one isolated block.
  #
  # No sorting needed to do the actual move: filter() preserves each
  # subset's original relative row order, so splitting into "everything
  # else" and "just the base rows" and re-concatenating them is already a
  # stable partition - each variable's base row stays in the same relative
  # order (whatever order pivoted() built them in) without an explicit
  # arrange() step.
  #
  # The synthetic section-title row is deliberately given row_type = "label"
  # (same as any ordinary variable-label row) rather than a new row type, so
  # format_nested_crosstab() bolds it and gives it its own top-padding gap
  # via the SAME existing logic every other label row already gets - no new
  # formatting code needed for the title itself. See format_nested_crosstab()
  # for the two real follow-on changes this required: without them, (1)
  # every base row after the first would ALSO claim its own top-padding gap
  # (each one's immediate predecessor is also row_type == "base"), reading
  # as loose, inconsistent spacing rather than one tight consolidated block
  # under a single header; and (2), a subtler one caught while implementing
  # this rather than asked for directly, two adjacent NUMERIC variables
  # (e.g. age directly followed by score) would stop getting any separating
  # gap at all, since block-boundary detection used to infer a new numeric
  # variable from a data row landing right after the PREVIOUS variable's
  # base row - always true when bases sat inline, never true once every
  # base row moves down here instead.
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


# ---- format_nested_crosstab: style pivot_nested_crosstab() output ----------
#
# Most of the digit formatting, conf-as-inserted-row, block padding,
# numeric-variable row merging, and styling below is format_crosstab()
# almost unchanged - see that function's header note for the reasoning
# behind each of those pieces, not repeated here. Two genuinely new pieces:
# the 4-row header block itself, and p-value columns being per OUTER LEVEL
# rather than per predictor set - each one's own span (rows 1-3) has to
# widen to fold in its p-value column, the same way a flat crosstab's single
# span already folds in its one p-value column (see pivot_nested_crosstab()'s
# header note on the "among women, is age associated with this outcome"
# design behind it).
#
# Row insertion order: huxtable::insert_row(ht, ..., after = 0) always drops
# the new row at the very top, pushing everything else down by one - so
# calling it three times in a row and populating each one immediately BEFORE
# the next insert (rather than inserting all three blank first) is what keeps
# the bookkeeping simple: insert + populate the row that should end up
# closest to the original header (inner label, final row 3), then insert +
# populate the row above that (outer level, final row 2), then insert +
# populate the row above THAT (outer label, final row 1). Each new insert
# pushes the previous ones down by exactly one, which is accounted for by
# incrementing their tracked row numbers as we go.
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

  # Presence-based, same as pivot_crosstab()'s own gate - `__sigdiff` columns
  # only exist at all when calc_stats() was run with pairwise = TRUE, so their
  # presence alone is the signal, not whether any individual cell happens to
  # be non-NA.
  has_pairwise <- length(sigdiff_cols) > 0

  # ---- digit formatting, as plain text, before the huxtable exists ----
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
  # format_pvalue() reused from pivot_crosstab.R (sourced above) - same
  # <0.001 floor / 3dp rule a flat crosstab's p-value column already gets.
  for (col in pval_cols) {
    data[[col]] <- format_pvalue(data[[col]])
  }

  # ---- insert one extra "conf" row under every data row that has a ----
  # ---- non-NA conf value on at least one column -------------------------
  # Gated on !has_pairwise, same rule pivot_crosstab() uses: when pairwise
  # letters/sig_diff are being shown, SE/CI rows are suppressed entirely
  # rather than shown alongside - see that file's header note for the
  # reasoning (the sig_diff row IS the inferential information at that point,
  # and showing both crowds the table without adding anything).
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
      # A conf row doesn't carry a p-value of its own - blanked explicitly
      # rather than left holding whatever the estimate row's p-value column
      # had (which would otherwise just get copied along by `row[i, ]`),
      # same guard format_crosstab() already needs for the same reason.
      for (pc in pval_cols) conf_row[[pc]] <- NA_character_

      bind_rows(row %>% select(-all_of(conf_cols)),
                conf_row %>% select(-all_of(conf_cols)))
    })
  } else {
    data <- data %>% select(-any_of(conf_cols))
  }

  # ---- insert one extra "sigdiff" row under every data row that has a ----
  # ---- non-NA/non-empty sig_diff value on at least one column - exact ----
  # ---- parallel to the conf-row insertion just above, ported from ----
  # ---- pivot_crosstab()'s format_crosstab(). ----
  if (has_pairwise) {
    data <- map_df(seq_len(nrow(data)), function(i) {
      row <- data[i, , drop = FALSE]
      sd_values <- unlist(row[, sigdiff_cols, drop = FALSE])
      has_sigdiff <- row$row_type == "data" &&
        any(!is.na(sd_values) & sd_values != "")

      if (!has_sigdiff) return(row %>% select(-all_of(sigdiff_cols)))

      sigdiff_row <- row %>%
        mutate(Variable = "", Statistics = "Sig. diff",
               row_type = "sigdiff", stat_code = NA_character_)
      for (lc in col_order) {
        sc <- paste0(lc, "__sigdiff")
        if (sc %in% names(row)) sigdiff_row[[lc]] <- row[[sc]]
      }
      # Same reasoning as the conf row's own blanking above - a sigdiff row
      # doesn't carry its own p-value.
      for (pc in pval_cols) sigdiff_row[[pc]] <- NA_character_

      bind_rows(row %>% select(-all_of(sigdiff_cols)),
                sigdiff_row %>% select(-all_of(sigdiff_cols)))
    })
  } else {
    data <- data %>% select(-any_of(sigdiff_cols))
  }

  # ---- legend row: one row, right under the 4-row header, giving the ----
  # ---- reference "column X = letter a" lookup - exact parallel to ----
  # ---- pivot_crosstab()'s own legend row, just keyed on the composite ----
  # ---- (outer, inner) column names instead of a flat predictor's levels. ----
  # Built and prepended here, AFTER all digit formatting/conf/sigdiff row
  # insertion is done (so the letter values - already plain character
  # strings from calc_stats() - never pass through format_statistic()'s
  # numeric formatting), and BEFORE row_type/block-boundary detection below,
  # so the legend row is accounted for in every row-index calculation from
  # here on (data_rows, conf_rows, block_start_rows, etc.) the same simple
  # way pivot_crosstab() already does it.
  if (!is.null(legend) && nrow(legend) > 0) {
    legend_wide <- legend %>% pivot_wider(names_from = level_col, values_from = sig_letter)

    legend_row <- data %>% dplyr::slice(0) %>% tibble::add_row()
    legend_row$Variable     <- "Column reference"
    legend_row$row_type     <- "legend"
    legend_row$Statistics   <- NA_character_
    # Defensively copied from the first real row - see pivot_crosstab()'s
    # identical comment on this same pattern: o_lab/outcome_type aren't
    # meaningful for a legend row on their own, but downstream block-boundary
    # detection (new_block, below) reads o_lab off every row unconditionally,
    # so it needs SOMETHING sane here, not NA.
    legend_row$o_lab         <- data$o_lab[1]
    legend_row$outcome_type  <- data$outcome_type[1]
    for (lc in col_order) {
      if (lc %in% names(legend_wide)) legend_row[[lc]] <- legend_wide[[lc]][1]
    }

    data <- bind_rows(legend_row, data)
  }

  # ---- block boundaries ----
  # format_crosstab() detects a NUMERIC variable's first data row (numeric
  # variables have no label row of their own) as the row immediately after
  # the PREVIOUS variable's base row - safe there, since a base row always
  # sits directly between one variable's data and the next's. That's no
  # longer true here: pivot_nested_crosstab() relocates every variable's
  # base row into one consolidated section at the bottom (see its header
  # note), so no variable's data ever again lands right after ANY base
  # row - reusing that heuristic unmodified would silently stop separating
  # adjacent numeric variables at all (e.g. age directly followed by score,
  # both numeric, with nothing to mark the boundary between them).
  #
  # Detected directly instead, via o_lab (kept around for exactly this - see
  # the comment where pivoted stops dropping it): a numeric data row starts a
  # new block whenever its o_lab differs from the row before it. This also
  # has to survive two edge cases a naive Variable-text comparison wouldn't:
  # a conf row (SE/CI) deliberately blanks its own Variable text, which
  # would make the NEXT row look like a new variable even when it's the same
  # one's next stat; and requesting more than one statistic for the same
  # numeric outcome (e.g. mean AND median for "age") must NOT be treated as
  # two different variables. o_lab is immune to both, since only Variable
  # (never o_lab) gets blanked on a conf row, and every stat row for the
  # same outcome shares the same o_lab.
  #
  # Base rows are untouched by this clause (it only fires for
  # row_type == "data" & outcome_type == "numeric") - the consolidated Base
  # section still gets exactly one gap, from its row_type == "label" header,
  # via the first clause below; individual base rows never claim their own.
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

  # ---- the 4-row nested header block ----
  # header_info is already in the same left-to-right order as col_order (both
  # built from the same arrange(p_cat1, p_cat2) in reshape_nested_predictor_
  # set()), so level_col_idx[k] and header_info's row k always describe the
  # same physical column - no name-parsing needed anywhere below.
  outer_runs <- rle(header_info$outer_level)
  run_starts <- cumsum(c(1, utils::head(outer_runs$lengths, -1)))
  outer_label <- header_info$outer_label[1]
  inner_label <- header_info$inner_label[1]

  # Column indices per outer level, WIDENED to include that level's own
  # p-value column when it has one - folded into the same span as its inner
  # columns, same as a flat crosstab already does for its per-set p-value
  # column (see format_crosstab()'s `cols <- c(level_cols, ...)`). The
  # p-value column's exact name is rebuilt the same way
  # pivot_nested_crosstab() built it in the first place (same inner_label +
  # outer level), not string-parsed back out of anything.
  outer_group_cols <- purrr::map(seq_along(run_starts), function(i) {
    lvl <- outer_runs$values[i]
    lvl_level_cols <- level_col_idx[run_starts[i]:(run_starts[i] + outer_runs$lengths[i] - 1)]
    p_col_name <- paste0("p_value (", inner_label, " | ", lvl, ")")
    c(lvl_level_cols, which(names(ht) == p_col_name))
  })
  # Every outer group's columns (levels + its own p-value column) laid out
  # contiguously, group after group - the full span row 1 needs.
  all_group_cols <- sort(unlist(outer_group_cols))

  # row 3 (inner label, repeated once per outer level) - inserted first so it
  # lands immediately above the original bare column-name row.
  ht <- huxtable::insert_row(ht, rep("", ncol(ht)), after = 0)
  inner_label_row <- 1L
  for (i in seq_along(run_starts)) {
    cols_i <- outer_group_cols[[i]]
    ht[inner_label_row, min(cols_i)] <- inner_label
    if (length(cols_i) > 1) ht <- huxtable::merge_cells(ht, inner_label_row, range(cols_i))
  }

  # row 2 (outer levels, one span each) - pushes row 3 down by one.
  ht <- huxtable::insert_row(ht, rep("", ncol(ht)), after = 0)
  outer_level_row <- 1L
  inner_label_row <- inner_label_row + 1L
  for (i in seq_along(run_starts)) {
    cols_i <- outer_group_cols[[i]]
    ht[outer_level_row, min(cols_i)] <- outer_runs$values[i]
    if (length(cols_i) > 1) ht <- huxtable::merge_cells(ht, outer_level_row, range(cols_i))
  }

  # row 1 (outer label, spanning everything - including every outer level's
  # own p-value column) - pushes rows 2 and 3 down by one.
  ht <- huxtable::insert_row(ht, rep("", ncol(ht)), after = 0)
  outer_label_row <- 1L
  outer_level_row <- outer_level_row + 1L
  inner_label_row <- inner_label_row + 1L
  ht[outer_label_row, min(all_group_cols)] <- outer_label
  if (length(all_group_cols) > 1) ht <- huxtable::merge_cells(ht, outer_label_row, range(all_group_cols))

  header_offset <- 4L
  bare_row <- header_offset   # the original as_hux(add_colnames = TRUE) row, now pushed down to row 4

  # Row 4's displayed text: the bare inner level, not the composite
  # "<outer>: <inner>" name that names(ht) still carries (and needs to keep
  # carrying, for every lookup in the rest of this function) - assigned
  # directly from header_info$inner_level rather than string-stripped from
  # the column name, since header_info already has it and the two are
  # guaranteed to be in the same order.
  ht[bare_row, level_col_idx] <- header_info$inner_level

  # Row 4's displayed text for each p-value column: just "p-value", not the
  # full internal column name ("p_value (age_group | Male)"). The internal
  # name stays as-is (still what names(ht)/tests look up, still built the
  # same way in pivot_nested_crosstab()) - only what's shown here changes.
  # It's redundant to spell out "age_group" and "Male" a fourth time in this
  # column when rows 1-3 (sex -> Male -> age_group) already establish both,
  # the same way the level columns' own row 4 text is just "Young"/"Old",
  # not the composite "sex: Male, age_group: Young".
  pval_col_idx <- which(names(ht) %in% pval_cols)
  if (length(pval_col_idx) > 0) ht[bare_row, pval_col_idx] <- "p-value"

  ht <- ht %>%
    huxtable::set_align(seq_len(3), huxtable::everywhere, "center") %>%
    huxtable::set_bold(seq_len(header_offset), huxtable::everywhere, TRUE) %>%
    huxtable::set_bottom_border(header_offset, huxtable::everywhere,
                                 huxtable::brdr(1, "solid", "grey40"))

  # ---- vertical divider between outer groups (header AND body) ----
  # A small visual aid a flat crosstab has no equivalent need for: without
  # it, the boundary between (say) Male's age columns and Female's age
  # columns has no visual cue at all beyond the header text two rows up.
  # Skips the very first group's own left edge - nothing to divide there.
  group_start_cols <- level_col_idx[run_starts]
  group_start_cols <- setdiff(group_start_cols, min(level_col_idx))
  if (length(group_start_cols) > 0) {
    ht <- huxtable::set_left_border(ht, huxtable::everywhere, group_start_cols,
                                     huxtable::brdr(1, "solid", "grey60"))
  }

  no_of_rows     <- nrow(ht)                     # includes all 4 header rows
  data_rows      <- which(row_type == "data") + header_offset
  conf_rows      <- which(row_type == "conf") + header_offset
  sigdiff_rows   <- which(row_type == "sigdiff") + header_offset
  legend_hux_row <- which(row_type == "legend") + header_offset

  # ---- NA formatting: blank by default, "-" for a genuinely missing ----
  # ---- statistic on a data, conf or sigdiff row (including a ----
  # ---- rectangularity-filled combination with zero respondents, per the ----
  # ---- header note above). The legend row is deliberately NOT included ----
  # ---- here - a blank cell under a p-value column (or any column that ----
  # ---- genuinely has no letter) should stay blank, not read as "-". --------
  ht <- ht %>%
    huxtable::set_na_string(value = "") %>%
    huxtable::set_na_string(row = c(data_rows, conf_rows, sigdiff_rows), col = level_col_idx, value = "-")

  # ---- alignment: Variable/Statistics left, everything else right ----
  # Right-alignment is scoped to header_offset:no_of_rows, NOT "everywhere"
  # - "everywhere" would also hit rows 1:3 (the outer/inner group header
  # rows), silently overwriting the center alignment set_align() already
  # gave them above. That's exactly what was happening before this fix:
  # rows 1:3's level/p-value-column header text (sex / Male / age_group,
  # etc.) was being set to center first, then immediately knocked back to
  # right by this later, broader call - Joe flagged the right-aligned
  # header text as looking odd, and this is why. Row 4 (the bare inner
  # levels / "p-value") is deliberately still included in the right-aligned
  # range - only rows 1:3 were the ones Joe called out.
  ht <- ht %>%
    huxtable::set_align(huxtable::everywhere, variable_col, "left") %>%
    huxtable::set_align(huxtable::everywhere, which(names(ht) == "Statistics"), "left") %>%
    huxtable::set_align(header_offset:no_of_rows,
                         which(!names(ht) %in% c("Variable", "Statistics")), "right")

  # ---- bold variable name rows ----
  is_variable_row <- row_type == "label" | (row_type == "data" & outcome_type == "numeric")
  variable_name_rows <- which(is_variable_row) + header_offset
  ht <- huxtable::set_bold(ht, row = variable_name_rows, col = variable_col, value = TRUE)

  # ---- merge repeated Variable text within a numeric variable's block ----
  merge_group <- dplyr::if_else(
    row_type %in% c("data", "conf") & outcome_type == "numeric",
    paste0("numeric_", block_id),
    paste0("nomerge_", seq_along(row_type))
  )
  run_lengths <- rle(merge_group)$lengths
  merge_run_starts <- cumsum(c(1, utils::head(run_lengths, -1)))

  # No follow-up loop blanking ht[r, variable_col] here (an earlier version
  # of this code had one) - removed as dead code, confirmed by a real test
  # run against actual huxtable output (in pivot_crosstab.R's twin of this
  # block - same copy-pasted pattern, same fix here), not assumed: per
  # huxtable's own docs (spans.Rd), setting rowspan() COPIES the anchor
  # cell's content into every cell it covers, and extracting a covered cell
  # (via `[[`, same as printing) always resolves back to that anchor
  # content regardless of what's assigned to the covered cell afterwards.
  # So a blanking loop here would run and change nothing observable - the
  # covered rows already read as the anchor's text on access, and were
  # always DISPLAYED as blank anyway since rowspan collapses them visually
  # either way.
  for (i in seq_along(merge_run_starts)) {
    if (run_lengths[i] > 1) {
      start_row <- merge_run_starts[i] + header_offset
      span_len  <- run_lengths[i]
      ht <- huxtable::set_rowspan(ht, row = start_row, col = variable_col, value = span_len)
    }
  }

  # ---- legend row: merge Variable + Statistics into one wide cell, italic ----
  # ---- text - exact parallel to pivot_crosstab()'s own legend row styling. ----
  # ---- Placed before stripe_rows() (below) so the merge is in place before ----
  # ---- striping runs; no background-color override (per Joe's "too dark" ----
  # ---- feedback on the flat crosstab's original legend-row shading) - it ----
  # ---- just takes whatever stripe_rows() naturally assigns it. ----
  if (length(legend_hux_row) > 0) {
    statistics_col <- which(names(ht) == "Statistics")
    ht <- huxtable::merge_cells(ht, legend_hux_row, range(c(variable_col, statistics_col)))
    ht <- huxtable::set_italic(ht, legend_hux_row, huxtable::everywhere, TRUE)
  }

  # ---- merge Variable + Statistics into one wide cell within the Sample ----
  # ---- sizes section - Statistics is blank there anyway on every row (the ----
  # ---- section header via row_type == "label", each individual row via ----
  # ---- pivot_nested_crosstab()'s case_when - see its comment), so folding ----
  # ---- it into Variable gives the variable label the room a normally- ----
  # ---- narrow Statistics column would otherwise waste. ----
  # ---- ----
  # ---- Per huxtable's own docs (confirmed there, not assumed): merging ----
  # ---- does NOT blank the non-anchor cell - it COPIES the anchor (top- ----
  # ---- left, i.e. Variable) cell's content into it, so subsetting/ ----
  # ---- reordering rows or columns later can't silently lose data. That's ----
  # ---- invisible here at the DISPLAY level (only the anchor's position is ----
  # ---- ever shown once cells are merged) but it does mean the underlying ----
  # ---- Statistics value at these rows stops being NA after this runs - it ----
  # ---- becomes a copy of Variable's text instead. Blanking Statistics ----
  # ---- beforehand is still correct to do (it's Joe's actual ask, and it's ----
  # ---- what the value would show as if this merge were ever removed) - ----
  # ---- it's just not what makes the MERGE itself safe, the way an earlier ----
  # ---- version of this comment claimed. ----
  base_data_rows <- which(row_type == "base")
  if (length(base_data_rows) > 0) {
    statistics_col <- which(names(ht) == "Statistics")
    # The row directly above the first base row - guaranteed to be the
    # "Sample sizes" section header, since relocation (pivot_nested_
    # crosstab()) always places exactly one row_type == "label" row
    # immediately before the whole relocated block, nothing else.
    section_header_row <- min(base_data_rows) - 1
    merge_rows <- c(section_header_row, base_data_rows) + header_offset
    for (r in merge_rows) {
      ht <- huxtable::merge_cells(ht, r, range(c(variable_col, statistics_col)))
    }
  }

  # ---- alternating background, one row one colour, next row the other ----
  ht <- huxtable::stripe_rows(ht, stripe1 = "#f5f7fa", stripe2 = "#ffffff")

  # ---- minimal styling: small font, tight padding, light grey borders ----
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

  # ---- legend row vertical alignment: top-aligned, asymmetric padding ----
  # ---- Placed here, AFTER the "minimal styling" block's blanket ----
  # ---- set_valign("middle")/set_all_padding(1) - calling it earlier would ----
  # ---- get silently overwritten by those later blanket calls, exactly the ----
  # ---- bug this ordering avoided in pivot_crosstab()'s own legend row (see ----
  # ---- that file's header note). Top/bottom padding is deliberately ----
  # ---- asymmetric (1 vs 6), not just set_valign("top") alone - with equal ----
  # ---- padding on both sides "top"-aligned text looks identical to ----
  # ---- centred text, which is what made the first attempt at this look ----
  # ---- like it hadn't worked at all. ----
  if (length(legend_hux_row) > 0) {
    ht <- ht %>%
      huxtable::set_valign(legend_hux_row, huxtable::everywhere, "top") %>%
      huxtable::set_top_padding(legend_hux_row, huxtable::everywhere, 1) %>%
      huxtable::set_bottom_padding(legend_hux_row, huxtable::everywhere, 6)
  }

  # ---- footnotes ----
  if (all(unlist(bases) != "NA")) {
    for (i in seq_along(bases)) {
      ht <- ht %>% huxtable::add_footnote(bases[[i]], border = NULL)
    }
  }

  ht
}
