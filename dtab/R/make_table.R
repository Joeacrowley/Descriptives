# =============================================================================
# make_table() — single entry point chaining calc_stats() -> the right
# pivot_*() -> the right format_*(), so a normal call site never has to name
# pivot_summary()/pivot_crosstab()/pivot_nested_crosstab() or their format_*()
# partners directly. Returns the finished huxtable by default (same return
# type as every format_*() function on its own) - bases are already folded in
# as footnotes by that point (see each format_*()'s own footnote block), so
# there's nothing else worth exposing alongside it in that mode.
#
# Arguments are calc_stats()'s own, unchanged and un-renamed (data, outcomes,
# predictors, statistics, conf, base, pval, multicode, pairwise), plus one
# new argument of make_table()'s own: `formatted`.
#
# ---- pairwise = TRUE -------------------------------------------------------
# Passed straight through to calc_stats(), with no dispatch logic of its own
# needed here: pivot_crosstab()/format_crosstab() and pivot_nested_crosstab()/
# format_nested_crosstab() already render the legend block and Sig. diff row
# automatically whenever the data they're given carries sig_letter/sig_diff
# columns (see each function's own header note) - make_table() doesn't need
# to know or care which table_type it dispatched to for this to work.
# pivot_summary() has nothing to be pairwise ABOUT (no predictor to compare
# levels across), so pairwise = TRUE there is a harmless no-op: calc_stats()
# still runs add_pairwise_sig() internally, but every row's cross_break is
# "Total", so add_pairwise_sig()'s own eligible-rows filter
# (cross_break != "Total") finds nothing to test and returns sig_letter/
# sig_diff as NA throughout, same as never having asked for it.
#
# ---- formatted = FALSE: stop after the pivot step, skip format_*() ----------
# The huxtable format_*() produces is display text, not data - digit
# formatting (rounding, comma-separating, "%"-stripping) happens as plain
# character strings before as_hux() ever runs, and format_*() also inserts
# presentation-only rows (SE/CI as their own row, the relocated "Sample
# sizes" section) that don't belong in something meant for further
# computation. `formatted = FALSE` returns pivot_*()'s own output instead -
# whichever of pivot_summary()/pivot_crosstab()/pivot_nested_crosstab() the
# dispatch below picked - untouched, as the list it already returns
# (`pivoted` plus `bases` and, for the two crosstab variants, some extra
# type-specific metadata format_*() would otherwise need). The list is
# returned as-is, not just the `pivoted` tibble alone, specifically so
# nothing (bases included) becomes silently unreachable in this mode - a
# caller who only wants the tibble can still just take `[[1]]`.
#
# It's purely a dispatcher otherwise: it inspects `predictors` (before
# calc_stats() even runs) to decide which table shape was asked for, calls
# calc_stats() exactly once, then routes the result to the matching
# pivot_*() (and, unless formatted = FALSE, that pivot_*()'s own format_*()
# partner).
#
# ---- dispatch rule ---------------------------------------------------------
# The three existing pivot_*() functions already carve up the space of
# possible `predictors` shapes between them, via calc_stats()'s own
# established syntax (see calc_stats()'s `if (is.character(predictors))
# predictors <- list(predictors)` coercion, and pivot_nested_crosstab()'s
# header note on `predictors = list(c(outer_var, inner_var))`):
#
#   predictors = NULL                          -> pivot_summary()   (Total only)
#   predictors = "sex"                         -> pivot_crosstab()  (1 flat set)
#   predictors = list("sex", "age_group")      -> pivot_crosstab()  (multiple flat sets)
#   predictors = list(c("sex", "age_group"))   -> pivot_nested_crosstab() (1 nested set)
#
# This is a clean three-way split with no real ambiguity, so make_table()
# infers it directly from the shape of `predictors` rather than asking for a
# separate `type =` argument that could disagree with what `predictors`
# itself implies.
#
# ---- the one combination nothing currently renders -------------------------
# calc_stats() itself can compute a call that MIXES a nested set with
# additional flat sets in the same call (see calc_stats.R's own comment,
# "a call can mix flat and nested predictor sets", in its nested-p-value
# branch) - but no pivot_*() function can currently DISPLAY that combination.
# pivot_crosstab() explicitly drops nested predictor columns and refuses to
# handle them ("Nested combined predictors are out of scope for this
# function" - its own header note); pivot_nested_crosstab() is, by its own
# design, scoped to "exactly one nested set per table - no side-by-side flat
# sets alongside it". So make_table() rejects this shape up front, before
# ever calling calc_stats(), with an error that says why and what to do
# instead (two separate calls) - better than either silently dropping data
# (pivot_crosstab()'s own behaviour if handed nested columns) or a confusing
# failure three functions deep inside pivot_nested_crosstab().
#
# Also rejected up front: more than one nested-shaped entry (pivot_nested_
# crosstab() only ever renders one), and a nested entry that isn't exactly
# 2 variables (outer, inner) - pivot_nested_crosstab() is explicitly scoped
# to one nesting level, not arbitrary N-level nesting (see its own header
# note). calc_stats() may or may not itself already reject some of these
# shapes - checked here regardless, so the error is about make_table()'s own
# real constraint (which pivot_*() function exists to render the result),
# not whatever calc_stats() happens to do first.
#
# I haven't been able to run any of this myself - no R available in this
# environment. See test_make_table.R for what's covered.
# =============================================================================


#' Single entry point: `calc_stats()` chained straight to the right
#' pivot/format pair
#'
#' Dispatches on the shape of `predictors` to decide which table this is -
#' a Total-only summary ([pivot_summary()]), a flat crosstab
#' ([pivot_crosstab()]), or a single nested crosstab
#' ([pivot_nested_crosstab()]) - so a normal call site never has to name
#' any of those three functions (or their `format_*()` partners) directly.
#'
#' @param data A data frame, or survey design object.
#' @param outcomes Character vector of outcome variable names.
#' @param predictors `NULL` for a Total-only summary, a single variable
#'   name or list of variable names for a flat crosstab (one or more
#'   side-by-side predictor blocks), or `list(c(<outer>, <inner>))` for a
#'   single nested crosstab. Mixing a nested set with additional flat sets
#'   in one call is rejected - run two separate `make_table()` calls
#'   instead.
#' @param statistics Character vector of statistic codes, passed to
#'   [calc_stats()].
#' @param conf `NULL`, `"se"`, or `"ci"`, passed to [calc_stats()].
#' @param base Optional base/filter expression, passed to [calc_stats()].
#' @param pval Optional p-value method, passed to [calc_stats()].
#' @param multicode Passed to [calc_stats()].
#' @param pairwise If `TRUE`, passed to [calc_stats()] - runs letter-based
#'   pairwise significance testing across predictor levels and adds the
#'   legend block/Sig. diff row wherever there's a predictor to compare
#'   across ([pivot_crosstab()]/[pivot_nested_crosstab()] and their
#'   `format_*()` partners already render this automatically). A no-op for
#'   a Total-only table (`predictors = NULL`), since there's nothing to
#'   compare.
#' @param formatted If `TRUE` (default), returns the styled `huxtable`.
#'   If `FALSE`, stops after the pivot step and returns the matching
#'   `pivot_*()` function's own list output untouched (no digit formatting
#'   or presentation-only rows), for further computation.
#'
#' @return A styled `huxtable`, or (if `formatted = FALSE`) the list
#'   returned by whichever `pivot_*()` function was dispatched to.
#'
#' @seealso [calc_stats()], [pivot_summary()], [pivot_crosstab()],
#'   [pivot_nested_crosstab()]
#' @export
make_table <- function(data, outcomes, predictors = NULL, statistics = c("count", "mean"),
                        conf = NULL, base = NULL, pval = NULL, multicode = TRUE,
                        pairwise = FALSE, formatted = TRUE) {

  # ---- normalise predictors the same way calc_stats() itself does, so the ----
  # ---- shape we inspect here is exactly the shape calc_stats() will see ------
  predictors_list <- if (is.null(predictors)) {
    NULL
  } else if (is.character(predictors)) {
    list(predictors)
  } else {
    predictors
  }

  # Each element's length tells nested apart from flat: a flat set is always
  # a single variable name (length 1, e.g. "sex"); a nested set is the one
  # shape with more than one (e.g. c("sex", "age_group")) - per pivot_
  # nested_crosstab()'s own `predictors = list(c(outer_var, inner_var))`
  # syntax.
  #
  # Guarded explicitly for predictors_list == NULL (the summary case) rather
  # than trusting purrr::map_int(NULL, length) to do the right thing on its
  # own - I can't confirm without R whether that returns integer(0) cleanly
  # or errors, so this sidesteps the question entirely instead of assuming.
  if (is.null(predictors_list)) {
    n_nested <- 0
  } else {
    set_lengths <- purrr::map_int(predictors_list, length)
    n_nested    <- sum(set_lengths > 1)
  }

  if (n_nested > 1) {
    stop("make_table() supports at most one nested predictor set per call ",
         "(e.g. list(c(\"sex\", \"age_group\"))) - pivot_nested_crosstab() ",
         "only ever renders one. Found ", n_nested, ".")
  }
  if (n_nested == 1 && length(predictors_list) > 1) {
    stop("make_table() can't combine a nested predictor set with additional ",
         "side-by-side predictor sets in the same table - pivot_nested_",
         "crosstab() only supports exactly one nested set, with nothing else ",
         "alongside it (pivot_crosstab() is the one that handles multiple ",
         "flat sets, but it explicitly can't render nested columns at all). ",
         "Run these as two separate make_table() calls instead.")
  }
  # which(), not set_lengths > 1 directly - the latter is a logical mask,
  # not an index, and predictors_list[[<logical vector>]] doesn't select
  # the nested entry the way it might look like it does.
  if (n_nested == 1 && length(predictors_list[[which(set_lengths > 1)]]) != 2) {
    stop("make_table()'s nested predictor set must have exactly 2 variables ",
         "(outer, inner) - pivot_nested_crosstab() is scoped to one nesting ",
         "level, not arbitrary N-level nesting.")
  }

  table_type <- if (is.null(predictors_list)) {
    "summary"
  } else if (n_nested == 1) {
    "nested_crosstab"
  } else {
    "crosstab"
  }

  stats_table <- calc_stats(data, outcomes, predictors, statistics, conf, base, pval, multicode,
                             pairwise)

  # ---- pivot step - always runs, regardless of `formatted` ----
  pivot_result <- switch(table_type,
    summary         = pivot_summary(stats_table),
    crosstab        = pivot_crosstab(stats_table),
    nested_crosstab = pivot_nested_crosstab(stats_table)
  )

  if (!formatted) return(pivot_result)

  # ---- format step - skipped entirely when formatted = FALSE ----
  switch(table_type,
    summary         = format_summary(pivot_result),
    crosstab        = format_crosstab(pivot_result),
    nested_crosstab = format_nested_crosstab(pivot_result)
  )
}
