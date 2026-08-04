# =============================================================================
# make_battery_table() — single entry point chaining calc_stats() ->
# pivot_battery() -> format_battery(), the battery-grid equivalent of
# make_table()'s single-call convenience for the other three table types
# (and make_numeric_summary_table()'s for the numeric-summary grid).
#
# NOT folded into make_table() itself - same fundamental reason
# make_numeric_summary_table() wasn't (see that file's own header note, not
# re-derived here in full): a genuine DISPATCH COLLISION, not just an edge
# case. make_table() already routes predictors = NULL to pivot_summary() -
# a battery grid is ALSO a no-predictors, Total-only table (pivot_battery()
# enforces predictors = NULL just as strictly - see its own header note on
# why there's no room for a third axis), so `predictors`' shape alone can't
# tell "pivot_summary()'s breakdown" and "a battery grid" apart any more
# than it could tell a numeric summary grid apart from either. Resolving
# that inside make_table() would need an explicit extra argument, defeating
# its "infer purely from what you passed" dispatch design.
#
# pivot_battery() also has validation make_table()'s existing paths don't:
# every outcome must be outcome_type "categorical"/"multicoded" (no mixing
# with numeric outcomes the way pivot_summary() allows), and exactly ONE
# statistic per call (perc, count, w_perc, or w_count) - not a vector of
# several the way every other table type accepts. A small parallel sibling
# keeps that validation scoped to where it belongs, rather than complicating
# make_table()'s shared dispatch with a fourth, differently-shaped branch.
#
# UNLIKE pivot_numeric_summary(), pivot_battery() does NOT call calc_stats()
# itself - it takes calc_stats() output as `data`, same as pivot_summary()/
# pivot_crosstab()/pivot_nested_crosstab() do. So this wrapper calls
# calc_stats() itself instead (predictors hardcoded to NULL, matching
# pivot_battery()'s own requirement), the same shape make_table() uses for
# its three paths - `data` here is the raw data/survey design, not
# calc_stats() output, same as every other make_*() function.
#
# `conf` IS exposed here (unlike make_numeric_summary_table(), which
# hardcodes conf = NULL since pivot_numeric_summary() never supported SE/CI
# at all) - pivot_battery()/format_battery() gained SE/CI support as an
# inserted row under each item (see pivot_battery.R's own "SE/CI: ADDED
# LATER" header note), so there's a real conf argument worth passing
# through now.
#
# Same `formatted` toggle and exact reasoning as make_table()/
# make_numeric_summary_table(): formatted = FALSE stops after the pivot step
# and returns pivot_battery()'s own list (pivoted, stat_code, category_order,
# conf_type) untouched, rather than the finished huxtable.
#
# I haven't been able to run any of this myself - no R available in this
# environment. See test_make_battery_table.R for what's covered.
# =============================================================================


#' Build a battery/grid table in one call
#'
#' Single entry point chaining [calc_stats()] straight to [pivot_battery()]
#' and [format_battery()] - the battery-grid equivalent of [make_table()]'s
#' one-call convenience, kept as a separate function rather than folded
#' into `make_table()` for the same dispatch-collision reason as
#' [make_numeric_summary_table()] (see this file's header note:
#' `predictors = NULL` already routes `make_table()` to a plain summary
#' table, and a battery grid is also a no-predictors table).
#'
#' @param data A data frame, or survey design object.
#' @param outcomes Character vector of categorical/multicoded outcome
#'   variable names sharing consistent category levels (e.g. a Likert
#'   battery).
#' @param statistics Exactly one statistic code: `"perc"`, `"count"`,
#'   `"w_perc"`, or `"w_count"`.
#' @param conf `NULL`, `"se"`, or `"ci"` - inserted as an extra row under
#'   each item when requested.
#' @param base Optional base/filter expression, passed to [calc_stats()].
#' @param multicode Passed to [calc_stats()].
#' @param formatted If `TRUE` (default), returns the styled `huxtable`.
#'   If `FALSE`, returns [pivot_battery()]'s own list output untouched
#'   (`pivoted`, `stat_code`, `category_order`, `conf_type`).
#'
#' @return A styled `huxtable`, or (if `formatted = FALSE`) the list
#'   returned by [pivot_battery()].
#'
#' @seealso [pivot_battery()], [format_battery()]
#' @export
make_battery_table <- function(data, outcomes, statistics = "perc", conf = NULL,
                                base = NULL, multicode = FALSE, formatted = TRUE) {

  stats_table <- calc_stats(data, outcomes = outcomes, predictors = NULL,
                             statistics = statistics, conf = conf, base = base,
                             multicode = multicode)

  pivot_result <- pivot_battery(stats_table)

  if (!formatted) return(pivot_result)

  format_battery(pivot_result)
}
