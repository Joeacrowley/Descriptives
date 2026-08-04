# =============================================================================
# make_numeric_summary_table() — single entry point chaining
# pivot_numeric_summary() -> format_numeric_summary(), the numeric-summary
# equivalent of make_table()'s single-call convenience for the other three
# table types.
#
# NOT folded into make_table() itself - a deliberate call, not an oversight.
# pivot_numeric_summary() is "STRUCTURALLY DIFFERENT" from pivot_summary()/
# pivot_crosstab()/pivot_nested_crosstab() by its own header note, in ways
# that would have compromised make_table()'s design rather than just adding
# a fourth branch to it:
#
#   - It calls calc_stats() ITSELF, rather than taking calc_stats() output as
#     its `data` argument the way all three of make_table()'s existing
#     branches do. make_table()'s whole structure assumes one shared
#     calc_stats() call, then routing the result - pivot_numeric_summary()
#     breaks that assumption at the root, not just at the margins.
#
#   - Its argument set doesn't overlay cleanly onto calc_stats()'s own: no
#     predictors/conf/pval/multicode at all (this table has no crossbreak
#     dimension and is point-estimates-only by design - see its own header
#     note), but it DOES have `weighted`, a plain TRUE/FALSE toggle that gets
#     translated into the right calc_stats() stat codes (mean -> w_mean, ...)
#     before calc_stats() ever runs. None of make_table()'s three paths need
#     or support that translation layer.
#
#   - Genuine dispatch collision, not just an edge case: make_table() already
#     routes predictors = NULL to pivot_summary(). A numeric summary is ALSO
#     a no-predictors, Total-only table - so predictors' shape alone can't
#     tell "pivot_summary()'s breakdown" and "a numeric summary grid" apart.
#     Resolving that would need an explicit extra argument on make_table(),
#     defeating the "infer purely from what you passed" design its dispatch
#     currently has.
#
# So this is a small parallel sibling instead - same one-call convenience,
# same `formatted` toggle and its exact reasoning (see make_table.R's header
# note on why formatted = FALSE returns the bare pivot_*() list rather than
# the huxtable), no compromise to either function's own design.
#
# Arguments are pivot_numeric_summary()'s own, unchanged and un-renamed
# (data, outcomes, statistics, weighted, base), plus `formatted` - this
# function adds no other arguments of its own, and does no dispatching (there
# being only one pivot_*()/format_*() pair to route to here in the first
# place).
#
# I haven't been able to run any of this myself - no R available in this
# environment. See test_make_numeric_summary_table.R for what's covered.
# =============================================================================


#' Build a numeric-only summary table in one call
#'
#' Single entry point chaining [pivot_numeric_summary()] straight to
#' [format_numeric_summary()] - the numeric-summary equivalent of
#' [make_table()]'s one-call convenience, kept as a separate function
#' rather than folded into `make_table()` (see this file's header note for
#' the dispatch-collision reasoning: `predictors = NULL` already routes
#' `make_table()` to a plain summary table, and a numeric-statistics grid
#' is also a no-predictors table).
#'
#' @param data A data frame, or survey design object if `weighted = TRUE`.
#' @param outcomes Character vector of numeric variable names to summarise.
#' @param statistics One or more of `mean`, `median`, `sum`, `min`, `max`,
#'   `range`, `iqr`, `sd`. Default `c("mean", "sd")`.
#' @param weighted If `TRUE`, requests the weighted equivalent of each
#'   statistic that has one (`data` must then be a survey design object).
#' @param base Optional base/filter expression, passed to [calc_stats()].
#' @param formatted If `TRUE` (default), returns the styled `huxtable`.
#'   If `FALSE`, returns [pivot_numeric_summary()]'s own list output
#'   untouched.
#'
#' @return A styled `huxtable`, or (if `formatted = FALSE`) the list
#'   returned by [pivot_numeric_summary()].
#'
#' @seealso [pivot_numeric_summary()], [format_numeric_summary()]
#' @export
make_numeric_summary_table <- function(data, outcomes, statistics = c("mean", "sd"),
                                        weighted = FALSE, base = NULL, formatted = TRUE) {

  pivot_result <- pivot_numeric_summary(data, outcomes, statistics, weighted, base)

  if (!formatted) return(pivot_result)

  format_numeric_summary(pivot_result)
}
