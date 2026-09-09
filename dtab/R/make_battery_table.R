# Kept separate from make_table() for the same dispatch-collision reason
# as make_numeric_summary_table() - see that file's header note.
# pivot_battery() takes calc_stats() output as `data` rather than calling
# calc_stats() itself, so this wrapper calls calc_stats() with predictors
# hardcoded to NULL before pivoting.

#' Build a battery/grid table in one call
#'
#' Single entry point chaining [calc_stats()] straight to [pivot_battery()]
#' and [format_battery()] - the battery-grid equivalent of [make_table()]'s
#' one-call convenience, kept separate for the same dispatch-collision
#' reason as [make_numeric_summary_table()] (see this file's header note).
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
