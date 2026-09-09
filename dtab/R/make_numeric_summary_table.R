# Kept separate from make_table() rather than folded in: predictors = NULL
# already routes make_table() to pivot_summary(), and a numeric summary is
# also a no-predictors table, so predictors' shape alone can't distinguish
# the two.

#' Build a numeric-only summary table in one call
#'
#' Single entry point chaining [pivot_numeric_summary()] straight to
#' [format_numeric_summary()] - the numeric-summary equivalent of
#' [make_table()]'s one-call convenience, kept separate rather than folded
#' in (see this file's header note, and [make_table()]'s own header note
#' for the `formatted` toggle's shared design rationale).
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
