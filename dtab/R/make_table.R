# make_table(): dispatch table for `predictors`' shape (see roxygen below).
# Rejected up front rather than failing confusingly deeper in a pivot_*()
# function: a nested set combined with additional flat sets, more than one
# nested-shaped entry, or a nested entry that isn't exactly 2 variables.


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

  # Normalise the same way calc_stats() itself does.
  predictors_list <- if (is.null(predictors)) {
    NULL
  } else if (is.character(predictors)) {
    list(predictors)
  } else {
    predictors
  }

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
  # not an index, and predictors_list[[<logical vector>]] wouldn't select
  # the nested entry.
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

  pivot_result <- switch(table_type,
    summary         = pivot_summary(stats_table),
    crosstab        = pivot_crosstab(stats_table),
    nested_crosstab = pivot_nested_crosstab(stats_table)
  )

  if (!formatted) return(pivot_result)

  switch(table_type,
    summary         = format_summary(pivot_result),
    crosstab        = format_crosstab(pivot_result),
    nested_crosstab = format_nested_crosstab(pivot_result)
  )
}
