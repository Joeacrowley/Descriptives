# =============================================================================
# dtab-package.R — package-level documentation and the blanket @import
# declarations every other file in R/ relies on.
#
# WHY A BLANKET @import RATHER THAN @importFrom-ing every individual
# function: the code ported into this package (from the old Scripts_new/
# folder, loaded via source() + library(tidyverse) at each file's own top)
# was written almost entirely with UNQUALIFIED tidyverse calls (mutate(),
# filter(), map(), str_detect(), ...) - only huxtable and survey were mostly
# (see below for the one exception found) already namespace-qualified
# (huxtable::foo(), specifically to avoid a real conflict with flextable -
# see pivot_summary.R's own header note, carried over unchanged).
# Rewriting every one of the hundreds of unqualified tidyverse calls to
# @importFrom-and-qualify individually, across ~10 files and 400+KB of
# code, without being able to run R here to catch a missed one, would be a
# large, high-risk mechanical rewrite for very little real benefit in an
# internal-use package - so this package imports the whole namespace of
# each component actually called unqualified anywhere in R/, exactly
# reproducing what library(dplyr); library(tidyr); ... already did when
# these files were loaded via source().
#
# Confirmed by an actual grep across every ported file for `pkg::` usage
# (to see what's ALREADY qualified) and for each package's own commonly-
# used function names (to see what's called bare) - not assumed:
#   - huxtable, survey, wrappedtools, haven: every real call site found was
#     already `pkg::`-qualified. No @import needed for these - just being
#     listed in DESCRIPTION's Imports is enough for `::` to resolve.
#   - ONE exception caught by that grep: weighted_test_cat_by_cat()'s own
#     `svychisq(...)` call was NOT qualified (unlike every other survey::
#     call in the same file) - a real latent bug, not a packaging
#     technicality: it only ever worked because library(survey) was called
#     at calc_stats.R's own top when sourced the old way, silently
#     attaching the whole package to the search path. Fixed by qualifying
#     it to survey::svychisq(), matching the file's own established
#     convention, rather than papering over it with @import survey (worth
#     the same fix in the pre-package Scripts_new/calc_stats.R too, since
#     it's equally fragile there - it just happens to still work as long as
#     that file keeps calling library(survey) itself).
#   - srvyr (survey_mean/survey_median/survey_total/survey_prop) and
#     labelled (var_label()) ARE called bare/unqualified at real call
#     sites in calc_stats.R - both genuinely need @import here.
#   - stats::pnorm() and utils::combn()/head()/tail() are already
#     `::`-qualified throughout - no @import needed, but stats/utils DO
#     need to be listed in DESCRIPTION's Imports (added) even though
#     they're base/recommended packages always installed with R - R CMD
#     check still expects any `::`-qualified package to be declared.
#
# rlang is the ONE deliberate exception to "blanket @import everything
# called bare": devtools::document()/library(dtab) itself surfaced a real
# NAMESPACE conflict when this was `@import rlang` - rlang and purrr both
# export several identically-named functions (flatten*, splice, invoke,
# %@%, all leftover purrr<1.0 compatibility shims that got moved into
# rlang), so importing both whole namespaces means one silently masks the
# other's version, order-dependent, on every load. The actual bare rlang
# usage in R/ turned out to be a single function - sym() (calc_stats.R,
# building a symbol from a variable-name string for tidy-eval column
# selection) - so this is @importFrom-ed individually instead, sidestepping
# the whole-namespace collision entirely rather than just tolerating the
# warning.
#
# stats/utils: R CMD check's "R code for possible problems" pass flagged a
# real gap here (separate from the huxtable/survey/wrappedtools/haven
# audit above) - base-R statistical/utility functions called bare
# throughout calc_stats.R (sd(), median(), lm(), chisq.test(),
# shapiro.test(), kruskal.test(), oneway.test(), as.formula(), formula(),
# complete.cases(), na.omit(), coef(), vcov(), residuals(), IQR(),
# setNames() - the last of these lives in stats, not base, despite
# looking like a base utility) and head() (utils). These work fine at
# runtime purely because stats/utils are always attached for every R
# session, unlike the dplyr/tidyr/... functions above - but R CMD check
# still wants them declared, and devtools::document() surfaced the exact
# list to add (rather than guessed at here).
#
# EXPORT SCOPE: 6 functions are @export'd - calc_stats(), make_table(),
# make_numeric_summary_table(), make_battery_table(), export_table_to_excel(),
# export_tables_to_excel(). calc_stats() is exported alongside the make_*()
# wrappers deliberately, not just left as their shared internal engine - it
# has real standalone utility on its own (the raw long-format tibble, for
# anyone who wants to compute on it directly rather than go straight to a
# rendered table). make_table() itself now has a pairwise argument (passed
# straight through to calc_stats() - see make_table.R's own header note),
# so pairwise significance testing is reachable through the wrapper path
# too, not just by calling calc_stats() directly.
# add_pairwise_sig(), base_information(), and every pivot_*()/format_*()
# pair remain internal (@keywords internal, not @export) - real, tested,
# fully roxygen-documented code, just not part of the public API. Still get
# a help topic each (roxygen2 links between functions still resolve), just
# don't show up in library(dtab)'s exported namespace. base_information()
# is genuinely unused by anything else in the package today (nothing
# currently builds a `base` argument to pass through) - kept internal
# rather than removed, as a foundation to build on later.
#
# NOTE: after any change to these @import/@export tags (or to any other
# roxygen block in this package), run devtools::document() to regenerate
# NAMESPACE and the Rd files - nothing here is hand-maintained.
#
#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @import tibble
#' @import forcats
#' @importFrom rlang sym :=
#' @import srvyr
#' @importFrom labelled var_label
#' @importFrom stats IQR as.formula chisq.test coef complete.cases formula
#'   kruskal.test lm median na.omit oneway.test residuals sd setNames
#'   shapiro.test vcov
#' @importFrom utils head
"_PACKAGE"


# =============================================================================
# NSE column names - not real global variables/functions, just how tidy
# evaluation (mutate(), filter(), summarise(), group_by(), pivot_wider()'s
# names_from, ...) looks to R CMD check's static analysis (codetools has
# no way to know these bare symbols resolve to columns of whatever `data`
# happens to be in scope at each call site, not actual globals). Standard
# tidyverse-package idiom for silencing the resulting NOTE - see
# https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-select-rename-relocate/
# and countless CRAN tidyverse-adjacent packages' own R/globals.R for the
# same pattern. ":=" is rlang's walrus operator (mutate(!!name := value));
# @importFrom rlang := above is the real fix for it resolving at runtime,
# but codetools flags it here independently of that. "." is the magrittr/
# purrr pipe placeholder. "..total.." is calc_stats.R's own dummy grouping
# column for the no-predictor Total case (see weighted_mean_svyby()'s and
# weighted_perc_svyby()'s own header notes) - backtick-quoted at every real
# call site because of its leading dots, which still reads as a bare
# symbol to codetools, same as any other column name here.
# =============================================================================
utils::globalVariables(c(
  ".", "..total..", ".block", ".block_key", ".se", ":=",
  "Base", "Estimate", "Statistics", "Variable",
  "base", "base_count", "base_description", "block_id",
  "col_key", "column_name", "cross_break",
  "data_row_number", "estimate", "estimate_low", "estimate_se", "estimate_upp",
  "is_variable_row", "left_stem", "left_stem2",
  "lev_num", "lev_relevant", "level_col", "max_n",
  "nested_p_method", "nested_p_value", "o_cat", "o_lab", "outcome",
  "outcome_type", "outer_level", "p_cat1", "p_cat1_label", "p_cat2",
  "p_lab1", "p_method", "p_value", "prop", "quantiles_q25", "quantiles_q75",
  "right_stem", "row_type", "sig_diff", "sig_letter", "stat", "stat_code",
  "stat_type", "stem_count", "unweighted_n", "value"
))
