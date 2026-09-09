# dtab-package.R: package-level documentation and the blanket @import
# declarations every other file in R/ relies on (tidyverse functions are
# called unqualified throughout, so imported wholesale rather than
# @importFrom-ed one by one).
#
# rlang is @importFrom-only, NOT @import - importing its whole namespace
# collides with purrr (both export flatten*/splice/invoke/%@%), so only
# the one function actually used bare (sym()) is imported individually.
# Do not change this to @import rlang.
#
# survey::svychisq(), broom::tidy(), and every other survey::/huxtable::/
# broom:: call must stay explicitly namespace-qualified - none of the three
# is @import-ed, only declared in DESCRIPTION, which is enough for `::` to
# resolve but not for a bare call.
#
# After any change to these @import/@export tags, run
# devtools::document() to regenerate NAMESPACE and the Rd files - nothing
# here is hand-maintained.
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
#'   kruskal.test lm median na.omit oneway.test p.adjust residuals sd
#'   setNames shapiro.test t.test vcov
#' @importFrom utils combn head
"_PACKAGE"


# NSE column names, not real globals - silences R CMD check's "no visible
# binding" NOTE for bare tidy-eval symbols (mutate()/filter()/group_by()/
# pivot_wider() column names, which codetools can't resolve statically).
# Standard tidyverse idiom - see
# https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-select-rename-relocate/
utils::globalVariables(c(
  ".", "..total..", ".block", ".block_key", ".se", ":=",
  "Base", "Estimate", "Statistics", "Variable",
  "base", "base_count", "base_description", "block_id",
  "col_key", "column_name", "comparison", "comparison_group_1",
  "comparison_group_2", "comparison_letter", "comparison_pcat1",
  "comparison_pcat2", "cross_break",
  "data_row_number", "estimate", "estimate_low", "estimate_se", "estimate_upp",
  "group1", "group2",
  "is_variable_row", "left_stem", "left_stem2",
  "lev_num", "lev_relevant", "level_col", "max_n",
  "nested_p_method", "nested_p_value", "o_cat", "o_lab", "outcome",
  "outcome_type", "outer_level", "p.value", "p_adj", "p_cat1", "p_cat1_label", "p_cat2",
  "p_lab1", "p_method", "p_value", "predictor", "prop", "quantiles_q25", "quantiles_q75",
  "ref_group", "right_stem", "row_type", "sig_diff", "sig_letter", "stat", "stat_code",
  "stat_type", "stem_count", "unweighted_n", "value"
))
