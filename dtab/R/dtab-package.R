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
#'   kruskal.test lm median na.omit oneway.test p.adjust p.adjust.methods
#'   residuals sd
#'   setNames shapiro.test t.test vcov
#' @importFrom utils combn head
"_PACKAGE"


# NSE column names, not real globals - silences R CMD check's "no visible
# binding" NOTE for bare tidy-eval symbols (mutate()/filter()/group_by()/
# pivot_wider() column names, which codetools can't resolve statically).
# Standard tidyverse idiom - see
# https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-select-rename-relocate/
utils::globalVariables(c(
  ".", ":=", "Base", "base", ".block", "block_id", "block_number", "col_key",
  "column_name", "comparison", "comparison_group_1", "comparison_group_2",
  "comparison_letter", "comparison_pcat1", "comparison_pcat2", "cross_break",
  "data_row_number", ".env", "Estimate", "estimate", "estimate_low",
  "estimate_se", "estimate_upp", "first_row", "first_stat", "g1_p_cat1", "g1_p_cat2",
  "g2_p_cat1", "g2_p_cat2", "group1", "group2", "is_total",
  "is_variable_row", "label", "level_col", "line", "method", "multicode_group",
  "n_bases", "o_base", "o_cat", "o_lab", "option", "option_number",
  "outcome", "outcome_type", "outer_level", "p.value", "p_adj", "p_cat1",
  "p_cat1_label", "p_cat2", "p_lab1", "p_method", "p_value", "predictor",
  "prop", "quantiles_q25", "quantiles_q75", "question", "ref_group",
  "row_position", "row_type", "same_bases", "sig_diff", "sig_letter",
  "sig_method", "sig_untested", "significant", "stat", "stat_code", "stat_tested",
  "stat_type", "Statistics", "table_order", "tested", "unweighted_n",
  "value", "Variable"
))
