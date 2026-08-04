# =============================================================================
# pivot_numeric_summary() / format_numeric_summary() — a numeric-only summary
# table: rows are variables, columns are statistics (Mean, SD, Min, ...), one
# number per cell. No predictor/crossbreak dimension - always a single
# overall ("Total") summary, since a "rows = variables, columns = statistics"
# layout has no room left for a third (predictor) axis without turning into
# something closer to pivot_crosstab() reshaped sideways. Deliberately closer
# to a classic "Table 1" descriptive summary than either of the other two
# pivot_*() functions in this project.
#
# STRUCTURALLY DIFFERENT from pivot_summary()/pivot_crosstab(): both of those
# take calc_stats() output as their `data` argument and never call
# calc_stats() themselves. pivot_numeric_summary() calls calc_stats() itself
# instead, because its `weighted` argument has to choose calc_stats()'s exact
# stat codes (mean vs w_mean, sd vs w_sd, ...) before calc_stats() ever runs -
# that decision has nowhere else to live. weighted = TRUE and weighted = FALSE
# never both appear in the output at once (you get one family of statistics,
# not a mix) - min/max/range are the one exception, since calc_stats.R never
# added w_min/w_max/w_range (a sample's observed extremes aren't population
# estimates weighting would adjust - see calc_stats.R's header comment on
# unweighted_min/max/range); under weighted = TRUE those three just fall back
# to their only (unweighted) version, same reasoning applied one level up.
#
# Only numeric-appropriate statistics are allowed at all - perc/count (and
# their weighted forms) are rejected outright, and every requested outcome
# must itself be numeric (checked via check_all_numeric(), see calc_stats.R) -
# a categorical variable simply has no home in this table.
# =============================================================================

library(tidyverse)
source(here::here("Scripts_new", "calc_stats.R"))
source(here::here("Scripts_new", "pivot_summary.R"))   # reuse tidy_statistic_description(), format_statistic()


# ---- pivot_numeric_summary: call calc_stats() and reshape wide -------------

# The 8 statistics this table can show. Deliberately excludes perc/count (and
# w_perc/w_count) - this table is numeric-only by design, and a percentage/
# count has no meaning as "the value of a numeric variable's own statistic"
# the way mean/sd/etc do.
numeric_summary_stats <- c("mean", "median", "sum", "min", "max", "range", "iqr", "sd")

# Weighted equivalents for the statistics that have one. min/max/range are
# deliberately absent - see the header note above.
numeric_summary_weighted_equiv <- c(
  mean = "w_mean", median = "w_median", sum = "w_sum", sd = "w_sd", iqr = "w_iqr"
)

pivot_numeric_summary <- function(data, outcomes, statistics = c("mean", "sd"),
                                   weighted = FALSE, base = NULL) {

  if (!isTRUE(vars_exist(data = data, variable_list = outcomes))) {
    stop("Some variables you want to use do not exist in the data frame.")
  }
  if (!isTRUE(check_all_numeric(data = data, variable_list = outcomes))) {
    stop("All outcomes must be numeric - pivot_numeric_summary() has no room for categorical statistics.")
  }
  if (!all(statistics %in% numeric_summary_stats)) {
    stop("statistics must be one or more of: ", paste(numeric_summary_stats, collapse = ", "))
  }
  if (isTRUE(weighted) && !any(str_detect(class(data), "survey"))) {
    stop("weighted = TRUE requires a survey design object (as_survey_design()) as data.")
  }

  # The actual calc_stats() stat codes to request - w_mean/w_sd/... under
  # weighted = TRUE wherever a weighted version exists, the plain name
  # everywhere else (including min/max/range even when weighted = TRUE, since
  # they have no other version to fall back to).
  requested_stats <- statistics
  if (isTRUE(weighted)) {
    has_weighted_equiv <- statistics %in% names(numeric_summary_weighted_equiv)
    requested_stats[has_weighted_equiv] <- numeric_summary_weighted_equiv[statistics[has_weighted_equiv]]
  }

  stats_table <- calc_stats(data, outcomes = outcomes, predictors = NULL,
                             statistics = requested_stats, conf = NULL,
                             base = base, pval = NULL, multicode = FALSE)

  # No o_cat fragmentation to worry about here, unlike pivot_summary()/
  # pivot_crosstab(): none of these 8 statistics group_on_outcome, so
  # calc_stats() never splits one outcome into multiple category rows the
  # way perc/count do - each outcome contributes exactly one row per
  # requested statistic, which is what makes a plain pivot_wider() safe
  # without any of pivot_summary()'s per-block/label-row machinery.
  labelled <- stats_table %>% tidy_statistic_description()

  # base is per-outcome, not per-statistic - calc_stat_engine() filters to
  # this outcome's own complete cases before computing ANY of its requested
  # statistics (see calc_stat_engine()'s `filtered`), so every statistic
  # requested for one outcome shares the same base. distinct() here is a
  # safety net for that invariant, not a real aggregation.
  base_by_outcome <- labelled %>% distinct(outcome, base) %>% rename(Base = base)

  pivoted <- labelled %>%
    select(outcome, o_lab, Statistics, estimate) %>%
    pivot_wider(id_cols = c(outcome, o_lab), names_from = Statistics, values_from = estimate) %>%
    left_join(base_by_outcome, by = "outcome") %>%
    rename(Variable = o_lab) %>%
    select(-outcome)

  # format_numeric_summary() needs to know which stat_code produced each
  # COLUMN (for format_statistic()'s digit rules) - unlike pivot_summary()/
  # pivot_crosstab(), where stat_code varies per ROW and can just be a data
  # column, here it varies per COLUMN, which doesn't fit as a data column at
  # all. Returned alongside the data instead, as a named vector: names are
  # the display column names pivot_wider() just created ("Mean", "SD", ...),
  # values are the underlying stat codes ("mean", "sd", ...) - built by
  # running the same requested_stats through tidy_statistic_description()
  # again on their own, so the labels are guaranteed to match exactly what
  # pivot_wider() used as column names above.
  stat_lookup <- setNames(requested_stats,
                           tidy_statistic_description(tibble(stat = requested_stats))$Statistics)

  list(pivoted, stat_lookup)
}


# ---- format_numeric_summary: style pivot_numeric_summary() output ----------
#
# Much simpler than format_summary()/format_crosstab(): every row here is a
# real data row (no label rows, no base rows, no conf-as-inserted-row - this
# first pass is point-estimates-only, matching min/max/range/iqr/sd's own
# scope decision one level up), so there's no row_type/block logic needed at
# all. NA renders as "-" uniformly (a genuinely missing statistic on a real
# variable, e.g. an all-NA column) rather than pivot_summary()'s blank-vs-"-"
# distinction, which exists there specifically to tell a label row's
# structural blank apart from a data row's real missingness - a distinction
# that has nothing to key off here since every row is a data row.
format_numeric_summary <- function(pivot_result) {

  data        <- pivot_result[[1]]
  stat_lookup <- pivot_result[[2]]
  stat_cols   <- names(stat_lookup)

  # ---- digit formatting, per column, keyed off that column's own stat_code ----
  for (col in stat_cols) {
    data[[col]] <- format_statistic(data[[col]], stat_lookup[[col]])
  }
  data$Base <- if_else(is.na(data$Base), NA_character_,
                        formatC(data$Base, digits = 0, big.mark = ",", format = "f"))

  ht <- data %>% huxtable::as_hux(add_colnames = TRUE)

  variable_col <- which(names(ht) == "Variable")
  stat_col_idx <- which(names(ht) %in% stat_cols)
  base_col     <- which(names(ht) == "Base")

  ht <- ht %>%
    huxtable::set_na_string(value = "-") %>%
    huxtable::set_align(huxtable::everywhere, variable_col, "left") %>%
    huxtable::set_align(huxtable::everywhere, c(stat_col_idx, base_col), "right") %>%
    huxtable::set_all_borders(huxtable::everywhere, huxtable::everywhere,
                               huxtable::brdr(0.5, "solid", "grey85")) %>%
    huxtable::set_bottom_border(1, huxtable::everywhere, huxtable::brdr(1, "solid", "grey40")) %>%
    huxtable::set_all_padding(1) %>%
    huxtable::set_font_size(8) %>%
    huxtable::set_font("Arial") %>%
    huxtable::set_bold(row = 1, col = huxtable::everywhere) %>%
    huxtable::set_background_color(1, huxtable::everywhere, "grey95") %>%
    huxtable::set_valign("middle") %>%
    huxtable::stripe_rows(stripe1 = "#f5f7fa", stripe2 = "#ffffff")

  # stripe_rows() re-tints row 1 arbitrarily (same fix as format_summary()/
  # format_crosstab() both needed) - reassert the header styling after it.
  ht <- ht %>%
    huxtable::set_bold(1, huxtable::everywhere, TRUE) %>%
    huxtable::set_background_color(1, huxtable::everywhere, "grey95")

  ht
}
