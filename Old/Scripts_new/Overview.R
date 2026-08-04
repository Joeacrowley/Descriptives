# =============================================================================
# Overview.R — simplified reference for the Scripts_new/ functions
#
# This documents Scripts_new/, NOT the older Scripts/ folder Overview.qmd
# describes. The two are separate, self-contained implementations (see
# calc_stats.R's own header note - "nothing in the original Scripts/ folder
# has been touched"); this file doesn't attempt to reconcile them, just to
# describe what's actually in Scripts_new/ as of now.
#
# Deliberately a plain .R script, not a .qmd - meant to be read top to bottom
# or sourced and run interactively, not rendered. Every example below uses
# small, self-contained synthetic data (no dependency on project data files),
# so this can be run as-is to see real output.
#
# Scope: this covers the functions you actually call directly. It does NOT
# re-document every internal helper (standardise_names(), create_bases(),
# common_prefix(), etc.) - see calc_stats.R's own comments for those; they're
# implementation details, not part of the day-to-day interface.
# =============================================================================


# ---- Setup ------------------------------------------------------------------
# Every file below sources calc_stats.R itself at the top (via here::here()),
# so sourcing any ONE of them pulls in everything calc_stats() needs. There's
# no source_folder_r() equivalent in Scripts_new/ - files are small enough in
# number that sourcing what you need directly is simpler.

library(tidyverse)
library(srvyr)
library(huxtable)

source(here::here("Scripts_new", "calc_stats.R"))              # calc_stats(), add_pairwise_sig()
source(here::here("Scripts_new", "pivot_summary.R"))            # pivot_summary() / format_summary()
source(here::here("Scripts_new", "pivot_crosstab.R"))           # pivot_crosstab() / format_crosstab()
source(here::here("Scripts_new", "pivot_nested_crosstab.R"))    # pivot_nested_crosstab() / format_nested_crosstab()
source(here::here("Scripts_new", "pivot_numeric_summary.R"))    # pivot_numeric_summary() / format_numeric_summary()
source(here::here("Scripts_new", "pivot_battery.R"))            # pivot_battery() / format_battery()
source(here::here("Scripts_new", "make_table.R"))                # make_table()
source(here::here("Scripts_new", "make_numeric_summary_table.R")) # make_numeric_summary_table()
source(here::here("Scripts_new", "make_battery_table.R"))         # make_battery_table()
source(here::here("Scripts_new", "export_to_excel.R"))           # export_table_to_excel(), export_tables_to_excel()


# =============================================================================
# 1. calc_stats() — the core function everything else builds on
# =============================================================================
#
# calc_stats(data, outcomes, predictors = NULL, statistics = c("count", "mean"),
#            conf = NULL, base = NULL, pval = NULL, multicode = TRUE,
#            pairwise = FALSE)
#
# Arguments
#   data        - a plain data frame (unweighted stats only) or a srvyr survey
#                 object (as_survey_design()) for weighted stats. Unweighted
#                 stats can still be requested from a survey object.
#   outcomes    - character vector of outcome variable names.
#   predictors  - a list; each element is a character vector of predictor
#                 names. A single bare character vector is auto-wrapped in
#                 list() for you. An element of length 1 is a normal (flat)
#                 breakdown; an element of length 2 (e.g. c("sex","age_group"))
#                 produces a NESTED breakdown (outer x inner). NULL returns an
#                 overall Total only. Multiple elements = multiple predictor
#                 sets, all run and stacked.
#   statistics  - any of: "mean","median","sum","min","max","range","iqr","sd",
#                 "perc","count" (unweighted), and their weighted equivalents
#                 "w_mean","w_median","w_sum","w_iqr","w_sd","w_perc","w_count"
#                 (no w_min/w_max/w_range - see calc_stats.R's own note on why
#                 sample extremes aren't something weighting adjusts).
#                 Numeric statistics are silently skipped for factor outcomes
#                 and vice versa.
#   conf        - "se" for standard errors, "ci" for confidence intervals, or
#                 NULL for neither.
#   base        - output of base_information() - a lookup of base-description
#                 text per variable. NULL omits base descriptions.
#   pval        - if not NULL, a significance test is run and joined in. Only
#                 for FLAT predictor sets - nested sets get their own per-
#                 outer-level p-value automatically (see section 3 below); no
#                 method exists for a flat set's p-value AND a nested set's
#                 in the same call being conflated.
#   multicode   - TRUE (default): auto-detect and collapse variables sharing a
#                 common name+label stem into one multicoded variable.
#   pairwise    - FALSE (default). TRUE runs add_pairwise_sig() automatically
#                 and appends sig_letter/sig_diff columns (and, for the stats
#                 that support it, a covmat list-column) - see section 2.
#
# Output columns
#   cross_break              - predictor variable name(s) joined by " X ", or
#                               "Total" if no predictor.
#   predictor1/p_lab1/p_cat1 - first predictor's name / label / level.
#   predictor2/p_lab2/p_cat2 - second predictor's name/label/level (nested
#                               sets only).
#   outcome / o_lab / o_cat  - outcome variable name / label / level (o_cat is
#                               a duplicate of `stat` for numeric statistics -
#                               nothing to categorise).
#   outcome_type              - "categorical" / "numeric" / "multicoded" - the
#                               authoritative type flag; drives every
#                               pivot_*()'s own dispatch logic.
#   stat                      - which statistic this row is ("mean","w_perc",...).
#   estimate                  - the statistic's value.
#   estimate_se               - standard error (conf = "se" only).
#   estimate_ci                - formatted "low - high" string (conf = "ci" only).
#   estimate_low/estimate_upp - numeric CI bounds, kept even when conf = "ci"
#                               already built the display string.
#   base / base_description   - unweighted base size, and its footnote text.
#   unweighted_n               - cell count.
#   p_method / p_value          - present when pval was requested.
#   sig_letter / sig_diff       - present when pairwise = TRUE (section 2).
#   covmat                      - present when pairwise = TRUE AND this row's
#                                 stat has an exact covariance path (w_perc,
#                                 w_mean only - section 2).

example_data <- tibble(
  score    = c(rnorm(40, 60, 12), rnorm(40, 45, 12)),
  approve  = factor(sample(c("Yes", "No"), 80, replace = TRUE, prob = c(0.6, 0.4))),
  region   = factor(rep(c("North", "South"), each = 40)),
  wt       = runif(80, 0.7, 1.4)
)
example_design <- example_data %>% srvyr::as_survey_design(ids = 1, weights = wt)

calc_stats(example_data, outcomes = "approve", predictors = "region",
           statistics = "perc", conf = "se", pval = TRUE)


# =============================================================================
# 2. Pairwise significance testing (pairwise = TRUE)
# =============================================================================
#
# For each (outcome, cross_break, stat, o_cat) group, every pair of predictor
# levels is z-tested against each other. Two new columns appear:
#   sig_letter - one letter per predictor level (within a cross_break), used
#                as a shared reference key.
#   sig_diff   - for each row, the OTHER levels' letters that this level
#                differs significantly from (alpha = 0.05 by default), ""
#                if tested but nothing significant, NA if not tested at all
#                (e.g. the Total row, or a stat outside pairwise_eligible_stats).
#
# Two different ways the comparison's standard error gets computed, entirely
# determined by the stat - not something you choose:
#
#   EXACT (real joint covariance): w_perc, w_mean only. calc_stats() swaps
#   these to svyby()-based twins (weighted_perc_svyby()/weighted_mean_svyby())
#   under pairwise = TRUE, which attach a real covariance matrix per group
#   (the `covmat` column) via survey::svyby(..., covmat = TRUE). SE_diff is
#   computed properly as Var_i + Var_j - 2*Cov_ij, not assuming independence.
#
#   APPROXIMATE (independent-SE fallback): everything else that's eligible -
#   perc, count, w_count, mean, median, w_median. No joint covariance exists
#   (or, for w_count/count/perc, wasn't built) - SE_diff is combined assuming
#   Cov_ij = 0: sqrt(SE_i^2 + SE_j^2), using each stat's own normal SE (still
#   the real design-based SE where one exists - e.g. w_median's SE still
#   comes from survey::svyquantile()'s Woodruff-CI method; only the CROSS-
#   GROUP covariance is approximated as zero, never the per-group SE itself).
#   Requires conf = "se" or conf = "ci" to have anything to fall back on -
#   otherwise those rows are skipped (a warning is issued, not an error).
#
#   Medians (median/w_median) are ALWAYS on the approximate path - survey's
#   svyquantile() doesn't return influence functions, so covmat = TRUE has
#   nothing to build from for either. This is a hard limitation of the survey
#   package, not a scope choice the way it was for means.
#
# pairwise_eligible_stats (in calc_stats.R) is the full current whitelist:
#   perc, w_perc, count, w_count, mean, w_mean, median, w_median
# sum/sd/iqr/min/max/range are NOT eligible - requesting pairwise = TRUE
# alongside one of those just leaves sig_letter/sig_diff as NA for that row.

calc_stats(example_design, outcomes = "approve", predictors = "region",
           statistics = "w_perc", conf = "se", pairwise = TRUE) %>%
  select(cross_break, p_cat1, estimate, sig_letter, sig_diff)

calc_stats(example_design, outcomes = "score", predictors = "region",
           statistics = "w_mean", conf = "se", pairwise = TRUE) %>%
  select(cross_break, p_cat1, estimate, sig_letter, sig_diff)


# =============================================================================
# 3. p-values (pval = TRUE)
# =============================================================================
# Test selection is automatic, based on outcome type and weighting:
#   categorical, unweighted -> chi-square
#   categorical, weighted   -> adjusted Wald (survey::svychisq)
#   numeric, unweighted     -> Shapiro-Wilk on model residuals decides
#                              Kruskal-Wallis (p < .05) vs Welch's ANOVA
#   numeric, weighted       -> survey::svyranktest / survey::regTermTest
#                              (survey-aware equivalents of the above)
# These are real methodological decisions embedded in the code (thresholds,
# test choices) - see the old Overview.qmd's "Methodological decisions
# requiring sign-off" section; that review hasn't been redone for Scripts_new
# specifically.
#
# Flat predictor sets get ONE p-value for the whole set. Nested sets get one
# PER OUTER LEVEL instead (e.g. "is age associated with the outcome, among
# women" and "...among men" separately) - never one for the whole nested set.


# =============================================================================
# 4. The pivot_*() / format_*() pairs
# =============================================================================
# All take calc_stats() output as `data` (pivot_numeric_summary() is the one
# exception - see below). pivot_*() reshapes into a tidy list (a data tibble
# plus metadata format_*() needs); format_*() turns that into a styled
# huxtable. Pick based on what `predictors` shape you used:

# --- pivot_summary() / format_summary() ---
# No predictor (Total only). Rows = variables (+ levels for categorical
# ones); one column of estimates.
calc_stats(example_data, outcomes = c("score", "approve"),
           statistics = c("mean", "perc"), conf = "se") %>%
  pivot_summary() %>% format_summary()

# --- pivot_crosstab() / format_crosstab() ---
# One or more FLAT predictor sets, side by side. Predictor levels become
# columns. Supports pairwise (legend row + Sig. diff row, SE/CI suppressed
# when pairwise is shown), p-values (one column per predictor set), multiple
# statistics per numeric variable. Categorical variables are limited to ONE
# statistic per call.
calc_stats(example_design, outcomes = "score", predictors = "region",
           statistics = "w_mean", conf = "se", pairwise = TRUE, pval = TRUE) %>%
  pivot_crosstab() %>% format_crosstab()

# --- pivot_nested_crosstab() / format_nested_crosstab() ---
# EXACTLY one nested (2-variable) predictor set - predictors =
# list(c(outer_var, inner_var)). 4-row header (outer label / outer levels /
# inner label / inner levels). Same pairwise/legend/Sig.diff mechanics as
# pivot_crosstab(), same generic column-driven design (confirmed working for
# mean/w_mean pairwise too, not just perc/w_perc).
nested_example <- tibble(
  score     = c(rnorm(20, 70, 8), rnorm(20, 55, 8), rnorm(20, 40, 8), rnorm(20, 25, 8)),
  sex       = factor(rep(c("Male", "Male", "Female", "Female"), each = 20)),
  age_group = factor(rep(c("Young", "Old"), each = 20, times = 2))
)
calc_stats(nested_example, outcomes = "score", predictors = list(c("sex", "age_group")),
           statistics = "mean", conf = "se", pairwise = TRUE) %>%
  pivot_nested_crosstab() %>% format_nested_crosstab()

# --- pivot_numeric_summary() / format_numeric_summary() ---
# Calls calc_stats() ITSELF (the one exception to "takes calc_stats() output")
# - a classic "Table 1" grid: rows = numeric variables, columns = statistics.
# No predictor axis at all - both axes are already spoken for. `weighted`
# picks mean/w_mean etc for you rather than you naming the stat code
# directly.
#
# `statistics` accepts any subset of ALL 8 numeric_summary_stats (defined in
# pivot_numeric_summary.R): mean, median, sum, min, max, range, iqr, sd -
# every one of them is fully supported, not just mean/sd. The default value,
# c("mean", "sd"), is only shown below because you have to pass SOMETHING -
# it's a default, not a restriction, same as calc_stats()'s own default of
# c("count", "mean") isn't a cap on calc_stats() either. Anything outside
# that set of 8 (e.g. "perc"/"count") errors explicitly, since a
# percentage/count has no meaning as "a numeric variable's own statistic".
pivot_numeric_summary(example_data, outcomes = "score",
                       statistics = c("mean", "median", "sd", "min", "max", "range", "iqr")) %>%
  format_numeric_summary()

# --- pivot_battery() / format_battery() ---
# A "battery grid": rows = several outcome variables sharing one response
# scale (e.g. Likert items), columns = the shared response categories, one
# %/N per cell. predictors = NULL is enforced (no room for a 3rd axis).
# Exactly ONE statistic per call (perc, count, w_perc, or w_count).
# SE/CI (conf = "se"/"ci") is supported - shown as an extra row under any
# item that has one, reusing the existing blank-header unit column to read
# "SE"/"95% CI" on that row instead of "%"/"N" (no separate Statistics
# column here, unlike pivot_crosstab()). One-call wrapper available now too
# - see make_battery_table() in section 5.
battery_example <- tibble(
  item1 = factor(sample(c("Agree", "Neutral", "Disagree"), 60, replace = TRUE)),
  item2 = factor(sample(c("Agree", "Neutral", "Disagree"), 60, replace = TRUE))
)
calc_stats(battery_example, outcomes = c("item1", "item2"), statistics = "perc") %>%
  pivot_battery() %>% format_battery()


# =============================================================================
# 5. Convenience wrappers
# =============================================================================
#
# make_table(data, outcomes, predictors = NULL, statistics = c("count","mean"),
#            conf = NULL, base = NULL, pval = NULL, multicode = TRUE,
#            formatted = TRUE)
# Chains calc_stats() -> the right pivot_*() -> the right format_*(), inferred
# purely from `predictors`' shape (NULL -> summary, one nested set -> nested
# crosstab, otherwise -> crosstab). formatted = FALSE stops after the pivot
# step and returns that list instead of a huxtable.
#
# KNOWN GAP: make_table() has no `pairwise` argument - calc_stats()'s own
# pairwise = TRUE isn't exposed through it at all. For a pairwise-annotated
# table you currently have to call calc_stats(pairwise = TRUE) and the
# matching pivot_*()/format_*() pair directly (as in section 4's examples),
# not make_table().

make_table(example_data, outcomes = "approve", predictors = "region",
           statistics = "perc", conf = "se")

# make_numeric_summary_table(data, outcomes, statistics = c("mean","sd"),
#                             weighted = FALSE, base = NULL, formatted = TRUE)
# Same one-call convenience, specifically for pivot_numeric_summary() (kept
# separate from make_table() rather than folded in - see that file's own
# header note on the dispatch collision this would otherwise cause).
# `statistics` here is the exact same argument, passed straight through to
# pivot_numeric_summary() unchanged - same full set of 8 available (see that
# section's own note above), c("mean", "sd") is just its default too, not a
# second, narrower restriction layered on top.

make_numeric_summary_table(example_data, outcomes = "score",
                            statistics = c("mean", "median", "sd", "min", "max", "range", "iqr"))

# make_battery_table(data, outcomes, statistics = "perc", conf = NULL,
#                     base = NULL, multicode = FALSE, formatted = TRUE)
# Same one-call convenience, for pivot_battery() - also kept separate from
# make_table() rather than folded in, same underlying reason as
# make_numeric_summary_table(): a real dispatch collision, not just an edge
# case. make_table() already routes predictors = NULL to pivot_summary(),
# and a battery grid is ALSO a no-predictors table - `predictors`' shape
# alone can't tell the two apart any more than it could tell a numeric
# summary grid apart from either. `conf` IS exposed here (unlike
# make_numeric_summary_table(), which has no conf argument at all, since
# pivot_numeric_summary() never supported SE/CI) - pivot_battery() gained
# real SE/CI support (section 4 above), so it's worth passing through.

make_battery_table(battery_example, outcomes = c("item1", "item2"),
                    statistics = "perc", conf = "se")


# =============================================================================
# 6. Excel export
# =============================================================================
# One generic converter, works on ANY huxtable (make_table() output,
# make_numeric_summary_table() output, make_battery_table() output, or a
# format_*() call directly) - no per-table-type branching needed.
#
#   export_table_to_excel(ht, file, sheet = "Table 1", title = NULL)
#     - writes one huxtable to one Excel file/sheet.
#   export_tables_to_excel(hux_list, file, sheet_names = NULL, titles = NULL, ...)
#     - writes several huxtables to one workbook, one sheet each.
#
# KNOWN LIMITATION: cells export as TEXT, not real numbers (see export_to_excel.R's
# own header note - the original Deprecated_Tables code solved this via
# addStyle()-based number formatting; that fix hasn't been ported here yet).

# export_table_to_excel(make_table(example_data, outcomes = "approve",
#                                   predictors = "region", statistics = "perc"),
#                        file = "example_table.xlsx")


# =============================================================================
# 7. Known limitations / open items (as of this file's writing)
# =============================================================================
# - make_table() doesn't expose calc_stats()'s pairwise = TRUE argument.
# - Medians (median/w_median) can only ever use the approximate pairwise path
#   - no exact covariance is possible via the survey package, not a scope
#     choice.
# - sum/sd/iqr/min/max/range are not pairwise-eligible at all.
# - Nested predictor sets get no p-value method of their own for the FLAT-
#   style single p-value - only the per-outer-level p-value described in
#   section 3.
# - export_to_excel.R writes numbers as text, not real Excel numeric cells.
# - The "Methodological decisions requiring sign-off" list in the old
#   Overview.qmd (Shapiro-Wilk threshold, chi-square with no sparse-cell
#   fallback, multicode same-base-size assumption) applies unchanged here -
#   Scripts_new ported this logic without revisiting those decisions.
