# =============================================================================
# The maths functions calc_stat_engine() wraps, and their registry.
#
# Adding a stat = one function here + one line in the registry below.
# The registry stays at the bottom: it refers to the functions above it.
# =============================================================================

# ---- maths functions ----------------------------------------------------------
# One fixed signature: (grouped, outcome, conf). `grouped` is already
# NA-filtered and grouped by the predictor(s). Return one row per group with
# `estimate`, `base`, and optionally estimate_se/estimate_low/estimate_upp.
# Missing SE/CI columns are fine - the engine fills them with NA.
#
# Weighted stats get a survey object and pass `conf` to the survey package
# as `vartype`, so it only computes what was asked for.

# ---- mean ----

stat_mean <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(estimate     = mean(.data[[outcome]], na.rm = TRUE),
              base         = n(),
              estimate_se  = sd(.data[[outcome]], na.rm = TRUE) / sqrt(base),
              estimate_low = estimate - 1.96 * estimate_se,
              estimate_upp = estimate + 1.96 * estimate_se,
              .groups      = "drop") # Always return an ungrouped tibble.
}

stat_w_mean <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(estimate = survey_mean(!!sym(outcome), vartype = conf),
              base     = unweighted(n()),
              .groups  = "drop")
}


# ---- median ----
# No analytic SE/CI for a plain median, so wrappedtools does it - only when
# asked, as the bootstrap CI is slow. Groups of 5 or fewer get an NA CI.

stat_median <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(
      estimate    = median(.data[[outcome]], na.rm = TRUE),
      base        = n(),
      estimate_se = if (identical(conf, "se")) wrappedtools::medianse(.data[[outcome]]) else NA_real_,
      # An unnamed tibble in summarise() becomes columns: estimate_low, estimate_upp.
      if (identical(conf, "ci") && n() > 5) {
        ci <- wrappedtools::median_cl_boot(.data[[outcome]])
        tibble(estimate_low = ci$CIlow, estimate_upp = ci$CIhigh)
      } else {
        tibble(estimate_low = NA_real_, estimate_upp = NA_real_)
      },
      .groups = "drop")
}

# zap_labels(): survey_median() can't take haven-labelled values.
stat_w_median <- function(grouped, outcome, conf) {
  grouped %>%
    mutate("{outcome}" := haven::zap_labels(.data[[outcome]])) %>%
    summarise(estimate = survey_median(!!sym(outcome), vartype = conf),
              base     = unweighted(n()),
              .groups  = "drop")
}


# ---- sum ----

stat_sum <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(estimate     = sum(.data[[outcome]], na.rm = TRUE),
              base         = n(),
              estimate_se  = sd(.data[[outcome]], na.rm = TRUE) * sqrt(base),
              estimate_low = estimate - 1.96 * estimate_se,
              estimate_upp = estimate + 1.96 * estimate_se,
              .groups      = "drop")
}

# zap_labels(): survey_total() can't take haven-labelled values.
stat_w_sum <- function(grouped, outcome, conf) {
  grouped %>%
    mutate("{outcome}" := haven::zap_labels(.data[[outcome]])) %>%
    summarise(estimate = survey_total(!!sym(outcome), vartype = conf),
              base     = unweighted(n()),
              .groups  = "drop")
}


# ---- min / max / range ----
# Unweighted only - a sample's observed min/max isn't a population estimate.

stat_min <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(estimate = min(.data[[outcome]], na.rm = TRUE),
              base     = n(),
              .groups  = "drop")
}

stat_max <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(estimate = max(.data[[outcome]], na.rm = TRUE),
              base     = n(),
              .groups  = "drop")
}

stat_range <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(estimate = max(.data[[outcome]], na.rm = TRUE) -
                         min(.data[[outcome]], na.rm = TRUE),
              base     = n(),
              .groups  = "drop")
}


# ---- IQR ----
# Point estimate only.

stat_iqr <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(estimate = IQR(.data[[outcome]], na.rm = TRUE),
              base     = n(),
              .groups  = "drop")
}

# survey_quantile() names its columns "<name>_q25"/"<name>_q75".
stat_w_iqr <- function(grouped, outcome, conf) {
  grouped %>%
    mutate("{outcome}" := haven::zap_labels(.data[[outcome]])) %>%
    summarise(quantiles = survey_quantile(!!sym(outcome), c(0.25, 0.75), vartype = NULL),
              base      = unweighted(n()),
              .groups   = "drop") %>%
    mutate(estimate = quantiles_q75 - quantiles_q25) %>%
    select(-quantiles_q25, -quantiles_q75)
}


# ---- standard deviation ----
# Point estimate only.

stat_sd <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(estimate = sd(.data[[outcome]], na.rm = TRUE),
              base     = n(),
              .groups  = "drop")
}

# vartype = NULL: sqrt() would otherwise also root the variance's SE,
# which isn't the SD's SE.
stat_w_sd <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(estimate = sqrt(survey_var(!!sym(outcome), na.rm = TRUE, vartype = NULL)),
              base     = unweighted(n()),
              .groups  = "drop")
}


# ---- percentage ----
# Categorical: `grouped` is grouped by predictor(s) + outcome. "drop_last"
# leaves it grouped by the predictor(s), so sums are within each predictor group.

# Wilson interval for x cases out of n, from base R's prop.test(). Unlike
# p +/- 1.96 * SE, it stays within 0-1 and still has some width at 0% or
# 100%. correct = FALSE gives the Wilson interval; suppressWarnings() hides
# the "Chi-squared approximation may be incorrect" warning for small counts.
wilson_ci <- function(x, n) {
  ci <- map2(x, n, \(x, n) suppressWarnings(stats::prop.test(x, n, correct = FALSE))$conf.int)
  list(low = map_dbl(ci, 1), upp = map_dbl(ci, 2))
}

stat_perc <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(unweighted_n = n(), .groups = "drop_last") %>%
    mutate(estimate     = unweighted_n / sum(unweighted_n),
           estimate_se  = sqrt(estimate * (1 - estimate) / sum(unweighted_n)),
           estimate_low = wilson_ci(unweighted_n, sum(unweighted_n))$low,
           estimate_upp = wilson_ci(unweighted_n, sum(unweighted_n))$upp,
           base         = sum(unweighted_n)) %>%
    ungroup()
}

stat_w_perc <- function(grouped, outcome, conf) {
  grouped %>%
    # proportion = TRUE is srvyr's default; stating it stops a once-a-session message.
    summarise(estimate     = survey_prop(vartype = conf, proportion = TRUE),
              unweighted_n = unweighted(n()),
              .groups      = "drop_last") %>%
    mutate(base = sum(unweighted_n)) %>%
    ungroup()
}


# ---- count ----

stat_count <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(estimate = n(), .groups = "drop_last") %>%
    mutate(base         = sum(estimate),
           unweighted_n = estimate,
           prop         = estimate / base,
           estimate_se  = sqrt(base * prop * (1 - prop)),
           estimate_low = estimate - 1.96 * estimate_se,
           estimate_upp = estimate + 1.96 * estimate_se) %>%
    select(-prop) %>%
    ungroup()
}

stat_w_count <- function(grouped, outcome, conf) {
  grouped %>%
    summarise(estimate     = survey_total(vartype = conf),
              unweighted_n = unweighted(n()),
              .groups      = "drop_last") %>%
    mutate(base = sum(unweighted_n)) %>%
    ungroup()
}


# ---- registry -----------------------------------------------------------------
# Everything the engine needs to know about a stat. `type` replaces the old
# group_on_outcome flag; `weighted` replaces str_starts(stat, "w_").
#
# The order here is the display order of numeric stats (the engine's
# cat_order): centre and spread, then extremes, then total. Each weighted
# stat sits straight after its unweighted version.

stat_registry <- list(
  mean     = list(fn = stat_mean,     type = "numeric",     weighted = FALSE),
  w_mean   = list(fn = stat_w_mean,   type = "numeric",     weighted = TRUE),
  sd       = list(fn = stat_sd,       type = "numeric",     weighted = FALSE),
  w_sd     = list(fn = stat_w_sd,     type = "numeric",     weighted = TRUE),
  median   = list(fn = stat_median,   type = "numeric",     weighted = FALSE),
  w_median = list(fn = stat_w_median, type = "numeric",     weighted = TRUE),
  iqr      = list(fn = stat_iqr,      type = "numeric",     weighted = FALSE),
  w_iqr    = list(fn = stat_w_iqr,    type = "numeric",     weighted = TRUE),
  min      = list(fn = stat_min,      type = "numeric",     weighted = FALSE),
  max      = list(fn = stat_max,      type = "numeric",     weighted = FALSE),
  range    = list(fn = stat_range,    type = "numeric",     weighted = FALSE),
  sum      = list(fn = stat_sum,      type = "numeric",     weighted = FALSE),
  w_sum    = list(fn = stat_w_sum,    type = "numeric",     weighted = TRUE),
  perc     = list(fn = stat_perc,     type = "categorical", weighted = FALSE),
  w_perc   = list(fn = stat_w_perc,   type = "categorical", weighted = TRUE),
  count    = list(fn = stat_count,    type = "categorical", weighted = FALSE),
  w_count  = list(fn = stat_w_count,  type = "categorical", weighted = TRUE)
)
