# =============================================================================
# compare_pairwise_vs_svycontrast.R
#
# Extends your uploaded "Pairwise sig difs.R" to directly compare
# add_pairwise_sig()'s approximate (independent-SE) approach against the
# real svyby(covmat = TRUE) + svycontrast() joint-covariance approach it's
# standing in for, on the SAME data.
#
# I can't run this file myself - CRAN and the webR/r-universe package
# mirrors are both network-blocked in this sandbox, so `survey` isn't
# installable here. (I did get a real base-R interpreter running via webR
# this session - a first - which let me Monte Carlo-verify the underlying
# *mechanism* independently of the survey package; see the writeup that
# came with this file. But the actual svyby()/svycontrast() calls below
# need your own R + survey install to execute.) Run this interactively.
#
# THE MECHANISM, in one paragraph:
# add_pairwise_sig() treats predictor-level estimates as independent -
# SE_diff = sqrt(SE1^2 + SE2^2) - which silently assumes
# Cov(estimate_i, estimate_j) = 0. That's exactly true when predictor
# levels are disjoint, unclustered subsets (svydesign(..., id = ~1), your
# original example's design - no PSU is shared between any two levels, so
# there's nothing to covary). It's also still exactly true under clustering
# if the predictor's levels happen to align with cluster boundaries (each
# cluster belongs entirely to one level - still nothing shared). It becomes
# NONZERO once the design has clustering AND the predictor cuts across
# cluster boundaries - some clusters contain respondents from more than one
# level - because a shared cluster-level random effect then pushes every
# level drawing from that cluster in the same direction. In my Monte Carlo
# checks that covariance came out positive (as it does in most real designs
# with a positive intra-cluster correlation), which means naive/independent
# SE_diff comes out too LARGE relative to the truth - add_pairwise_sig() is
# the CONSERVATIVE direction on that kind of design (misses real
# differences rather than manufacturing false ones). That's a property of
# the sign of the covariance in that simulation, not a guarantee - don't
# treat "conservative" as automatic for your own data.
#
# PART 1 (apistrat, id = ~1) should show close-to-exact agreement between
# the two approaches - same design as your original file.
# PART 2 (apiclus1, id = ~dnum) uses the same meals3 predictor, but now
# schools in the same district can land in different meals3 tertiles, so
# districts are shared across predictor levels - expect a visible,
# systematic gap here.
# =============================================================================

library(survey)
library(tidyverse)
source(here::here("Scripts_new", "calc_stats.R"))
source(here::here("Scripts_new", "add_pairwise_sig.R"))


# ---- your own pairwise_svycontrast(), copied verbatim from the uploaded file
pairwise_svycontrast <- function(props, outcome) {
  all_names <- names(coef(props))
  outcome_names <- all_names[grepl(outcome, all_names)]
  pairs <- combn(outcome_names, 2, simplify = FALSE)
  results <- lapply(pairs, function(pair) {
    contrast <- setNames(c(1, -1), pair)
    result <- svycontrast(props, contrast)
    z <- as.numeric(result) / as.numeric(SE(result))
    p <- 2 * pnorm(abs(z), lower.tail = FALSE)
    data.frame(group1 = pair[1], group2 = pair[2],
               estimate = as.numeric(result), SE = as.numeric(SE(result)),
               z = z, p = p)
  })
  do.call(rbind, results)
}


# ---- pairs-down version of add_pairwise_sig()'s internal
# ---- pairwise_test_one_group() math, but returning full estimate/SE/z/p per
# ---- pair (not just letters) so it lines up with pairwise_svycontrast()'s
# ---- own output shape for a direct comparison. Not meant to replace
# ---- add_pairwise_sig() - just exposes what it computes internally.
independent_se_pairwise <- function(stats_table, predictor, outcome_var, level) {
  rows <- stats_table %>%
    filter(cross_break == predictor, outcome == outcome_var, o_cat == level) %>%
    mutate(.se = derive_se(.)) %>%
    arrange(p_cat1)

  pairs <- combn(rows$p_cat1, 2, simplify = FALSE)
  results <- lapply(pairs, function(pair) {
    i <- which(rows$p_cat1 == pair[1]); j <- which(rows$p_cat1 == pair[2])
    diff    <- rows$estimate[i] - rows$estimate[j]
    se_diff <- sqrt(rows$.se[i]^2 + rows$.se[j]^2)
    z <- diff / se_diff
    p <- 2 * pnorm(abs(z), lower.tail = FALSE)
    data.frame(group1 = pair[1], group2 = pair[2],
               estimate = diff, SE = se_diff, z = z, p = p)
  })
  do.call(rbind, results)
}


compare_one_design <- function(design_label, svy_data, id_formula, weight_formula,
                                predictor, outcome_var = "both", level = "Yes",
                                extra_args = list()) {

  cat("\n============================================================\n")
  cat(design_label, "\n")
  cat("============================================================\n")

  des <- do.call(svydesign, c(list(data = svy_data, id = id_formula, weights = weight_formula), extra_args))

  outcome_formula   <- as.formula(paste0("~", outcome_var))
  predictor_formula <- as.formula(paste0("~", predictor))
  props <- svyby(outcome_formula, predictor_formula, design = des,
                 FUN = svymean, keep.var = TRUE, vartype = "var", covmat = TRUE)

  # group1/group2 come back as svyby()'s own coefficient names
  # ("Low:bothYes", not "Low") - strip the ":<outcome><level>" suffix to get
  # back to the plain predictor level, so this lines up with approx_pw's
  # (which never had that suffix in the first place).
  true_pw <- pairwise_svycontrast(props, paste0(outcome_var, level))
  suffix_pattern <- paste0(":", outcome_var, level, "$")
  true_pw$level1 <- sub(suffix_pattern, "", true_pw$group1)
  true_pw$level2 <- sub(suffix_pattern, "", true_pw$group2)
  true_pw$pair   <- paste(true_pw$level1, true_pw$level2, sep = " vs ")
  # combn() on this side runs in factor-level order; combn() on approx_pw's
  # side (below) runs in alphabetical order (arrange(p_cat1)) - same set of
  # pairs, but "Low vs High" here can come out as "High vs Low" there. A
  # plain string match on `pair` would silently drop those rows to NA, so
  # join on an order-independent key instead (sorted alphabetically) and
  # keep `pair` only for display.
  true_pw$pair_key <- mapply(function(a, b) paste(sort(c(a, b)), collapse = " | "),
                              true_pw$level1, true_pw$level2)

  # srvyr::as_survey_design() takes bare column names / 1 via tidyselect, not
  # formulas - `ids = !!id_formula` was passing a ~1/~dnum formula straight
  # into that NSE layer, which is what srvyr_select_vars() was rejecting.
  # Wrapping the already-built base `des` sidesteps the mismatch entirely,
  # and guarantees the design calc_stats() sees is identical to the one
  # svyby() used above - not just built from the same formulas twice.
  survey_design_srvyr <- srvyr::as_survey(des)
  stats_table <- calc_stats(survey_design_srvyr, outcomes = outcome_var,
                             predictors = predictor, statistics = "w_perc",
                             conf = "se", multicode = FALSE)
  approx_pw <- independent_se_pairwise(stats_table, predictor, outcome_var, level)
  approx_pw$pair_key <- mapply(function(a, b) paste(sort(c(a, b)), collapse = " | "),
                                approx_pw$group1, approx_pw$group2)

  compare <- true_pw %>%
    select(pair, pair_key, true_estimate = estimate, true_SE = SE, true_z = z, true_p = p) %>%
    left_join(approx_pw %>% select(pair_key, approx_SE = SE, approx_z = z, approx_p = p),
              by = "pair_key") %>%
    select(-pair_key) %>%
    mutate(SE_ratio = approx_SE / true_SE,
           verdict_agrees = (true_p < 0.05) == (approx_p < 0.05))

  cat("\n-- svyby()/svycontrast() (true, joint covariance) vs\n")
  cat("-- add_pairwise_sig()'s independent-SE approximation --\n")
  print(compare, row.names = FALSE, digits = 4)

  cat(sprintf("\nAll %d pairs agree on significance at alpha=0.05: %s\n",
              nrow(compare), all(compare$verdict_agrees)))

  compare
}


# ---- PART 1: unclustered - id = ~1, same design as your original example --
data(api, package = "survey")
apistrat$meals3 <- factor(dplyr::ntile(apistrat$meals, 3), labels = c("Low", "Mid", "High"))

part1 <- compare_one_design(
  "PART 1 - apistrat, id = ~1 (unclustered) - expect close agreement",
  apistrat, ~1, ~pw, "meals3"
)


# ---- PART 2: clustered - id = ~dnum, meals3 cuts across districts ---------
data(api, package = "survey")   # reload fresh - apiclus1 this time
apiclus1$meals3 <- factor(dplyr::ntile(apiclus1$meals, 3), labels = c("Low", "Mid", "High"))

part2 <- compare_one_design(
  "PART 2 - apiclus1, id = ~dnum (clustered) - expect a visible gap",
  apiclus1, ~dnum, ~pw, "meals3", extra_args = list(fpc = ~fpc)
)


# ---- summary ---------------------------------------------------------------
cat("\n============================================================\n")
cat("SUMMARY\n")
cat("============================================================\n")
cat(sprintf("PART 1 (unclustered)          - mean SE_ratio (approx/true): %.3f\n",
            mean(part1$SE_ratio)))
cat(sprintf("PART 2 (clustered, mixed)     - mean SE_ratio (approx/true): %.3f\n",
            mean(part2$SE_ratio)))
cat("\nSE_ratio near 1.0 => the independent-SE shortcut is fine here.\n")
cat("SE_ratio far from 1.0 => this design's clustering/predictor overlap\n")
cat("matters enough that add_pairwise_sig() alone isn't safe to trust -\n")
cat("worth running the real svyby(covmat=TRUE)+svycontrast() route (as\n")
cat("above) for that specific call instead.\n")
