# =============================================================================
# test_weighted_mean_svyby.R
#
# Compares weighted_mean() (the existing srvyr-based implementation) against
# weighted_mean_svyby() (the svyby()-based one, added so calc_stats(pairwise
# = TRUE) can compare means across predictor levels using a real joint
# covariance matrix, same mechanism weighted_perc_svyby() already uses for
# proportions) on identical inputs - same weighted data, same outcome/
# predictor, same conf setting - checking that estimate/estimate_se/
# unweighted_n/base come back the same (within floating-point tolerance for
# the survey-computed numbers; exact for the plain unweighted counts).
#
# Same caveat as every other test file this session: I haven't run these
# myself - no R in this environment. Unlike some earlier additions though,
# the underlying mechanism here IS confirmed against real R output first -
# see check_svyby_mean_naming.R's Sections 1a/2a/3a/4 (bare-level coef()
# naming, both flat and nested; SE() == sqrt(diag(vcov())) directly, no
# gotcha like the factor-outcome case had; estimates/SEs matching
# weighted_mean() exactly; na.rm = TRUE required and confirmed to fix the
# silent NA/NaN propagation without it) - so these tests are pinning down
# already-observed real behaviour, not hoping an untested assumption holds.
# Run with:
#   testthat::test_file(here::here("Scripts_new", "test_weighted_mean_svyby.R"))
#
# Tests 1-5: unclustered design (id = 1) - predictor present/absent x
# conf = NULL/"se"/"ci", then a clustered design repeat - direct structural
# mirror of test_weighted_perc_svyby.R's own tests 1-5, adapted for a
# numeric outcome.
# Test 6: three-level predictor, confirming clean level-label recovery. No
# colon-split needed at all here (unlike test_weighted_perc_svyby.R's own
# test 6) - coef()'s names ARE the bare level for a numeric outcome, so this
# is really just confirming they pass straight through tidy_svyby_mean()
# unmangled.
# Tests 7-10: the covmat list-column and calc_stats()'s pairwise = TRUE/FALSE
# dispatch - direct mirror of tests 7-10 there.
# Test 11: NA-in-outcome handling - a direct regression pin on
# check_svyby_mean_naming.R's Section 4 finding (silent NA/NaN without
# na.rm, correct numbers with it), including a raw, non-calc_stat_engine()-
# mediated svyby() call so the na.rm = TRUE protection is actually exercised,
# not just relying on calc_stat_engine()'s own NA pre-filter to make the
# question moot.
# Test 12: nested (2-predictor) end-to-end, hand-calculated known answer -
# same 2x2 unweighted/unclustered design shape as test_add_pairwise_sig.R's
# own test 21 for proportions, adapted for a numeric outcome so there's a
# real known SE and a real known (near-zero) cross-group covariance to check
# the exact pairwise machinery against, not just "it runs".
# =============================================================================
library(testthat)
source(here::here("Scripts_new", "calc_stats.R"))


# ---- shared helper: run both implementations, return a row-aligned
# ---- comparison tibble keyed by whatever category columns are present.
compare_mean_implementations <- function(design, outcomes, predictors = NULL, conf = NULL) {
  original <- weighted_mean(design, outcomes = outcomes, predictors = predictors, conf = conf)
  new      <- weighted_mean_svyby(design, outcomes = outcomes, predictors = predictors, conf = conf)

  join_cols <- intersect(c("outcome", "cross_break", "p_cat1"), names(original))

  original %>%
    select(all_of(join_cols), estimate, unweighted_n, base, any_of(c("estimate_se", "estimate_ci"))) %>%
    rename_with(~ paste0("orig_", .x), -all_of(join_cols)) %>%
    left_join(
      new %>% select(all_of(join_cols), estimate, unweighted_n, base, any_of(c("estimate_se", "estimate_ci"))) %>%
        rename_with(~ paste0("new_", .x), -all_of(join_cols)),
      by = join_cols
    )
}


build_unclustered_design <- function() {
  set.seed(1)
  n <- 60
  data <- tibble(
    score  = rnorm(n, mean = 50, sd = 10),
    region = factor(sample(c("North", "South"), n, replace = TRUE)),
    wt     = runif(n, 0.5, 2)
  )
  data %>% srvyr::as_survey_design(ids = 1, weights = wt)
}

build_clustered_design <- function() {
  set.seed(2)
  n_clusters  <- 12
  per_cluster <- 8
  cluster_id     <- rep(seq_len(n_clusters), each = per_cluster)
  cluster_effect <- rep(rnorm(n_clusters, 0, 5), each = per_cluster)
  n <- n_clusters * per_cluster
  data <- tibble(
    cluster = cluster_id,
    score   = 50 + cluster_effect + rnorm(n, 0, 10),
    region  = factor(sample(c("North", "South", "East"), n, replace = TRUE)),
    wt      = runif(n, 0.5, 2)
  )
  data %>% srvyr::as_survey_design(ids = cluster, weights = wt)
}


test_that("1. unclustered, with predictor, conf = NULL - estimates and counts match", {
  design <- build_unclustered_design()
  cmp <- compare_mean_implementations(design, outcomes = "score", predictors = "region", conf = NULL)

  expect_equal(cmp$new_estimate, cmp$orig_estimate, tolerance = 1e-8)
  expect_equal(cmp$new_unweighted_n, cmp$orig_unweighted_n)
  expect_equal(cmp$new_base, cmp$orig_base)
})


test_that("2. unclustered, with predictor, conf = \"se\" - SEs match too", {
  design <- build_unclustered_design()
  cmp <- compare_mean_implementations(design, outcomes = "score", predictors = "region", conf = "se")

  expect_equal(cmp$new_estimate, cmp$orig_estimate, tolerance = 1e-8)
  expect_equal(as.numeric(cmp$new_estimate_se), as.numeric(cmp$orig_estimate_se), tolerance = 1e-6)
})


test_that("3. unclustered, no predictor (Total only) - matches", {
  design <- build_unclustered_design()
  cmp <- compare_mean_implementations(design, outcomes = "score", predictors = NULL, conf = "se")

  expect_equal(nrow(cmp), 1)
  expect_equal(cmp$new_estimate, cmp$orig_estimate, tolerance = 1e-8)
  expect_equal(as.numeric(cmp$new_estimate_se), as.numeric(cmp$orig_estimate_se), tolerance = 1e-6)
})


test_that("4. unclustered, conf = \"ci\" - point estimates still match (CI bounds not compared - different construction between the two implementations, same as test_weighted_perc_svyby.R's own test 4)", {
  design <- build_unclustered_design()
  cmp <- compare_mean_implementations(design, outcomes = "score", predictors = "region", conf = "ci")

  expect_equal(cmp$new_estimate, cmp$orig_estimate, tolerance = 1e-8)
  expect_true(all(c("orig_estimate_ci", "new_estimate_ci") %in% names(cmp)))
})


test_that("5. clustered design - still matches (confirms neither implementation is quietly assuming id = 1)", {
  design <- build_clustered_design()
  cmp <- compare_mean_implementations(design, outcomes = "score", predictors = "region", conf = "se")

  expect_equal(cmp$new_estimate, cmp$orig_estimate, tolerance = 1e-8)
  expect_equal(as.numeric(cmp$new_estimate_se), as.numeric(cmp$orig_estimate_se), tolerance = 1e-6)
  expect_true(all(as.numeric(cmp$orig_estimate_se) > 0))
})


test_that("6. three-level predictor - bare coef() names recovered as clean level labels, no mangling", {
  design <- build_clustered_design()   # region already has 3 levels here
  cmp <- compare_mean_implementations(design, outcomes = "score", predictors = "region", conf = "se")

  region_rows <- cmp %>% filter(cross_break == "region")
  expect_equal(sort(unique(region_rows$p_cat1)), c("East", "North", "South"))
  expect_equal(region_rows$new_estimate, region_rows$orig_estimate, tolerance = 1e-8)
})


test_that("7. weighted_mean_svyby() covmat column - NULL for Total, a real matrix for predictor rows, internally consistent with estimate_se", {
  design <- build_unclustered_design()
  result <- weighted_mean_svyby(design, outcomes = "score", predictors = "region", conf = "se")

  total_rows    <- result %>% filter(cross_break == "Total")
  predictor_row <- result %>% filter(cross_break == "region") %>% slice(1)

  expect_true(all(map_lgl(total_rows$covmat, is.null)))
  expect_true(is.matrix(predictor_row$covmat[[1]]))

  # Bare level name - no ":outcome"/o_cat suffix at all for a numeric outcome
  # (confirmed by check_svyby_mean_naming.R). This is the coefficient name
  # predictor_row's own estimate_se was itself built from
  # (sqrt(diag(vcov(...))) in tidy_svyby_mean()), so it should agree to
  # floating-point precision, not just approximately.
  coef_name <- predictor_row$p_cat1
  se_from_covmat <- sqrt(predictor_row$covmat[[1]][coef_name, coef_name])
  expect_equal(se_from_covmat, as.numeric(predictor_row$estimate_se), tolerance = 1e-8)
})


test_that("8. calc_stats(pairwise = TRUE/FALSE) - covmat column present only when asked for", {
  design <- build_unclustered_design()

  with_pairwise <- calc_stats(design, outcomes = "score", predictors = "region",
                               statistics = "w_mean", conf = "se", multicode = FALSE, pairwise = TRUE)
  without_pairwise <- calc_stats(design, outcomes = "score", predictors = "region",
                                  statistics = "w_mean", conf = "se", multicode = FALSE, pairwise = FALSE)

  expect_true("covmat" %in% names(with_pairwise))
  expect_false("covmat" %in% names(without_pairwise))
})


test_that("9. calc_stats(pairwise = TRUE) errors clearly on more than 2 nested predictors", {
  design <- build_clustered_design() %>%
    mutate(age_group    = factor(rep(c("Young", "Old"), 48)),
           income_group = factor(rep(c("Low", "High"), 48)))

  expect_error(
    calc_stats(design, outcomes = "score",
               predictors = list(c("region", "age_group", "income_group")),
               statistics = "w_mean", conf = "se", multicode = FALSE, pairwise = TRUE),
    "more than 2 nested predictors"
  )
})


test_that("10. calc_stats(pairwise = TRUE) dispatch matches calling weighted_mean_svyby() directly", {
  design <- build_unclustered_design()

  via_calc_stats <- calc_stats(design, outcomes = "score", predictors = "region",
                                statistics = "w_mean", conf = "se", multicode = FALSE, pairwise = TRUE)
  direct <- weighted_mean_svyby(design, outcomes = "score", predictors = "region", conf = "se")

  join_cols <- c("outcome", "cross_break", "p_cat1")
  cmp <- via_calc_stats %>% select(all_of(join_cols), estimate) %>%
    left_join(direct %>% select(all_of(join_cols), estimate), by = join_cols,
              suffix = c("_via_calc_stats", "_direct"))

  expect_equal(cmp$estimate_via_calc_stats, cmp$estimate_direct, tolerance = 1e-8)
})


test_that("11. NA in the outcome - weighted_mean_svyby() handles it correctly, and na.rm = TRUE is what's actually doing the work", {
  set.seed(3)
  n <- 60
  data <- tibble(
    score  = rnorm(n, mean = 50, sd = 10),
    region = factor(sample(c("North", "South"), n, replace = TRUE)),
    wt     = runif(n, 0.5, 2)
  )
  na_rows <- sample(seq_len(n), 6)
  data$score[na_rows] <- NA
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  # Through calc_stat_engine() (i.e. calling weighted_mean_svyby() the normal
  # way) - calc_stat_engine() pre-filters any row with an NA outcome before
  # compute() ever runs, so this mainly confirms na.rm = TRUE doesn't change
  # the answer once that pre-filter has already run, not that it's doing the
  # work here specifically.
  cmp <- compare_mean_implementations(design, outcomes = "score", predictors = "region", conf = "se")
  expect_false(any(is.na(cmp$new_estimate)))
  expect_equal(cmp$new_estimate, cmp$orig_estimate, tolerance = 1e-8)
  expect_equal(as.numeric(cmp$new_estimate_se), as.numeric(cmp$orig_estimate_se), tolerance = 1e-6)

  # A raw, non-calc_stat_engine()-mediated svyby() call - the actual case
  # na.rm = TRUE is protecting against (see weighted_mean_svyby()'s own
  # header note on this). Pins the exact real-world finding from
  # check_svyby_mean_naming.R's Section 4: without na.rm, svyby(FUN =
  # svymean) silently returns NA for every group rather than erroring or
  # dropping just the missing rows; with it, the numbers come back clean.
  design_base <- design
  class(design_base) <- setdiff(class(design_base), "tbl_svy")

  without_na_rm <- survey::svyby(~score, ~region, design = design_base, FUN = survey::svymean,
                                  keep.var = TRUE, vartype = "se", covmat = TRUE)
  expect_true(all(is.na(coef(without_na_rm))))

  with_na_rm <- survey::svyby(~score, ~region, design = design_base, FUN = survey::svymean,
                               na.rm = TRUE, keep.var = TRUE, vartype = "se", covmat = TRUE)
  expect_false(any(is.na(coef(with_na_rm))))
})


# Male/Young: 10,20 -> mean 15   Male/Old: 30,40 -> mean 35
# Female/Young: 50,60 -> mean 55   Female/Old: 70,80 -> mean 75
# Same shape as test_pivot_nested_crosstab.R's tests 1/6 (age 10..80 in
# pairs), reused here for the same reason: each pair's spread (10 apart)
# gives a clean, hand-calculable SE. IMPORTANT correction (found via Joe's
# real run): the naive per-subgroup SRS formula (sample sd / sqrt(n_group) =
# 5.0) is NOT what svyby()/svymean() computes for a domain/subgroup mean
# pulled out of a larger design. It uses a domain (ratio) linearization
# across the FULL n = 8 sample: build z_i = (y_i - domain_mean) for units in
# the domain and 0 elsewhere, take the with-replacement variance of the
# TOTAL of z (n * unbiased_var(z) using n-1 = 7 across all 8 units, not just
# the 2 domain members), then divide by domain_size^2 to get the ratio's
# variance. For this data: z = (-5, 5, 0,0,0,0,0,0), zbar = 0,
# sum((z-zbar)^2) = 50, s2 = 50/7, var_total = 8*(50/7), var_ratio =
# var_total/2^2 = 400/28 = 14.2857, SE = sqrt(14.2857) = 3.779645 - same for
# every group here since all 4 groups share the same +-5 offset shape.
# Off-diagonal covariance between disjoint domains is still genuinely zero
# under this formula (their z-vectors have no overlapping nonzero indices),
# so that assertion is untouched - same reasoning test_add_pairwise_sig.R's
# own test 21 uses for its nested proportions case. Unweighted (wt = 1),
# unclustered (ids = 1).
test_that("12. weighted_mean_svyby() nested (2-predictor) end-to-end, hand-calculated known answer", {
  data <- tibble(
    score     = c(10, 20, 30, 40, 50, 60, 70, 80),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old")),
    wt        = rep(1, 8)
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  result <- weighted_mean_svyby(design, outcomes = "score", predictors = list(c("sex", "age_group")), conf = "se")

  nested_rows <- result %>% filter(cross_break == "sex_X_age_group")
  expect_equal(nrow(nested_rows), 4)

  my <- nested_rows %>% filter(p_cat1 == "Male", p_cat2 == "Young")
  mo <- nested_rows %>% filter(p_cat1 == "Male", p_cat2 == "Old")
  fy <- nested_rows %>% filter(p_cat1 == "Female", p_cat2 == "Young")
  fo <- nested_rows %>% filter(p_cat1 == "Female", p_cat2 == "Old")

  expect_equal(my$estimate, 15); expect_equal(mo$estimate, 35)
  expect_equal(fy$estimate, 55); expect_equal(fo$estimate, 75)
  expect_equal(as.numeric(my$estimate_se), sqrt(400/28), tolerance = 1e-6)
  expect_equal(as.numeric(fo$estimate_se), sqrt(400/28), tolerance = 1e-6)

  expect_true(is.matrix(my$covmat[[1]]))

  # Bare "Male.Young"-style dot-join, no outcome suffix - confirmed by
  # check_svyby_mean_naming.R's Section 2a.
  coef_name <- paste0(my$p_cat1, ".", my$p_cat2)
  se_from_covmat <- sqrt(my$covmat[[1]][coef_name, coef_name])
  expect_equal(se_from_covmat, as.numeric(my$estimate_se), tolerance = 1e-8)

  # Off-diagonal covariance between two disjoint (non-overlapping) groups
  # should be genuinely (near-)zero under this unclustered design - the same
  # Monte-Carlo-confirmed property test_add_pairwise_sig.R's own nested test
  # relies on for proportions.
  other_coef_name <- paste0(fo$p_cat1, ".", fo$p_cat2)
  cross_cov <- my$covmat[[1]][coef_name, other_coef_name]
  expect_equal(cross_cov, 0, tolerance = 1e-8)
})
