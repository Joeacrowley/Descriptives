# =============================================================================
# test_weighted_perc_svyby.R
# Compares weighted_perc() (the existing srvyr-based implementation) against
# weighted_perc_svyby() (the svyby()-based one, now living in calc_stats.R
# itself alongside the other statistic functions) on identical inputs - same
# weighted data, same outcome/predictor, same conf setting - checking that
# estimate/estimate_se/unweighted_n/base come back the same (within
# floating-point tolerance for the survey-computed numbers; exact for the
# plain unweighted counts).
# Same caveat as every other test file this session: unverified, no R here
# to run it. Run with:
#   testthat::test_file("tests/testthat/test-weighted_perc_svyby.R"), or devtools::test()
# Tests 1-4: unclustered design (id = 1) - predictor present/absent x
# conf = NULL/"se"/"ci".
# Test 5: same comparison again under a CLUSTERED design (id = cluster) -
# weighted_perc_svyby() should still match weighted_perc() exactly, since
# both are computing the same design-based estimator; this only confirms
# neither implementation quietly assumes an unclustered design somewhere.
# Test 6: three-level predictor, checking tidy_svyby_perc()'s colon-split
# recovers clean level labels (not just that the numbers happen to match -
# a bug that mangled labels but coincidentally preserved row order could
# still pass a pure numeric comparison).
# Tests 7-10: the covmat list-column and calc_stats()'s pairwise = TRUE/FALSE
# dispatch, now that weighted_perc_svyby() is wired into calc_stats() itself
# rather than only being callable directly.
# =============================================================================


# ---- shared helper: run both implementations, return a row-aligned
# ---- comparison tibble keyed by whatever category columns are present.
compare_implementations <- function(design, outcomes, predictors = NULL, conf = NULL) {
  original <- weighted_perc(design, outcomes = outcomes, predictors = predictors, conf = conf)
  new      <- weighted_perc_svyby(design, outcomes = outcomes, predictors = predictors, conf = conf)

  join_cols <- intersect(c("outcome", "o_cat", "cross_break", "p_cat1"), names(original))

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
    response = factor(sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.6, 0.4))),
    region   = factor(sample(c("North", "South"), n, replace = TRUE)),
    wt       = runif(n, 0.5, 2)
  )
  data %>% srvyr::as_survey_design(ids = 1, weights = wt)
}

build_clustered_design <- function() {
  set.seed(2)
  n_clusters  <- 12
  per_cluster <- 8
  cluster_id      <- rep(seq_len(n_clusters), each = per_cluster)
  cluster_effect  <- rep(rnorm(n_clusters, 0, 1), each = per_cluster)
  n <- n_clusters * per_cluster
  data <- tibble(
    cluster  = cluster_id,
    response = factor(ifelse(plogis(cluster_effect + rnorm(n)) > 0.5, "Yes", "No")),
    region   = factor(sample(c("North", "South", "East"), n, replace = TRUE)),
    wt       = runif(n, 0.5, 2)
  )
  data %>% srvyr::as_survey_design(ids = cluster, weights = wt)
}


test_that("1. unclustered, with predictor, conf = NULL - estimates and counts match", {
  design <- build_unclustered_design()
  cmp <- compare_implementations(design, outcomes = "response", predictors = "region", conf = NULL)

  expect_equal(cmp$new_estimate, cmp$orig_estimate, tolerance = 1e-8)
  expect_equal(cmp$new_unweighted_n, cmp$orig_unweighted_n)
  expect_equal(cmp$new_base, cmp$orig_base)
})


test_that("2. unclustered, with predictor, conf = \"se\" - SEs match too", {
  design <- build_unclustered_design()
  cmp <- compare_implementations(design, outcomes = "response", predictors = "region", conf = "se")

  expect_equal(cmp$new_estimate, cmp$orig_estimate, tolerance = 1e-8)
  expect_equal(as.numeric(cmp$new_estimate_se), as.numeric(cmp$orig_estimate_se), tolerance = 1e-6)
})


test_that("3. unclustered, no predictor (Total only) - matches", {
  design <- build_unclustered_design()
  cmp <- compare_implementations(design, outcomes = "response", predictors = NULL, conf = "se")

  expect_equal(nrow(cmp), 2)   # Yes / No
  expect_equal(cmp$new_estimate, cmp$orig_estimate, tolerance = 1e-8)
  expect_equal(as.numeric(cmp$new_estimate_se), as.numeric(cmp$orig_estimate_se), tolerance = 1e-6)
})


test_that("4. unclustered, conf = \"ci\" - point estimates still match (CI bounds not compared - see header note in weighted_perc_svyby.R on the two implementations' different CI construction)", {
  design <- build_unclustered_design()
  cmp <- compare_implementations(design, outcomes = "response", predictors = "region", conf = "ci")

  expect_equal(cmp$new_estimate, cmp$orig_estimate, tolerance = 1e-8)
  expect_true(all(c("orig_estimate_ci", "new_estimate_ci") %in% names(cmp)))
})


test_that("5. clustered design - still matches (confirms neither implementation is quietly assuming id = 1)", {
  design <- build_clustered_design()
  cmp <- compare_implementations(design, outcomes = "response", predictors = "region", conf = "se")

  expect_equal(cmp$new_estimate, cmp$orig_estimate, tolerance = 1e-8)
  expect_equal(as.numeric(cmp$new_estimate_se), as.numeric(cmp$orig_estimate_se), tolerance = 1e-6)
  # a clustered design's SE should come back as a real positive number - a
  # loose sanity check that this test is actually exercising the clustered
  # path, not accidentally degenerate.
  expect_true(all(as.numeric(cmp$orig_estimate_se) > 0))
})


test_that("6. three-level predictor - colon-splitting recovers clean level labels", {
  design <- build_clustered_design()   # region already has 3 levels here
  cmp <- compare_implementations(design, outcomes = "response", predictors = "region", conf = "se")

  # cmp also includes the "Total" block (cross_break == "Total", p_cat1 ==
  # "Total") that weighted_perc()/weighted_perc_svyby() always return
  # alongside the predictor breakdown - filter to the real region rows
  # before checking there are exactly the 3 actual levels, with no stray
  # fragment left over from the colon-split.
  region_rows <- cmp %>% filter(cross_break == "region")
  expect_equal(sort(unique(region_rows$p_cat1)), c("East", "North", "South"))
  expect_equal(region_rows$new_estimate, region_rows$orig_estimate, tolerance = 1e-8)
})


test_that("7. weighted_perc_svyby() covmat column - NULL for Total, a real matrix for predictor rows, internally consistent with estimate_se", {
  design <- build_unclustered_design()
  result <- weighted_perc_svyby(design, outcomes = "response", predictors = "region", conf = "se")

  total_rows    <- result %>% filter(cross_break == "Total")
  predictor_row <- result %>% filter(cross_break == "region") %>% slice(1)

  expect_true(all(map_lgl(total_rows$covmat, is.null)))
  expect_true(is.matrix(predictor_row$covmat[[1]]))

  # The covmat this row's estimate_se was itself built from (sqrt(diag(...))
  # in tidy_svyby_perc()) should still agree with the coefficient this row's
  # own (p_cat1, o_cat) pair corresponds to - same source, so this should
  # hold to floating-point precision, not just approximately.
  coef_name <- paste0(predictor_row$p_cat1, ":", predictor_row$outcome, predictor_row$o_cat)
  se_from_covmat <- sqrt(predictor_row$covmat[[1]][coef_name, coef_name])
  expect_equal(se_from_covmat, as.numeric(predictor_row$estimate_se), tolerance = 1e-8)
})


test_that("8. calc_stats(pairwise = TRUE/FALSE) - covmat column present only when asked for", {
  design <- build_unclustered_design()

  with_pairwise <- calc_stats(design, outcomes = "response", predictors = "region",
                               statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)
  without_pairwise <- calc_stats(design, outcomes = "response", predictors = "region",
                                  statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = FALSE)

  expect_true("covmat" %in% names(with_pairwise))
  expect_false("covmat" %in% names(without_pairwise))
})


test_that("9. calc_stats(pairwise = TRUE) errors clearly on more than 2 nested predictors", {
  # A genuine second/third predictor, not the outcome itself - using
  # "response" here (as an earlier version of this test did) trips
  # calc_stats()'s own outcomes_not_in_predictors() validation before ever
  # reaching the nested-predictor-set guard this test is actually meant to
  # exercise. 2-variable nested sets are supported now - svyby()'s by-formula
  # crosses them via "+" (tested end-to-end in test_add_pairwise_sig.R's
  # test 21) - only more than 2 still errors.
  design <- build_clustered_design() %>%
    mutate(age_group    = factor(rep(c("Young", "Old"), 48)),
           income_group = factor(rep(c("Low", "High"), 48)))

  expect_error(
    calc_stats(design, outcomes = "response",
               predictors = list(c("region", "age_group", "income_group")),
               statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE),
    "more than 2 nested predictors"
  )
})


test_that("10. calc_stats(pairwise = TRUE) dispatch matches calling weighted_perc_svyby() directly", {
  design <- build_unclustered_design()

  via_calc_stats <- calc_stats(design, outcomes = "response", predictors = "region",
                                statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)
  direct <- weighted_perc_svyby(design, outcomes = "response", predictors = "region", conf = "se")

  join_cols <- c("outcome", "o_cat", "cross_break", "p_cat1")
  cmp <- via_calc_stats %>% select(all_of(join_cols), estimate) %>%
    left_join(direct %>% select(all_of(join_cols), estimate), by = join_cols,
              suffix = c("_via_calc_stats", "_direct"))

  expect_equal(cmp$estimate_via_calc_stats, cmp$estimate_direct, tolerance = 1e-8)
})
