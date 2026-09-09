# =============================================================================
# Tests for the pairwise-testing machinery in calc_stats.R: safe_ttest(),
# pw_test_cats()/pw_test_cats_multi(), pw_test_means(), add_letters(),
# merge_p_letters(), pwtest_over_all_combinations(), add_pairwise_sig(), and
# calc_stats(pairwise = TRUE) end-to-end. Replaces the old svyby/covmat-based
# pairwise test file entirely - that machinery (derive_se(), assign_sig_
# letters(), pairwise_test_one_group(), exact_se_diff(), weighted_mean_svyby())
# was deleted this session and replaced with the Holm/BH-adjusted-t-test
# approach tested here.
# =============================================================================


# ---- pwtest_over_all_combinations() ----------------------------------------

test_that("1. pwtest_over_all_combinations() runs a flat categorical predictor and stamps stat/crit_lev", {
  data <- tibble(
    response = factor(c(rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16)),
                       levels = c("No", "Yes")),
    region   = factor(c(rep("North", 20), rep("South", 20)), levels = c("North", "South"))
  )
  results <- pwtest_over_all_combinations(data = data, outcome = "response",
                                           predictor = list("region"), statistics = "perc")

  expect_equal(names(results), "response X region")
  expect_true(all(c("stat", "crit_lev", "p.value", "estimate") %in% names(results[[1]])))
  expect_true(all(results[[1]]$stat == "perc"))
  expect_true(all(results[[1]]$crit_lev == 0.05))
})


test_that("2. pwtest_over_all_combinations() names a nested predictor result correctly (not vectorised across the pair)", {
  data <- tibble(
    response  = factor(c(rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16),
                          rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16)),
                        levels = c("No", "Yes")),
    sex       = factor(c(rep("Male", 40), rep("Female", 40))),
    age_group = factor(rep(c(rep("Young", 20), rep("Old", 20)), 2))
  )
  results <- pwtest_over_all_combinations(data = data, outcome = "response",
                                           predictor = list(c("sex", "age_group")), statistics = "perc")

  expect_equal(length(results), 1)
  expect_equal(names(results), "response X sex_X_age_group")
})


test_that("3. pwtest_over_all_combinations() unwraps a survey design to plain data when the picked stat is unweighted", {
  data <- tibble(
    response = factor(c(rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16))),
    region   = factor(c(rep("North", 20), rep("South", 20))),
    wt       = rep(1, 40)
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  via_design <- pwtest_over_all_combinations(data = design, outcome = "response",
                                              predictor = list("region"), statistics = "perc")
  via_plain  <- pwtest_over_all_combinations(data = data, outcome = "response",
                                              predictor = list("region"), statistics = "perc")

  expect_equal(sort(via_design[[1]]$estimate), sort(via_plain[[1]]$estimate))
})


test_that("4. pwtest_over_all_combinations() keeps the survey design when the picked stat is weighted", {
  data <- tibble(
    response = factor(c(rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16))),
    region   = factor(c(rep("North", 20), rep("South", 20))),
    wt       = c(rep(2, 20), rep(1, 20))
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  via_wperc <- pwtest_over_all_combinations(data = design, outcome = "response",
                                             predictor = list("region"), statistics = "w_perc")
  direct    <- pw_test_cats_multi(outcome = "response", predictor = "region", data = design)

  expect_equal(sort(via_wperc[[1]]$estimate), sort(direct$estimate))
})


test_that("5. pwtest_over_all_combinations() skips an outcome needing a weighted stat when no survey design was given", {
  data <- tibble(
    response = factor(c(rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16))),
    region   = factor(c(rep("North", 20), rep("South", 20)))
  )
  results <- pwtest_over_all_combinations(data = data, outcome = "response",
                                           predictor = list("region"), statistics = "w_perc")
  expect_equal(length(results), 0)
})


test_that("6. pwtest_over_all_combinations() skips an outcome with no pairwise-eligible statistic requested", {
  data <- tibble(score = c(10, 20, 30, 40), region = factor(c("N", "N", "S", "S")))
  results <- pwtest_over_all_combinations(data = data, outcome = "score",
                                           predictor = list("region"), statistics = "median")
  expect_equal(length(results), 0)
})


test_that("7. pwtest_over_all_combinations() catches a too-small comparison group, warns, and skips only that outcome", {
  tiny_data <- tibble(
    approve = factor(c(rep("No", 14), "Yes", rep("No", 8), rep("Yes", 7))),
    region  = factor(c(rep("Alpha", 15), rep("Beta", 15))),
    score   = rnorm(30, 50, 10)
  )
  expect_warning(
    results <- pwtest_over_all_combinations(data = tiny_data, outcome = c("approve", "score"),
                                             predictor = list("region"), statistics = c("perc", "mean")),
    "Pairwise test skipped"
  )
  expect_equal(names(results), "score X region")
})


# ---- add_letters() ----------------------------------------------------------

test_that("8. add_letters() assigns sequential letters per cross_break, NA for Total", {
  df <- tibble(cross_break = c("Total", "region", "region", "region"),
               p_cat1      = c("Total", "North", "South", "East"))
  result <- add_letters(df)
  expect_true(is.na(result$sig_letter[result$cross_break == "Total"]))
  expect_equal(result$sig_letter[result$cross_break == "region"], c("a", "b", "c"))
})


test_that("9. add_letters() restarts the letter sequence at 'a' for each cross_break block", {
  df <- tibble(cross_break = c("region", "region", "sex", "sex"),
               p_cat1      = c("North", "South", "F", "M"))
  result <- add_letters(df)
  expect_equal(result$sig_letter, c("a", "b", "a", "b"))
})


# ---- merge_p_letters() -------------------------------------------------------

test_that("10. merge_p_letters() correctly flags a flat 3-level predictor - South differs from both, North/East don't differ from each other", {
  data <- tibble(
    response = factor(c(rep("Yes", 16), rep("No", 4),
                         rep("Yes", 4),  rep("No", 16),
                         rep("Yes", 14), rep("No", 6))),
    region   = factor(c(rep("North", 20), rep("South", 20), rep("East", 20)),
                       levels = c("North", "South", "East"))
  )
  cs <- calc_stats(data, outcomes = "response", predictors = "region",
                    statistics = "perc", conf = "se", multicode = FALSE) %>% add_letters()
  pw <- pw_test_cats_multi(outcome = "response", predictor = "region", data = data) %>%
    mutate(stat = "perc")

  merged <- merge_p_letters(pw, cs, crit_lev = 0.05)
  yes_rows <- merged %>% filter(o_cat == "Yes")

  north <- yes_rows %>% filter(p_cat1 == "North") %>% pull(sig_diff)
  south <- yes_rows %>% filter(p_cat1 == "South") %>% pull(sig_diff)
  east  <- yes_rows %>% filter(p_cat1 == "East")  %>% pull(sig_diff)

  north_letter <- cs %>% filter(cross_break == "region", p_cat1 == "North") %>% pull(sig_letter) %>% unique()
  south_letter <- cs %>% filter(cross_break == "region", p_cat1 == "South") %>% pull(sig_letter) %>% unique()
  east_letter  <- cs %>% filter(cross_break == "region", p_cat1 == "East")  %>% pull(sig_letter) %>% unique()

  expect_equal(north, south_letter)
  expect_equal(east, south_letter)
  expect_true(grepl(north_letter, south, fixed = TRUE))
  expect_true(grepl(east_letter, south, fixed = TRUE))
})


test_that("11. merge_p_letters() keys correctly on a nested predictor set (p_cat1 x p_cat2)", {
  data <- tibble(
    response  = factor(c(rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16),
                          rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16))),
    sex       = factor(c(rep("Male", 40), rep("Female", 40))),
    age_group = factor(rep(c(rep("Young", 20), rep("Old", 20)), 2))
  )
  cs <- calc_stats(data, outcomes = "response", predictors = list(c("sex", "age_group")),
                    statistics = "perc", conf = "se", multicode = FALSE) %>% add_letters()
  pw <- pw_test_cats_multi(outcome = "response", predictor = c("sex", "age_group"), data = data) %>%
    mutate(stat = "perc")

  merged <- merge_p_letters(pw, cs, crit_lev = 0.05)
  yes_rows <- merged %>% filter(o_cat == "Yes")
  expect_equal(nrow(yes_rows), 4)

  my <- yes_rows %>% filter(p_cat1 == "Male", p_cat2 == "Young")
  mo <- yes_rows %>% filter(p_cat1 == "Male", p_cat2 == "Old")
  # same p_cat1 (Male), differing p_cat2 (Young vs Old) - a valid comparison,
  # and here a genuinely significant one (0.8 vs 0.2).
  expect_true(grepl(mo$sig_letter, my$sig_diff, fixed = TRUE))
})


test_that("12. merge_p_letters() keeps perc and w_perc as separate rows via the stat key, no row fan-out", {
  data <- tibble(
    response = factor(c(rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16))),
    region   = factor(c(rep("North", 20), rep("South", 20))),
    wt       = c(rep(2, 20), rep(1, 20))
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  cs <- calc_stats(design, outcomes = "response", predictors = "region",
                    statistics = c("perc", "w_perc"), conf = "se", multicode = FALSE) %>% add_letters()

  pw_u <- pw_test_cats_multi(outcome = "response", predictor = "region", data = data) %>% mutate(stat = "perc")
  pw_w <- pw_test_cats_multi(outcome = "response", predictor = "region", data = design) %>% mutate(stat = "w_perc")

  merged <- bind_rows(
    merge_p_letters(pw_u, cs, crit_lev = 0.05),
    merge_p_letters(pw_w, cs, crit_lev = 0.05)
  )

  expect_equal(nrow(merged), nrow(distinct(merged, cross_break, p_cat1, o_cat, stat)))
  expect_setequal(unique(merged$stat), c("perc", "w_perc"))
})


# ---- add_pairwise_sig() ------------------------------------------------------

test_that("13. add_pairwise_sig() leaves sig_diff as NA when predictors is NULL", {
  cs <- calc_stats(tibble(score = c(10, 20, 30)), outcomes = "score",
                    predictors = NULL, statistics = "mean")
  result <- add_pairwise_sig(cs, data = tibble(score = c(10, 20, 30)), outcomes = "score",
                              predictors = NULL, statistics = "mean")
  expect_true(all(is.na(result$sig_diff)))
})


test_that("14. add_pairwise_sig() filters out a predictor set longer than 2 without erroring", {
  data <- tibble(
    response = factor(c(rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16))),
    region   = factor(c(rep("North", 20), rep("South", 20))),
    sex      = factor(rep(c("F", "M"), 20))
  )
  cs <- calc_stats(data, outcomes = "response", predictors = list(c("region", "sex")),
                    statistics = "perc", conf = "se")
  result <- add_pairwise_sig(cs, data = data, outcomes = "response",
                              predictors = list(c("region", "sex", "response")),  # length 3, invalid anyway
                              statistics = "perc")
  expect_true(all(is.na(result$sig_diff)))
})


# ---- calc_stats(pairwise = TRUE) end-to-end ----------------------------------

test_that("15. calc_stats(pairwise = TRUE) populates sig_letter/sig_diff for a flat categorical predictor, NA on Total", {
  data <- tibble(
    response = factor(c(rep("Yes", 16), rep("No", 4),
                         rep("Yes", 4),  rep("No", 16),
                         rep("Yes", 14), rep("No", 6))),
    region   = factor(c(rep("North", 20), rep("South", 20), rep("East", 20)),
                       levels = c("North", "South", "East"))
  )
  result <- calc_stats(data, outcomes = "response", predictors = "region",
                        statistics = "perc", conf = "se", multicode = FALSE, pairwise = TRUE)

  region_rows <- result %>% filter(cross_break == "region")
  total_rows  <- result %>% filter(cross_break == "Total")

  expect_equal(length(unique(region_rows$sig_letter)), 3)
  expect_true(all(is.na(total_rows$sig_letter)))
  expect_true(all(is.na(total_rows$sig_diff)))

  south_letter <- region_rows %>% filter(p_cat1 == "South") %>% pull(sig_letter) %>% unique()
  north_diff   <- region_rows %>% filter(p_cat1 == "North", o_cat == "Yes") %>% pull(sig_diff)
  expect_equal(north_diff, south_letter)
})


test_that("16. calc_stats(pairwise = TRUE) works end-to-end on a nested (2-variable) predictor set", {
  data <- tibble(
    response  = factor(c(rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16),
                          rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16))),
    sex       = factor(c(rep("Male", 40), rep("Female", 40))),
    age_group = factor(rep(c(rep("Young", 20), rep("Old", 20)), 2))
  )
  result <- calc_stats(data, outcomes = "response", predictors = list(c("sex", "age_group")),
                        statistics = "perc", conf = "se", multicode = FALSE, pairwise = TRUE)

  expect_true("p_cat2" %in% names(result))
  yes_rows <- result %>% filter(o_cat == "Yes", cross_break == "sex_X_age_group")
  expect_equal(nrow(yes_rows), 4)
  expect_equal(length(unique(yes_rows$sig_letter)), 4)
})


test_that("17. calc_stats(pairwise = TRUE) works end-to-end for a numeric mean outcome", {
  data <- tibble(
    score  = c(rep(c(8, 9, 10, 11, 12), 4), rep(c(48, 49, 50, 51, 52), 4)),
    region = factor(c(rep("North", 20), rep("South", 20)), levels = c("North", "South"))
  )
  result <- calc_stats(data, outcomes = "score", predictors = "region",
                        statistics = "mean", conf = "se", multicode = FALSE, pairwise = TRUE)

  region_rows <- result %>% filter(cross_break == "region")
  expect_equal(length(unique(region_rows$sig_letter)), 2)
  expect_true(all(region_rows$sig_diff != "-"))
})


test_that("18. calc_stats(pairwise = FALSE), the default, adds no sig_letter/sig_diff columns at all", {
  data <- tibble(response = factor(c("Yes", "No", "Yes", "No")), region = factor(c("N", "N", "S", "S")))
  result <- calc_stats(data, outcomes = "response", predictors = "region", statistics = "perc")
  expect_false("sig_letter" %in% names(result))
  expect_false("sig_diff" %in% names(result))
})


test_that("19. calc_stats(pairwise = TRUE, pw_outcome_levels = ...) restricts which outcome categories get tested", {
  data <- tibble(
    response = factor(c(rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16))),
    region   = factor(c(rep("North", 20), rep("South", 20)))
  )
  result <- calc_stats(data, outcomes = "response", predictors = "region", statistics = "perc",
                        conf = "se", multicode = FALSE, pairwise = TRUE,
                        pw_outcome_levels = list(response = "Yes"))

  no_rows  <- result %>% filter(cross_break == "region", o_cat == "No")
  yes_rows <- result %>% filter(cross_break == "region", o_cat == "Yes")
  expect_true(all(no_rows$sig_diff == "-"))
  expect_true(all(yes_rows$sig_diff != "-"))
})


test_that("20. calc_stats(pairwise = TRUE, pw_crit_lev = ...) a stricter threshold can flip a borderline case from significant to not", {
  # A modest, real difference (0.6 vs 0.4, n=20 each) - significant at a
  # loose threshold, not at a very strict one.
  data <- tibble(
    response = factor(c(rep("Yes", 12), rep("No", 8), rep("Yes", 8), rep("No", 12))),
    region   = factor(c(rep("North", 20), rep("South", 20)))
  )
  loose <- calc_stats(data, outcomes = "response", predictors = "region", statistics = "perc",
                       conf = "se", multicode = FALSE, pairwise = TRUE, pw_crit_lev = 0.5)
  strict <- calc_stats(data, outcomes = "response", predictors = "region", statistics = "perc",
                        conf = "se", multicode = FALSE, pairwise = TRUE, pw_crit_lev = 1e-10)

  loose_diff  <- loose  %>% filter(cross_break == "region", o_cat == "Yes") %>% pull(sig_diff)
  strict_diff <- strict %>% filter(cross_break == "region", o_cat == "Yes") %>% pull(sig_diff)

  expect_true(all(loose_diff != "-"))
  expect_true(all(strict_diff == "-"))
})


test_that("21. calc_stats(pairwise = TRUE) mixing perc and w_perc in one call keeps them separate, no row duplication", {
  data <- tibble(
    response = factor(c(rep("Yes", 16), rep("No", 4), rep("Yes", 4), rep("No", 16))),
    region   = factor(c(rep("North", 20), rep("South", 20))),
    wt       = c(rep(2, 20), rep(1, 20))
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  without_pairwise <- calc_stats(design, outcomes = "response", predictors = "region",
                                  statistics = c("perc", "w_perc"), conf = "se", multicode = FALSE)
  with_pairwise <- calc_stats(design, outcomes = "response", predictors = "region",
                               statistics = c("perc", "w_perc"), conf = "se", multicode = FALSE,
                               pairwise = TRUE)

  expect_equal(nrow(with_pairwise), nrow(without_pairwise))
  expect_setequal(unique(with_pairwise$stat), c("perc", "w_perc"))
})
