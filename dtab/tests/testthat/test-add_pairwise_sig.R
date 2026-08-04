# =============================================================================
# Tests for add_pairwise_sig() and friends - now living in calc_stats.R
# itself, called automatically when calc_stats(pairwise = TRUE) is used,
# still fully usable standalone too.
# Same caveat as every other test file in this project: I haven't run these
# myself (no R in this environment). Run with:
#   testthat::test_file("tests/testthat/test-add_pairwise_sig.R"), or devtools::test()
# Tests 1-3 cover derive_se() (the "se"/"ci"/neither cases - test 3 updated
# for derive_se()'s softened failure mode, see its own header comment in
# calc_stats.R: warns and returns all-NA now rather than stopping the whole
# call, so calc_stats(pairwise = TRUE) without conf set doesn't lose
# pairwise testing for blocks that have a real covmat and never needed this
# fallback in the first place). Tests 4-5 cover assign_sig_letters() - one
# block, then letters continuing across blocks rather than resetting.
# Tests 6-9 cover pairwise_test_one_group() directly on small hand-built
# tibbles (clearly significant, clearly not, a single row, an NA SE),
# exercising the approximate (independent-SE) path only - no covmat column
# present at all in these. Tests 14-17 cover the exact (covmat-based) path
# specifically: exact_se_diff()'s own arithmetic, pairwise_test_one_group()
# actually preferring it over .se when both are present, and falling back
# correctly when covmat exists as a column but isn't a real matrix for that
# row (the NA-from-bind_rows() case discussed with Joe).
# Tests 10-13 are integration tests against calc_stats() output built by
# hand and passed through add_pairwise_sig() directly (the approximate path
# - no weighted design object involved). Test 18 is the equivalent
# end-to-end check through calc_stats(pairwise = TRUE) itself (the exact
# path) - same three-region scenario as test 10, but weighted with wt = 1
# and unclustered (ids = 1), so the true covariance between disjoint groups
# is genuinely zero (confirmed via Monte Carlo earlier this session) and
# the exact path should reproduce test 10's same hand-verified numbers, not
# just some other self-consistent answer.
# Test 10's hand calculation, reused by test 18:
# North: 8/10 Yes -> p = 0.8, SE = sqrt(0.8*0.2/10) = 0.12649
# South: 2/10 Yes -> p = 0.2, SE = sqrt(0.2*0.8/10) = 0.12649 (same, by
#   symmetry of p(1-p))
# East:  7/10 Yes -> p = 0.7, SE = sqrt(0.7*0.3/10) = 0.14491
# North vs South: diff = 0.6, SE_diff = sqrt(2 * 0.12649^2) = 0.17889,
#   z = 3.354, p ~ 0.0008 -> significant
# North vs East:  diff = 0.1, SE_diff = sqrt(0.12649^2 + 0.14491^2) = 0.19235,
#   z = 0.520, p ~ 0.603  -> not significant
# South vs East:  diff = -0.5, SE_diff = 0.19235 (same pair of SEs as above),
#   z = -2.600, p ~ 0.0093 -> significant
# unweighted_perc()'s own SE formula (calc_stats.R) is
# sqrt(estimate*(1-estimate)/sum(unweighted_n)) with sum(unweighted_n) taken
# WITHIN each region (10, given 10 respondents per region here) - matches
# the hand calculation above exactly, not a separate approximation of it.
# Tests 19-21 cover a nested (2-variable) predictor set - the design point
# Joe raised: comparisons should run across EVERY (p_cat1, p_cat2)
# combination in a block, not just within a shared p_cat1 (e.g. "women
# 18-24 vs men 18-24" is exactly as valid a comparison as "women 18-24 vs
# women 25-34"). Test 19 is assign_sig_letters() alone; test 20 is
# pairwise_test_one_group() alone, forcing the exact covmat path with a
# combined "p_cat1.p_cat2" key; test 21 is the full calc_stats(pairwise =
# TRUE) pipeline end-to-end (tidy_svyby_perc()'s split_nested_pred_level(),
# the covariance block_key, both joins in add_pairwise_sig(), and the
# pairwise testing itself, all together).
# Test 21's hand calculation - 2x2 design, Male/Female x Young/Old, wt = 1
# and ids = 1 (unclustered, so true covariance between disjoint groups is
# genuinely zero, same reasoning as test 18):
# Male.Young / Female.Young: 8/10 Yes -> p = 0.8, SE = 0.12649
# Male.Old / Female.Old:     2/10 Yes -> p = 0.2, SE = 0.12649
# Same-cluster pairs (diff = 0): z = 0 -> not significant.
# Cross-cluster pairs (diff = 0.6): SE_diff = sqrt(2*0.12649^2) = 0.17889,
#   z = 3.354, p ~ 0.0008 -> significant.
# Tests 22-23 cover two combinations flagged as untested-but-likely-fine
# once nested pairwise support landed: test 22 mixes a flat predictor set
# and a nested one in the SAME calc_stats(pairwise = TRUE) call (checking
# for cross-contamination - does each block's own comparison come out
# identical to running it alone, and do letters stay globally unique across
# both blocks together); test 23 requests pval = TRUE and pairwise = TRUE
# together on a nested set (checking the two independent mechanisms -
# nested_pvalues()'s chi-square p_value and add_pairwise_sig()'s
# sig_letter/sig_diff - populate their own columns without either
# clobbering the other via a join key collision).
# Tests 24-25 cover assign_sig_letters()'s new design (Joe's call, for
# pivot_crosstab() integration): a letter is now keyed on (cross_break,
# p_cat1[, p_cat2]) only, NOT outcome, so a given predictor level's letter
# is fixed once and shared identically across every outcome that uses it -
# a column's reference letter has to hold table-wide once it's shown once
# as a legend rather than repeated per block. Test 24 is assign_sig_letters()
# directly; test 25 is the same guarantee through the real calc_stats() +
# add_pairwise_sig() pipeline.
# =============================================================================


test_that("1. derive_se() uses estimate_se directly when conf = \"se\" was requested", {
  data <- tibble(estimate_se = c("0.05", "0.10"))
  expect_equal(derive_se(data), c(0.05, 0.10))
})


test_that("2. derive_se() derives SE from the CI half-width when conf = \"ci\" was requested", {
  # estimate ± 1.96*SE = CI, per apply_conf_columns() in calc_stats.R -
  # SE = (upp - low) / (2*1.96). Built here with SE = 0.1 exactly, so the
  # bounds are 0.5 - 0.196 = 0.304 and 0.5 + 0.196 = 0.696.
  data <- tibble(estimate_low = 0.304, estimate_upp = 0.696)
  expect_equal(derive_se(data), 0.1, tolerance = 1e-6)
})


test_that("3. derive_se() warns and returns all-NA when conf was never requested, rather than stopping the whole call", {
  # calc_stats() always creates estimate_se, filled with the literal string
  # "-" when conf = NULL - see apply_conf_columns(). Used to stop() outright
  # - softened so calc_stats(pairwise = TRUE) without conf set doesn't lose
  # pairwise testing for blocks that have a real covmat and never needed
  # this fallback in the first place (see calc_stats.R's own header comment
  # on derive_se()).
  data <- tibble(estimate_se = c("-", "-"))
  expect_warning(result <- derive_se(data), "conf")
  expect_true(all(is.na(result)))
})


test_that("4. assign_sig_letters() assigns a/b/c within one (outcome, cross_break) block, in predictor level order", {
  eligible <- tibble(
    outcome = "response", cross_break = "region",
    p_cat1 = c("North", "South", "East", "North", "South", "East")   # repeats - one per o_cat row
  )
  lookup <- assign_sig_letters(eligible)

  expect_equal(nrow(lookup), 3)   # distinct() - one row per predictor level, not per (level, o_cat) row
  expect_equal(lookup$p_cat1, c("North", "South", "East"))
  expect_equal(lookup$sig_letter, c("a", "b", "c"))
})


test_that("5. assign_sig_letters() continues the letter sequence across blocks rather than resetting", {
  # Two blocks - (outcome1, region) with 2 levels, then (outcome1, age_group)
  # with 2 levels - letters should run a, b, c, d straight through, not
  # a, b then a, b again.
  eligible <- tibble(
    outcome     = c("q1", "q1", "q1", "q1"),
    cross_break = c("region", "region", "age_group", "age_group"),
    p_cat1      = c("North", "South", "Young", "Old")
  )
  lookup <- assign_sig_letters(eligible)

  expect_equal(lookup$sig_letter, c("a", "b", "c", "d"))
})


test_that("6. pairwise_test_one_group() marks a clearly significant pair on both sides", {
  rows <- tibble(
    p_cat1 = c("North", "South"), sig_letter = c("a", "b"),
    estimate = c(0.8, 0.2), .se = c(0.12649, 0.12649)
  )
  result <- pairwise_test_one_group(rows, alpha = 0.05)

  expect_equal(result$sig_diff[result$p_cat1 == "North"], "b")
  expect_equal(result$sig_diff[result$p_cat1 == "South"], "a")
})


test_that("7. pairwise_test_one_group() marks a clearly non-significant pair as \"\", not NA", {
  rows <- tibble(
    p_cat1 = c("North", "East"), sig_letter = c("a", "c"),
    estimate = c(0.8, 0.7), .se = c(0.12649, 0.14491)
  )
  result <- pairwise_test_one_group(rows, alpha = 0.05)

  expect_equal(result$sig_diff, c("", ""))
})


test_that("8. pairwise_test_one_group() returns \"\" for a single-row group - nothing to compare against", {
  rows <- tibble(p_cat1 = "North", sig_letter = "a", estimate = 0.8, .se = 0.12649)
  result <- pairwise_test_one_group(rows, alpha = 0.05)

  expect_equal(result$sig_diff, "")
})


test_that("9. pairwise_test_one_group() skips a pair with an NA SE rather than erroring or forcing a verdict", {
  rows <- tibble(
    p_cat1 = c("North", "South", "East"), sig_letter = c("a", "b", "c"),
    estimate = c(0.8, 0.2, 0.7), .se = c(0.12649, NA_real_, 0.14491)
  )
  result <- pairwise_test_one_group(rows, alpha = 0.05)

  # North vs East (both real SEs) still gets evaluated normally - not
  # significant, per test 7's identical pair.
  expect_equal(result$sig_diff[result$p_cat1 == "North"], "")
  expect_equal(result$sig_diff[result$p_cat1 == "East"], "")
  # South's own comparisons (both involve its NA SE) contribute nothing.
  expect_equal(result$sig_diff[result$p_cat1 == "South"], "")
})


test_that("10. add_pairwise_sig() matches hand-calculated pairwise z-tests across three regions", {
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2),    # North: 8/10 Yes
                         rep("Yes", 2), rep("No", 8),    # South: 2/10 Yes
                         rep("Yes", 7), rep("No", 3)),   # East:  7/10 Yes
                       levels = c("No", "Yes")),
    region = factor(c(rep("North", 10), rep("South", 10), rep("East", 10)),
                     levels = c("North", "South", "East"))
  )

  stats_table <- calc_stats(data, outcomes = "response", predictors = "region",
                             statistics = "perc", conf = "se", multicode = FALSE)
  result <- add_pairwise_sig(stats_table, alpha = 0.05)

  yes_rows <- result %>% filter(o_cat == "Yes", cross_break == "region")
  expect_equal(nrow(yes_rows), 3)

  north <- yes_rows %>% filter(p_cat1 == "North")
  south <- yes_rows %>% filter(p_cat1 == "South")
  east  <- yes_rows %>% filter(p_cat1 == "East")

  expect_equal(north$sig_letter, "a")
  expect_equal(south$sig_letter, "b")
  expect_equal(east$sig_letter, "c")

  expect_equal(north$sig_diff, "b")        # significant vs South only
  expect_equal(south$sig_diff, "a, c")     # significant vs both North and East
  expect_equal(east$sig_diff, "b")         # significant vs South only

  # cross_break == "Total" rows (calc_stats()'s own overall estimate,
  # present alongside the region breakdown in every call) aren't part of
  # any comparison set - present in the output, untouched.
  total_rows <- result %>% filter(cross_break == "Total")
  expect_true(nrow(total_rows) > 0)
  expect_true(all(is.na(total_rows$sig_letter)))
  expect_true(all(is.na(total_rows$sig_diff)))
})


test_that("11. add_pairwise_sig() continues letters across two predictor sets rather than resetting", {
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2),
                         rep("Yes", 2), rep("No", 8)),
                       levels = c("No", "Yes")),
    region    = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South")),
    age_group = factor(rep(c("Young", "Old"), 10), levels = c("Young", "Old"))
  )

  stats_table <- calc_stats(data, outcomes = "response",
                             predictors = list("region", "age_group"),
                             statistics = "perc", conf = "se", multicode = FALSE)
  result <- add_pairwise_sig(stats_table, alpha = 0.05)

  yes_rows <- result %>% filter(o_cat == "Yes")

  region_letters <- yes_rows %>% filter(cross_break == "region") %>%
    distinct(p_cat1, sig_letter)
  age_letters <- yes_rows %>% filter(cross_break == "age_group") %>%
    distinct(p_cat1, sig_letter)

  # region's own two levels get a/b - age_group's, processed second, picks
  # up from c, not restarting at a.
  expect_setequal(region_letters$sig_letter, c("a", "b"))
  expect_setequal(age_letters$sig_letter, c("c", "d"))
})


test_that("12. add_pairwise_sig() leaves ineligible rows (wrong stat, or no predictor at all) as NA, not an error", {
  numeric_data <- tibble(age = c(10, 20, 30, 40))
  stats_table  <- calc_stats(numeric_data, outcomes = "age", statistics = "mean", conf = "se")
  result <- add_pairwise_sig(stats_table, alpha = 0.05)

  # No predictor was requested - cross_break is "Total" for every row, and
  # "mean" isn't an eligible stat either - both reasons this should end up
  # untouched rather than erroring.
  expect_true(all(is.na(result$sig_letter)))
  expect_true(all(is.na(result$sig_diff)))
})


test_that("13. add_pairwise_sig() only tests eligible rows when a call mixes an eligible and an ineligible statistic", {
  # NOTE: this test's "ineligible statistic" has had to change twice now, as
  # pairwise_eligible_stats has grown over the course of this session -
  # originally "mean" (until mean/w_mean were added, see
  # weighted_mean_svyby()'s own work), then "median" (until median/w_median
  # were added too, see pairwise_eligible_stats' own header comment on why
  # there's no exact path for medians but an approximate one was still added
  # regardless). Swapped to "sum" now - nothing in this project's design
  # anywhere suggests pairwise support is heading there next (unlike
  # mean/median, sum was never the SUBJECT of any pairwise discussion this
  # session), so it should stay a stable choice rather than needing a third
  # swap later. If it ever does get added, this test breaking (rows that
  # were expected NA suddenly getting real letters) is exactly the right
  # failure mode to catch that and prompt another swap, same as happened
  # here twice already.
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2), rep("Yes", 2), rep("No", 8)),
                       levels = c("No", "Yes")),
    age      = c(20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58),
    region   = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South"))
  )

  stats_table <- calc_stats(data, outcomes = c("response", "age"), predictors = "region",
                             statistics = c("perc", "sum"), conf = "se", multicode = FALSE)
  result <- add_pairwise_sig(stats_table, alpha = 0.05)

  perc_rows <- result %>% filter(stat == "perc", cross_break == "region")
  sum_rows  <- result %>% filter(stat == "sum", cross_break == "region")

  expect_true(all(!is.na(perc_rows$sig_letter)))
  expect_true(all(is.na(sum_rows$sig_letter)))
  expect_true(all(is.na(sum_rows$sig_diff)))
})


test_that("14. exact_se_diff() computes Var_i + Var_j - 2*Cov_ij from a real covariance matrix", {
  m <- matrix(c(0.02, 0.005, 0.005, 0.03), nrow = 2,
              dimnames = list(c("North:responseYes", "South:responseYes"),
                               c("North:responseYes", "South:responseYes")))
  result <- exact_se_diff(m, "North", "South", "response", "Yes", stat = "perc")

  expect_equal(result, sqrt(0.02 + 0.03 - 2 * 0.005), tolerance = 1e-8)
  # different from the naive independent-sum answer, since Cov != 0 here -
  # confirms this is actually using the off-diagonal term, not just
  # reconstructing sqrt(Var_i + Var_j) from the same matrix's diagonal.
  expect_false(isTRUE(all.equal(result, sqrt(0.02 + 0.03))))
})


test_that("15. exact_se_diff() returns NA when a coefficient name isn't found in the matrix", {
  m <- matrix(1, nrow = 1, dimnames = list("North:responseYes", "North:responseYes"))
  expect_true(is.na(exact_se_diff(m, "North", "South", "response", "Yes", stat = "perc")))
})


test_that("16. pairwise_test_one_group() uses the exact covmat-based SE, not the approximate .se, when both are present", {
  # Deliberately wrong .se values (huge - would give an obviously
  # non-significant result if actually used) alongside a real covmat with a
  # substantial NEGATIVE Cov_ij between North and South. Var(diff) =
  # Var_N + Var_S - 2*Cov_NS = 0.016 + 0.016 - 2*(-0.010) = 0.052,
  # SE_diff = 0.2280, z = 0.6/0.2280 = 2.632, p ~ 0.0085 -> significant.
  # The .se-based answer (SE_diff = sqrt(100^2+100^2) = 141.4, z ~ 0.004)
  # would NOT be significant - the two paths disagree here on purpose, so
  # whichever verdict comes out pins down which one actually ran.
  m <- matrix(0, nrow = 2, ncol = 2,
              dimnames = list(c("North:responseYes", "South:responseYes"),
                               c("North:responseYes", "South:responseYes")))
  m["North:responseYes", "North:responseYes"] <- 0.016
  m["South:responseYes", "South:responseYes"] <- 0.016
  m["North:responseYes", "South:responseYes"] <- -0.010
  m["South:responseYes", "North:responseYes"] <- -0.010

  # outcome/o_cat/stat passed via group_keys, not as columns on rows -
  # matching how group_modify() actually calls this (see the call site in
  # add_pairwise_sig() for why: outcome/o_cat/stat are grouping columns,
  # group_modify() strips those out of .x by default and supplies them
  # separately via .y instead). stat = "perc" here since exact_se_diff()
  # now branches its key-building on group_keys$stat (added for the
  # mean/w_mean work) and these covmat rownames are colon-suffixed
  # ("North:responseYes"), the perc-style key shape.
  rows <- tibble(
    p_cat1 = c("North", "South"), sig_letter = c("a", "b"),
    estimate = c(0.8, 0.2),
    .se = c(100, 100),
    covmat = list(m, m)
  )
  group_keys <- tibble(outcome = "response", o_cat = "Yes", stat = "perc")
  result <- pairwise_test_one_group(rows, alpha = 0.05, group_keys)

  expect_equal(result$sig_diff, c("b", "a"))
})


test_that("17. pairwise_test_one_group() falls back to the approximate .se path when covmat is present but not a real matrix (e.g. NA from bind_rows())", {
  rows <- tibble(
    p_cat1 = c("North", "South"), sig_letter = c("a", "b"),
    outcome = "response", o_cat = "Yes",
    estimate = c(0.8, 0.2), .se = c(0.12649, 0.12649),
    covmat = list(NA, NA)
  )
  result <- pairwise_test_one_group(rows, alpha = 0.05)

  # Same verdict as test 6's identical pair via the approximate path -
  # confirms is.matrix() correctly treats NA as "no real covariance to use",
  # not as a value it tries to index into (which would error, not silently
  # misbehave - this test would fail loudly either way if that guard broke).
  expect_equal(result$sig_diff[result$p_cat1 == "North"], "b")
  expect_equal(result$sig_diff[result$p_cat1 == "South"], "a")
})


test_that("18. calc_stats(pairwise = TRUE) end-to-end matches test 10's hand-calculated case via the exact covmat path", {
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2),    # North: 8/10 Yes
                         rep("Yes", 2), rep("No", 8),    # South: 2/10 Yes
                         rep("Yes", 7), rep("No", 3)),   # East:  7/10 Yes
                       levels = c("No", "Yes")),
    region = factor(c(rep("North", 10), rep("South", 10), rep("East", 10)),
                     levels = c("North", "South", "East")),
    wt = rep(1, 30)   # unweighted - and ids = 1 (unclustered) below - so the
                       # true covariance between these disjoint groups is
                       # genuinely zero, and this should reproduce test 10's
                       # exact numbers via the covmat path, not just some
                       # other self-consistent answer.
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  result <- calc_stats(design, outcomes = "response", predictors = "region",
                        statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)

  expect_true("covmat" %in% names(result))

  yes_rows <- result %>% filter(o_cat == "Yes", cross_break == "region")
  north <- yes_rows %>% filter(p_cat1 == "North")
  south <- yes_rows %>% filter(p_cat1 == "South")
  east  <- yes_rows %>% filter(p_cat1 == "East")

  expect_true(is.matrix(north$covmat[[1]]))

  expect_equal(north$sig_diff, "b")
  expect_equal(south$sig_diff, "a, c")
  expect_equal(east$sig_diff, "b")
})


test_that("19. assign_sig_letters() differentiates rows sharing a p_cat1 but differing on p_cat2 (a nested predictor set)", {
  eligible <- tibble(
    outcome = "response", cross_break = "sex_X_age_group",
    p_cat1 = c("Male", "Male", "Female", "Female"),
    p_cat2 = c("Young", "Old", "Young", "Old")
  )
  lookup <- assign_sig_letters(eligible)

  # p_cat1 alone would collapse Young/Old together within each sex - 2 rows,
  # not 4, and Male.Young/Male.Old would wrongly share one letter.
  expect_equal(nrow(lookup), 4)
  expect_equal(length(unique(lookup$sig_letter)), 4)
})


test_that("20. pairwise_test_one_group() builds the combined \"p_cat1.p_cat2\" key and compares across every combination, not just within a shared p_cat1", {
  # Coefficient names use svyby()'s own "level1.level2:outcomeCat" format
  # (confirmed by direct inspection - see check_svyby_nested_naming.R).
  # Off-diagonal covariance left at 0 (independent) for simplicity - this
  # test is about the KEY CONSTRUCTION and comparison scope, not the
  # covariance arithmetic itself (already covered by test 14).
  labels <- c("Male.Young:responseYes", "Male.Old:responseYes",
              "Female.Young:responseYes", "Female.Old:responseYes")
  m <- matrix(0, nrow = 4, ncol = 4, dimnames = list(labels, labels))
  diag(m) <- 0.016

  # Deliberately wrong, huge .se values - same trick as test 16: if the
  # approximate (independent-SE) path ran instead of the exact covmat path,
  # nothing here would come out significant, so a significant verdict below
  # confirms the exact path (and its key construction) actually ran.
  rows <- tibble(
    p_cat1 = c("Male", "Male", "Female", "Female"),
    p_cat2 = c("Young", "Old", "Young", "Old"),
    sig_letter = c("a", "b", "c", "d"),
    estimate = c(0.8, 0.2, 0.8, 0.2),
    .se = c(100, 100, 100, 100),
    covmat = list(m, m, m, m)
  )
  group_keys <- tibble(outcome = "response", o_cat = "Yes", stat = "perc")
  result <- pairwise_test_one_group(rows, alpha = 0.05, group_keys)

  # Var(diff) = 0.016 + 0.016 = 0.032, SE_diff = 0.17889 throughout (equal
  # variances, zero covariance). Male.Young vs Female.Young (diff = 0,
  # different p_cat1, SAME p_cat2) and Male.Old vs Female.Old (same story)
  # -> z = 0, not significant. Every cross-cluster pair (diff = 0.6) ->
  # z = 3.354, p ~ 0.0008, significant - including Male.Young vs Male.Old
  # (same p_cat1) AND Male.Young vs Female.Old (different on both).
  my <- result %>% filter(p_cat1 == "Male", p_cat2 == "Young")
  mo <- result %>% filter(p_cat1 == "Male", p_cat2 == "Old")
  fy <- result %>% filter(p_cat1 == "Female", p_cat2 == "Young")
  fo <- result %>% filter(p_cat1 == "Female", p_cat2 == "Old")

  expect_equal(my$sig_diff, paste(sort(c(mo$sig_letter, fo$sig_letter)), collapse = ", "))
  expect_equal(fy$sig_diff, paste(sort(c(mo$sig_letter, fo$sig_letter)), collapse = ", "))
  expect_equal(mo$sig_diff, paste(sort(c(my$sig_letter, fy$sig_letter)), collapse = ", "))
  expect_equal(fo$sig_diff, paste(sort(c(my$sig_letter, fy$sig_letter)), collapse = ", "))
})


test_that("21. calc_stats(pairwise = TRUE) end-to-end on a real nested (2-variable) predictor set", {
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2),    # Male.Young:   8/10 Yes
                         rep("Yes", 2), rep("No", 8),    # Male.Old:     2/10 Yes
                         rep("Yes", 8), rep("No", 2),    # Female.Young: 8/10 Yes
                         rep("Yes", 2), rep("No", 8)),   # Female.Old:   2/10 Yes
                       levels = c("No", "Yes")),
    sex       = factor(c(rep("Male", 20), rep("Female", 20)), levels = c("Male", "Female")),
    age_group = factor(rep(c(rep("Young", 10), rep("Old", 10)), 2), levels = c("Young", "Old")),
    wt        = rep(1, 40)   # unweighted, unclustered (ids = 1 below) - true
                              # covariance between disjoint groups is
                              # genuinely zero, same reasoning as test 18.
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  result <- calc_stats(design, outcomes = "response", predictors = list(c("sex", "age_group")),
                        statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)

  expect_true("covmat" %in% names(result))
  expect_true("p_cat2" %in% names(result))

  yes_rows <- result %>% filter(o_cat == "Yes", cross_break == "sex_X_age_group")
  expect_equal(nrow(yes_rows), 4)

  my <- yes_rows %>% filter(p_cat1 == "Male", p_cat2 == "Young")
  mo <- yes_rows %>% filter(p_cat1 == "Male", p_cat2 == "Old")
  fy <- yes_rows %>% filter(p_cat1 == "Female", p_cat2 == "Young")
  fo <- yes_rows %>% filter(p_cat1 == "Female", p_cat2 == "Old")

  expect_true(is.matrix(my$covmat[[1]]))
  expect_equal(length(unique(c(my$sig_letter, mo$sig_letter, fy$sig_letter, fo$sig_letter))), 4)

  # "high" cluster (Male.Young, Female.Young, both 0.8) - not significant vs
  # each other despite differing on sex (p_cat1). This is the case Joe
  # asked about directly: is it valid to compare "women 18-24" to "men
  # 18-24"? Yes - here they're compared, and correctly found not different.
  expect_equal(my$sig_diff, paste(sort(c(mo$sig_letter, fo$sig_letter)), collapse = ", "))
  expect_equal(fy$sig_diff, paste(sort(c(mo$sig_letter, fo$sig_letter)), collapse = ", "))
  # "low" cluster (Male.Old, Female.Old, both 0.2) - same story.
  expect_equal(mo$sig_diff, paste(sort(c(my$sig_letter, fy$sig_letter)), collapse = ", "))
  expect_equal(fo$sig_diff, paste(sort(c(my$sig_letter, fy$sig_letter)), collapse = ", "))

  # cross_break == "Total" (calc_stat_engine()'s own always-present overall
  # row, unrelated to the nested breakdown) stays untouched, same as tests
  # 10/18 - p_cat2 there is the literal string "Total" too (calc_stat_engine()
  # fills every NA cross_break/p_cat/predictor column with "Total", not just
  # p_cat1), so it's excluded from pairwise testing the same way.
  total_rows <- result %>% filter(cross_break == "Total")
  expect_true(nrow(total_rows) > 0)
  expect_true(all(is.na(total_rows$sig_letter)))
})


test_that("22. calc_stats(pairwise = TRUE) mixing a flat predictor set and a nested one in the same call - no cross-contamination between blocks", {
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2),    # Male.Young:   8/10 Yes
                         rep("Yes", 2), rep("No", 8),    # Male.Old:     2/10 Yes
                         rep("Yes", 8), rep("No", 2),    # Female.Young: 8/10 Yes
                         rep("Yes", 2), rep("No", 8)),   # Female.Old:   2/10 Yes
                       levels = c("No", "Yes")),
    sex       = factor(c(rep("Male", 20), rep("Female", 20)), levels = c("Male", "Female")),
    age_group = factor(rep(c(rep("Young", 10), rep("Old", 10)), 2), levels = c("Young", "Old")),
    # A third, independent flat predictor cutting across the sex/age_group
    # blocks (first 5 of every 10-row block are North, last 5 South) rather
    # than aligning with either - this test is about structural correctness
    # (does mixing blocks corrupt either one), not new z-test arithmetic
    # (already covered by tests 10/18/21), so the exact proportion doesn't
    # matter beyond being non-degenerate - works out to North 14/20 = 0.7,
    # South 6/20 = 0.3, a real (and significant) difference.
    region = factor(rep(rep(c("North", "South"), each = 5), 4), levels = c("North", "South")),
    wt = rep(1, 40)
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  region_alone <- calc_stats(design, outcomes = "response", predictors = "region",
                              statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)
  nested_alone <- calc_stats(design, outcomes = "response", predictors = list(c("sex", "age_group")),
                              statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)
  combined <- calc_stats(design, outcomes = "response",
                          predictors = list("region", c("sex", "age_group")),
                          statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)

  # region's own estimates and significance VERDICT (whether sig_diff is
  # empty or not) come out identical whether region is requested alone or
  # alongside the nested set. Letters themselves aren't compared directly -
  # global numbering legitimately continues across whichever blocks are
  # present in a given call, so the same predictor level can pick up a
  # different letter in the combined call than it does alone.
  region_alone_yes    <- region_alone %>% filter(o_cat == "Yes", cross_break == "region") %>% arrange(p_cat1)
  region_combined_yes <- combined     %>% filter(o_cat == "Yes", cross_break == "region") %>% arrange(p_cat1)

  expect_equal(region_alone_yes$estimate, region_combined_yes$estimate, tolerance = 1e-8)
  expect_equal(region_alone_yes$sig_diff == "", region_combined_yes$sig_diff == "")
  expect_true(any(region_combined_yes$sig_diff != ""))   # confirms this is exercising the non-trivial branch

  # same check for the nested block.
  nested_alone_yes    <- nested_alone %>% filter(o_cat == "Yes", cross_break == "sex_X_age_group") %>%
    arrange(p_cat1, p_cat2)
  nested_combined_yes <- combined     %>% filter(o_cat == "Yes", cross_break == "sex_X_age_group") %>%
    arrange(p_cat1, p_cat2)

  expect_equal(nested_alone_yes$estimate, nested_combined_yes$estimate, tolerance = 1e-8)
  expect_equal(nested_alone_yes$sig_diff == "", nested_combined_yes$sig_diff == "")

  # letters are globally unique across BOTH blocks in the combined call -
  # assign_sig_letters() correctly extends across a mix of flat and nested
  # cross_break blocks in one distinct() call, not just multiple flat ones
  # (test 11) or a single nested one (test 19). Distinct on (cross_break,
  # p_cat1, p_cat2) first - sig_letter is joined without o_cat, so it's
  # legitimately repeated once per o_cat row (Yes AND No) for the same
  # predictor level; that repetition isn't what this check is about.
  distinct_letters <- combined %>% filter(!is.na(sig_letter)) %>%
    distinct(cross_break, p_cat1, p_cat2, sig_letter) %>% pull(sig_letter)
  expect_equal(length(unique(distinct_letters)), length(distinct_letters))
})


test_that("23. calc_stats(pval = TRUE, pairwise = TRUE) together on a nested predictor set - both mechanisms populate their own columns without interfering with each other", {
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2),
                         rep("Yes", 2), rep("No", 8),
                         rep("Yes", 8), rep("No", 2),
                         rep("Yes", 2), rep("No", 8)),
                       levels = c("No", "Yes")),
    sex       = factor(c(rep("Male", 20), rep("Female", 20)), levels = c("Male", "Female")),
    age_group = factor(rep(c(rep("Young", 10), rep("Old", 10)), 2), levels = c("Young", "Old")),
    wt = rep(1, 40)
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  result <- calc_stats(design, outcomes = "response", predictors = list(c("sex", "age_group")),
                        statistics = "w_perc", conf = "se", multicode = FALSE,
                        pval = TRUE, pairwise = TRUE)

  expect_true(all(c("sig_letter", "sig_diff", "p_value", "p_method", "covmat") %in% names(result)))

  yes_rows <- result %>% filter(o_cat == "Yes", cross_break == "sex_X_age_group")
  expect_equal(nrow(yes_rows), 4)

  # pairwise machinery still works exactly as test 21 - not displaced or
  # broken by pval = TRUE also being requested in the same call.
  expect_true(all(!is.na(yes_rows$sig_letter)))
  expect_true(is.matrix(yes_rows$covmat[[1]]))

  # nested_pvalues() also ran - one p-value per OUTER level (sex), testing
  # age_group's association with response WITHIN that level - present for
  # every row, and (per nested_pvalues()'s own design, joined on p_cat1
  # only, not p_cat2) shared identically between Young and Old within the
  # same sex, since it's testing age_group's overall association with
  # response within that sex, not a per-cell value.
  expect_true(all(!is.na(yes_rows$p_value)))
  male_pvals   <- yes_rows %>% filter(p_cat1 == "Male") %>% pull(p_value)
  female_pvals <- yes_rows %>% filter(p_cat1 == "Female") %>% pull(p_value)
  expect_equal(male_pvals[1], male_pvals[2])
  expect_equal(female_pvals[1], female_pvals[2])
})


test_that("24. assign_sig_letters() gives the same (cross_break, p_cat1) the SAME letter regardless of which outcome it appears under", {
  # region appears under two different outcomes here - q1's rows come
  # first, q2's second. Previously (keyed on outcome too), q2's North/South
  # would have picked up FRESH letters (c/d) continuing on from q1's a/b -
  # now there's exactly one lookup row per (cross_break, p_cat1), shared by
  # both outcomes.
  eligible <- tibble(
    outcome     = c("q1", "q1", "q2", "q2"),
    cross_break = c("region", "region", "region", "region"),
    p_cat1      = c("North", "South", "North", "South")
  )
  lookup <- assign_sig_letters(eligible)

  expect_false("outcome" %in% names(lookup))
  expect_equal(nrow(lookup), 2)
  expect_equal(lookup$sig_letter, c("a", "b"))
})


test_that("25. calc_stats(pairwise = TRUE) with two outcomes sharing one predictor - the same predictor level gets the same sig_letter in both outcomes' blocks", {
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2),     # North: 8/10 Yes
                         rep("Yes", 2), rep("No", 8)),    # South: 2/10 Yes
                       levels = c("No", "Yes")),
    opinion  = factor(c(rep("Agree", 7), rep("Disagree", 3),   # North: 7/10 Agree
                         rep("Agree", 3), rep("Disagree", 7)), # South: 3/10 Agree
                       levels = c("Disagree", "Agree")),
    region   = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South"))
  )

  stats_table <- calc_stats(data, outcomes = c("response", "opinion"), predictors = "region",
                             statistics = "perc", conf = "se", multicode = FALSE)
  result <- add_pairwise_sig(stats_table, alpha = 0.05)

  north_response <- result %>% filter(outcome == "response", o_cat == "Yes", p_cat1 == "North") %>% pull(sig_letter)
  north_opinion  <- result %>% filter(outcome == "opinion", o_cat == "Agree", p_cat1 == "North") %>% pull(sig_letter)
  south_response <- result %>% filter(outcome == "response", o_cat == "Yes", p_cat1 == "South") %>% pull(sig_letter)
  south_opinion  <- result %>% filter(outcome == "opinion", o_cat == "Agree", p_cat1 == "South") %>% pull(sig_letter)

  # the letter belonging to "North" is identical whether you're looking at
  # response's block or opinion's - the actual guarantee this whole change
  # was for, since a column's reference letter (once wired into pivot_
  # crosstab()) has to hold across every outcome's row under it.
  expect_equal(north_response, north_opinion)
  expect_equal(south_response, south_opinion)
  expect_false(north_response == south_response)
})


# Tests 26-29 cover the numeric-mean pairwise integration (weighted_mean_
# svyby()/tidy_svyby_mean(), added in calc_stats.R alongside the existing
# proportion machinery - see that file's own header notes and
# check_svyby_mean_naming.R for the real-output confirmation this was built
# against). Test 26 is exact_se_diff()'s new stat-aware key branch, directly:
# a bare level for mean/w_mean, the existing colon-suffixed key for
# everything else - both checked in one test since they're two branches of
# the same `if`, not two separate concerns. Tests 27-28 are calc_stats(
# pairwise = TRUE) end-to-end for "mean" and "w_mean" SEPARATELY, not
# together - deliberately, since they take genuinely different paths
# (unweighted "mean" never gets a real covmat and stays on the independent-SE
# approximate path, exact for unclustered data; "w_mean" gets
# weighted_mean_svyby()'s real covmat and the exact path) and a single
# combined test could pass for the wrong reason if one path silently fell
# back to the other's behaviour. Test 27 also confirms the "mean" case really
# did take the approximate path - no covmat column in the table AT ALL (not
# NULL, not NA, genuinely absent, since no svyby()-based function ever ran to
# create one) - rather than happening to get the right answer some other way.
# Test 29 is the same "mixed eligible stats in one call don't
# cross-contaminate" guarantee tests 22-23 already established for
# flat+nested proportions, now for a percentage outcome and a mean outcome
# requested together.

test_that("26. exact_se_diff() builds a bare-level key for mean/w_mean, the existing colon-suffixed key for everything else", {
  # A 2x2 covmat with mean-style bare-level dimnames ("North"/"South") -
  # matches what weighted_mean_svyby()'s tidy_svyby_mean() actually produces
  # (confirmed by check_svyby_mean_naming.R), not the "level:outcomeCategory"
  # shape a factor outcome gives.
  mean_covmat <- matrix(c(4, 0, 0, 9), nrow = 2,
                         dimnames = list(c("North", "South"), c("North", "South")))
  se_diff_mean <- exact_se_diff(mean_covmat, "North", "South", outcome = "score", o_cat = "w_mean",
                                 stat = "w_mean")
  expect_equal(se_diff_mean, sqrt(4 + 9 - 2 * 0))

  # Same matrix, but a stat NOT in c("mean", "w_mean") - exact_se_diff()
  # should build the OLD colon-suffixed key instead, which won't be found in
  # this mean-shaped matrix, so it should return NA rather than silently
  # matching something wrong.
  se_diff_wrong_branch <- exact_se_diff(mean_covmat, "North", "South", outcome = "score", o_cat = "w_mean",
                                         stat = "w_perc")
  expect_true(is.na(se_diff_wrong_branch))

  # And the reverse - a percentage-shaped covmat (colon-suffixed dimnames),
  # with stat = "w_perc" correctly finding it.
  perc_covmat <- matrix(c(0.01, 0, 0, 0.02), nrow = 2,
                         dimnames = list(c("North:responseYes", "South:responseYes"),
                                         c("North:responseYes", "South:responseYes")))
  se_diff_perc <- exact_se_diff(perc_covmat, "North", "South", outcome = "response", o_cat = "Yes",
                                 stat = "w_perc")
  expect_equal(se_diff_perc, sqrt(0.01 + 0.02 - 2 * 0))
})


test_that("27. calc_stats(pairwise = TRUE) end-to-end for unweighted \"mean\" - sig_letter/sig_diff populated via the approximate path, no covmat column anywhere in the table", {
  # Small real within-group spread (NOT a constant value per group) is
  # deliberate - unweighted_mean()'s SE is sd/sqrt(n), so a perfectly
  # constant group would give SE = 0, and pairwise_test_one_group() skips
  # any pair with se_diff == 0 entirely rather than treating it as
  # significant - this would silently produce sig_diff = "" for both rows
  # instead of exercising the comparison at all.
  data <- tibble(
    score  = c(rep(c(8, 9, 10, 11, 12), 2), rep(c(48, 49, 50, 51, 52), 2)),   # North ~10, South ~50
    region = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South"))
  )
  result <- calc_stats(data, outcomes = "score", predictors = "region",
                        statistics = "mean", conf = "se", multicode = FALSE, pairwise = TRUE)

  # No svyby()-based function ever ran (unweighted "mean" stays on
  # unweighted_mean()) - the column shouldn't exist at all, not just be
  # NULL/NA per row.
  expect_false("covmat" %in% names(result))

  region_rows <- result %>% filter(cross_break == "region")
  expect_equal(length(unique(region_rows$sig_letter)), 2)
  expect_true(all(region_rows$sig_diff != ""))   # obviously-different groups, small spread - should be significant
  north_letter <- region_rows %>% filter(p_cat1 == "North") %>% pull(sig_letter)
  south_letter <- region_rows %>% filter(p_cat1 == "South") %>% pull(sig_letter)
  expect_equal(region_rows %>% filter(p_cat1 == "North") %>% pull(sig_diff), south_letter)
  expect_equal(region_rows %>% filter(p_cat1 == "South") %>% pull(sig_diff), north_letter)
})


# Same 2x2 unweighted/unclustered shape as test 12 in
# test_weighted_mean_svyby.R (10..80 in pairs) - Male/Young mean 15,
# Male/Old mean 35, Female/Young mean 55, Female/Old mean 75, every group's
# SE exactly 5.0. All 4 groups pairwise-significantly different from each
# other at this spread (differences of 20+ against SE_diff = sqrt(2*5^2) =
# 7.07, z >= 2.83 for the smallest gap) - deliberately so every row gets a
# real sig_diff to check, not just "the column exists".
test_that("28. calc_stats(pairwise = TRUE) end-to-end for \"w_mean\" - a real covmat, the exact path", {
  data <- tibble(
    score     = c(10, 20, 30, 40, 50, 60, 70, 80),
    sex       = factor(c("Male", "Male", "Male", "Male", "Female", "Female", "Female", "Female"),
                        levels = c("Male", "Female")),
    age_group = factor(c("Young", "Young", "Old", "Old", "Young", "Young", "Old", "Old"),
                        levels = c("Young", "Old")),
    wt        = rep(1, 8)
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  result <- calc_stats(design, outcomes = "score", predictors = list(c("sex", "age_group")),
                        statistics = "w_mean", conf = "se", multicode = FALSE, pairwise = TRUE)

  nested_rows <- result %>% filter(cross_break == "sex_X_age_group")
  expect_true(is.matrix(nested_rows$covmat[[1]]))
  expect_equal(length(unique(nested_rows$sig_letter)), 4)

  my <- nested_rows %>% filter(p_cat1 == "Male", p_cat2 == "Young")
  fo <- nested_rows %>% filter(p_cat1 == "Female", p_cat2 == "Old")
  # every row differs significantly from every other row here (see header
  # note) - each one's sig_diff should list the OTHER 3 groups' letters.
  expect_equal(length(strsplit(my$sig_diff, ", ")[[1]]), 3)
  expect_equal(length(strsplit(fo$sig_diff, ", ")[[1]]), 3)
})


test_that("29. calc_stats(pairwise = TRUE) with a percentage outcome and a mean outcome requested together - no cross-contamination", {
  data <- tibble(
    response = factor(c(rep("Yes", 8), rep("No", 2),    # North: 8/10 Yes
                         rep("Yes", 2), rep("No", 8)),   # South: 2/10 Yes
                       levels = c("No", "Yes")),
    # Small real within-group spread, not a constant - see test 27's header
    # note on why a constant-per-group value would silently break the
    # comparison (SE = 0, pair skipped entirely) rather than testing it.
    score    = c(rep(c(8, 9, 10, 11, 12), 2), rep(c(48, 49, 50, 51, 52), 2)),   # North ~10, South ~50
    region   = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South"))
  )

  result <- calc_stats(data, outcomes = c("response", "score"), predictors = "region",
                        statistics = c("perc", "mean"), conf = "se", multicode = FALSE, pairwise = TRUE)

  perc_rows <- result %>% filter(outcome == "response", o_cat == "Yes", cross_break == "region")
  mean_rows <- result %>% filter(outcome == "score", cross_break == "region")

  expect_equal(nrow(perc_rows), 2)
  expect_equal(nrow(mean_rows), 2)

  # letters are independently assigned per (cross_break, p_cat1) - both
  # blocks share the SAME cross_break ("region") and the SAME two p_cat1
  # values (North/South), so per assign_sig_letters()'s own design (keyed on
  # cross_break/p_cat1 only, not outcome or stat), North and South should
  # get the SAME letters in both blocks - this is the expected, correct
  # behaviour (see test 25's identical guarantee for two percentage
  # outcomes), not a sign of contamination. What WOULD indicate
  # contamination is sig_diff itself: each block's comparison must be
  # computed only against rows from its OWN (outcome, cross_break, stat,
  # o_cat) group - checked here via each block correctly finding a
  # significant difference using ITS OWN estimates/SEs, not the other
  # block's.
  expect_true(all(perc_rows$sig_diff != ""))
  expect_true(all(mean_rows$sig_diff != ""))

  # Neither "perc" nor "mean" is a svyby()-based stat (only "w_perc"/
  # "w_mean" are, and neither was requested here) - no covmat column should
  # exist anywhere in the table at all, the same absent-not-NULL situation
  # test 27 checks for unweighted "mean" alone. Both blocks getting their
  # own correct sig_diff (checked above) with NO covmat present at all is
  # what actually demonstrates they're each running their own independent
  # approximate-path comparison, not somehow sharing state.
  expect_false("covmat" %in% names(result))
})


# Tests 30-31: median/w_median added to pairwise_eligible_stats. UNLIKE mean,
# there is no exact/covmat path here at all, for either the weighted or
# unweighted case - not a scope decision the way medians-vs-means-in-general
# was, but a hard limitation of the survey package itself (svyquantile()
# doesn't return influence functions, so covmat = TRUE has nothing to build
# from - confirmed via check_svyby_mean_naming.R's direct diagnostic run).
# So both tests exercise the approximate (independent-SE) path exclusively -
# there's no "exact path" twin test the way test 28 is to test 27 for means.
#
# Test 31 in particular is the direct check on the thing actually worth
# confirming here: that going through the approximate pairwise path doesn't
# quietly downgrade w_median's own per-group SE to something cruder. It
# doesn't need to - weighted_median() already computes a real, weighted AND
# clustering-aware SE via srvyr::survey_median()'s vartype = "se"
# (svyquantile()'s own Woodruff-CI-based SE), and "w_median" is never
# swapped to a different compute function under pairwise = TRUE (only
# "w_perc"/"w_mean" are - see active_stat_registry's own comment in
# calc_stats()), so the exact same estimate_se comes out whether or not
# pairwise testing is also requested. Only the CROSS-GROUP covariance is
# approximated as zero; each group's own SE is untouched.

test_that("30. calc_stats(pairwise = TRUE) end-to-end for unweighted \"median\" - sig_letter/sig_diff via the approximate path (grouped_medianse()'s own SE), no covmat anywhere since no exact path exists for medians at all", {
  data <- tibble(
    # Same North ~10 / South ~50 shape as test 27/29's own mean data - small
    # real within-group spread (not constant), clear between-group
    # separation.
    score  = c(rep(c(8, 9, 10, 11, 12), 2), rep(c(48, 49, 50, 51, 52), 2)),
    region = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South"))
  )
  result <- calc_stats(data, outcomes = "score", predictors = "region",
                        statistics = "median", conf = "se", multicode = FALSE, pairwise = TRUE)

  expect_false("covmat" %in% names(result))

  region_rows <- result %>% filter(cross_break == "region")
  expect_equal(length(unique(region_rows$sig_letter)), 2)
  expect_true(all(region_rows$sig_diff != ""))
  north_letter <- region_rows %>% filter(p_cat1 == "North") %>% pull(sig_letter)
  south_letter <- region_rows %>% filter(p_cat1 == "South") %>% pull(sig_letter)
  expect_equal(region_rows %>% filter(p_cat1 == "North") %>% pull(sig_diff), south_letter)
  expect_equal(region_rows %>% filter(p_cat1 == "South") %>% pull(sig_diff), north_letter)
})


test_that("31. calc_stats(pairwise = TRUE) end-to-end for \"w_median\" - approximate path, using weighted_median()'s OWN real design-based SE unchanged, not a downgraded/naive one", {
  data <- tibble(
    score  = c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28,
               80, 82, 84, 86, 88, 90, 92, 94, 96, 98),
    region = factor(c(rep("North", 10), rep("South", 10)), levels = c("North", "South")),
    wt     = rep(1, 20)
  )
  design <- data %>% srvyr::as_survey_design(ids = 1, weights = wt)

  with_pairwise <- calc_stats(design, outcomes = "score", predictors = "region",
                               statistics = "w_median", conf = "se", multicode = FALSE, pairwise = TRUE)
  without_pairwise <- calc_stats(design, outcomes = "score", predictors = "region",
                                  statistics = "w_median", conf = "se", multicode = FALSE, pairwise = FALSE)

  # No covmat column at all - there is no exact path for medians, weighted
  # or not (see the header note above).
  expect_false("covmat" %in% names(with_pairwise))

  # The actual point of this test: estimate_se should be IDENTICAL whether
  # or not pairwise = TRUE was requested, since weighted_median() itself
  # never changes - confirms the approximate path is reading (via
  # derive_se()) the same real, weighted/clustering-aware SE
  # survey_median() always produces, not silently substituting something
  # cruder just because pairwise testing was also asked for.
  region_rows_with    <- with_pairwise    %>% filter(cross_break == "region") %>% arrange(p_cat1)
  region_rows_without <- without_pairwise %>% filter(cross_break == "region") %>% arrange(p_cat1)
  expect_equal(as.numeric(region_rows_with$estimate_se), as.numeric(region_rows_without$estimate_se))

  expect_equal(length(unique(region_rows_with$sig_letter)), 2)
  expect_true(all(region_rows_with$sig_diff != ""))
})
