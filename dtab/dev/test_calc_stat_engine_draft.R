# =============================================================================
# Tests for the DRAFT inverted engine in dev/calc_stat_engine_draft.R, run
# against the synthetic BSA data in ../data.
#
# Paths use here::here(), so it runs from anywhere in the Descriptives project:
#   source(here::here("dtab", "dev", "test_calc_stat_engine_draft.R"))
#
# Two kinds of check:
#   A. Same output as the current engine (unweighted_mean()) - the refactor
#      shouldn't change any numbers, labels or column layout.
#   B. Hand calculations straight off the data - so the draft is checked
#      against the truth, not just against the old code.
# =============================================================================

suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
source(here::here("dtab", "dev", "calc_stat_engine_draft.R"))
library(testthat)

bsa <- readRDS(here::here("data", "synthetic_bsa2019.rds"))

# Numeric outcomes, all with missing values.
outcomes <- c("NatFrEst", "welfare2", "leftrigh")

# Flat, side-by-side, nested and mixed predictor sets. RAgeCat and
# eq_inc_quintiles have missing values, so these also exercise the NA filter.
predictor_sets <- list(
  none        = NULL,
  flat        = list("RSex"),
  flat_na     = list("eq_inc_quintiles"),
  side_by_side = list("RSex", "RAgeCat"),
  nested      = list(c("RSex", "RAgeCat")),
  mixed       = list("RClassGp", c("RSex", "eq_inc_quintiles"))
)

confs <- list(none = NULL, se = "se", ci = "ci")


# ---- A. matches the current engine -------------------------------------------

for (outcome in outcomes) {
  for (p_name in names(predictor_sets)) {
    for (c_name in names(confs)) {
      test_that(paste("A. matches unweighted_mean():", outcome, "/", p_name, "/ conf", c_name), {
        old <- unweighted_mean(bsa, outcome, predictors = predictor_sets[[p_name]],
                               conf = confs[[c_name]])
        new <- calc_stat_engine(bsa, outcome, predictors = predictor_sets[[p_name]],
                                stat = "mean", conf = confs[[c_name]])
        # The old engine leaves cross_break out when there are no predictors
        # (calc_stats() adds it later); the draft always includes it.
        if (is.null(predictor_sets[[p_name]])) {
          old <- old %>% mutate(cross_break = "Total", .before = 1)
        }
        # The old engine adds an estimate_se = "-" placeholder when conf is
        # NULL; the draft leaves the column out.
        if (is.null(confs[[c_name]])) {
          old <- old %>% select(-estimate_se)
        }
        # The old engine adds base_description; the draft leaves bases to
        # calc_stats() (add_base_descriptions()).
        old <- old %>% select(-base_description)
        # The draft adds table_order, cat_order and returns a tibble; the old engine doesn't.
        new <- new %>% select(-table_order, -cat_order) %>% as.data.frame()
        expect_equal(new, old)
      })
    }
  }
}

# (The old "with a base description" test is gone: the engine no longer
# handles bases.)


# ---- B. hand calculations ----------------------------------------------------

test_that("B. total row: mean, base, SE and CI match a hand calculation", {
  x <- na.omit(bsa$welfare2)
  se <- sd(x) / sqrt(length(x))

  result <- calc_stat_engine(bsa, "welfare2", stat = "mean", conf = "se")
  expect_equal(nrow(result), 1)
  expect_equal(result$estimate, mean(x))
  expect_equal(result$base, length(x))
  expect_equal(result$estimate_se, se)

  result_ci <- calc_stat_engine(bsa, "welfare2", stat = "mean", conf = "ci")
  expect_equal(result_ci$estimate_ci,
               paste0(mean(x) - 1.96 * se, " - ", mean(x) + 1.96 * se))
})

test_that("B. grouped rows match hand-calculated group means and bases", {
  expected <- bsa %>%
    filter(!is.na(leftrigh), !is.na(RAgeCat)) %>%
    group_by(RAgeCat) %>%
    summarise(mean = mean(leftrigh), n = n()) %>%
    mutate(RAgeCat = as.character(RAgeCat))

  result <- calc_stat_engine(bsa, "leftrigh", predictors = list("RAgeCat"), stat = "mean") %>%
    filter(cross_break == "RAgeCat") %>%
    inner_join(expected, by = c("p_cat1" = "RAgeCat"))

  expect_equal(nrow(result), nrow(expected))
  expect_equal(result$estimate, result$mean)
  expect_equal(result$base, result$n)
})

test_that("B. nested rows match hand-calculated cell means", {
  expected <- bsa %>%
    filter(!is.na(NatFrEst), !is.na(RSex), !is.na(RAgeCat)) %>%
    group_by(RSex, RAgeCat) %>%
    summarise(mean = mean(NatFrEst), n = n(), .groups = "drop") %>%
    mutate(across(c(RSex, RAgeCat), as.character))

  result <- calc_stat_engine(bsa, "NatFrEst", predictors = list(c("RSex", "RAgeCat")),
                             stat = "mean") %>%
    filter(cross_break == "RSex_X_RAgeCat") %>%
    inner_join(expected, by = c("p_cat1" = "RSex", "p_cat2" = "RAgeCat"))

  expect_equal(nrow(result), nrow(expected))
  expect_equal(result$estimate, result$mean)
  expect_equal(result$base, result$n)
})

test_that("B. missing predictor values are dropped from that break only", {
  result <- calc_stat_engine(bsa, "welfare2", predictors = list("eq_inc_quintiles"),
                             stat = "mean")
  total_base <- result %>% filter(cross_break == "Total") %>% pull(base)
  break_base <- result %>% filter(cross_break == "eq_inc_quintiles") %>% pull(base) %>% sum()

  expect_equal(total_base, sum(!is.na(bsa$welfare2)))
  expect_equal(break_base, sum(!is.na(bsa$welfare2) & !is.na(bsa$eq_inc_quintiles)))
  expect_lt(break_base, total_base)
})


# ---- C. shape and labelling --------------------------------------------------

test_that("C. labels and standard columns are filled in", {
  result <- calc_stat_engine(bsa, "NatFrEst", predictors = list(c("RSex", "RAgeCat")),
                             stat = "mean")
  nested <- result %>% filter(cross_break == "RSex_X_RAgeCat")

  expect_true(all(result$outcome == "NatFrEst"))
  expect_true(all(result$o_lab == attr(bsa$NatFrEst, "label")))
  expect_true(all(result$o_cat == "mean"))
  expect_true(all(result$stat == "mean"))
  expect_true(all(result$outcome_type == "numeric"))
  expect_true(all(nested$predictor1 == "RSex"))
  expect_true(all(nested$predictor2 == "RAgeCat"))
  expect_true(all(nested$p_lab1 == attr(bsa$RSex, "label")))
  expect_true(all(nested$p_lab2 == attr(bsa$RAgeCat, "label")))
})

test_that("C. Total row has 'Total' in every predictor column", {
  result <- calc_stat_engine(bsa, "leftrigh", predictors = list(c("RSex", "RAgeCat")),
                             stat = "mean")
  total <- result %>% filter(cross_break == "Total")
  expect_equal(nrow(total), 1)
  expect_true(all(unlist(total[, c("predictor1", "p_lab1", "p_cat1",
                                   "predictor2", "p_lab2", "p_cat2")]) == "Total"))
})

test_that("C. cross_break is always present, and 'Total' with no predictors", {
  result <- calc_stat_engine(bsa, "leftrigh", stat = "mean")
  expect_equal(names(result)[1], "cross_break")
  expect_equal(result$cross_break, "Total")
})

test_that("C. unweighted_n equals base for a numeric stat", {
  result <- calc_stat_engine(bsa, "leftrigh", predictors = list("RClassGp"), stat = "mean")
  expect_equal(result$unweighted_n, result$base)
})

test_that("C. conf controls which uncertainty columns come back", {
  none <- calc_stat_engine(bsa, "leftrigh", stat = "mean")
  se   <- calc_stat_engine(bsa, "leftrigh", stat = "mean", conf = "se")
  ci   <- calc_stat_engine(bsa, "leftrigh", stat = "mean", conf = "ci")

  expect_false(any(c("estimate_se", "estimate_low", "estimate_upp") %in% names(none)))
  expect_false(any(c("estimate_low", "estimate_upp") %in% names(se)))
  expect_true(all(c("estimate_low", "estimate_upp", "estimate_ci") %in% names(ci)))
  expect_false("estimate_se" %in% names(ci))
})


# ---- D. behaviour -------------------------------------------------------------

test_that("D. runs quietly - no messages or warnings now suppression is gone", {
  expect_silent(calc_stat_engine(bsa, "NatFrEst",
                                 predictors = list("RSex", c("RSex", "RAgeCat")),
                                 stat = "mean", conf = "ci"))
})

test_that("D. unknown statistic gives a clear error", {
  expect_error(calc_stat_engine(bsa, "leftrigh", stat = "mode"), "Unknown statistic: mode")
})
