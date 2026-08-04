source(here::here("Scripts_new", "calc_stats.R"))

data(api, package = "survey")
apistrat$meals3 <- factor(dplyr::ntile(apistrat$meals, 3), labels = c("Low", "Mid", "High"))
design <- apistrat %>% srvyr::as_survey_design(ids = 1, weights = pw)

weighted_result <- calc_stats(design, outcomes = "sch.wide", predictors = "meals3",
                               statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)

unweighted_result <- calc_stats(apistrat, outcomes = "sch.wide", predictors = "meals3",
                                 statistics = "perc", conf = "se", multicode = FALSE, pairwise = TRUE)

both_result <- calc_stats(design, outcomes = "sch.wide", predictors = "meals3",
                           statistics = c("perc", "w_perc"), conf = "se", multicode = FALSE, pairwise = TRUE)

print(weighted_result)
print(unweighted_result)
print(both_result)

# manual cross-check via svycontrast() directly, bypassing calc_stats() entirely
design_base <- design
class(design_base) <- setdiff(class(design_base), "tbl_svy")

props <- survey::svyby(~sch.wide, ~meals3, design = design_base, FUN = survey::svymean,
                        keep.var = TRUE, vartype = "se", covmat = TRUE)

check_contrast <- function(level_a, level_b) {
  contrast <- survey::svycontrast(props, setNames(c(1, -1),
                                    c(paste0(level_a, ":sch.wideYes"), paste0(level_b, ":sch.wideYes"))))
  est <- as.numeric(contrast)
  se  <- as.numeric(survey::SE(contrast))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)
  cat(level_a, "vs", level_b, "- est:", est, " SE:", se, " z:", z, " p:", p, "\n")
}

check_contrast("Low", "Mid")
check_contrast("Low", "High")


# =============================================================================
# bigger, more separated synthetic dataset - 500 per group, proportions
# 30% / 50% / 80%, so differences are large relative to the (small, given
# the sample size) SEs - should come out clearly significant throughout.
# =============================================================================

set.seed(1)
n <- 500
big_data <- tibble(
  response = factor(c(rbinom(n, 1, 0.3), rbinom(n, 1, 0.5), rbinom(n, 1, 0.8)),
                     levels = c(0, 1), labels = c("No", "Yes")),
  region   = factor(rep(c("Low", "Mid", "High"), each = n), levels = c("Low", "Mid", "High")),
  wt       = runif(3 * n, 0.5, 2)
)
big_design <- big_data %>% srvyr::as_survey_design(ids = 1, weights = wt)

big_weighted_result <- calc_stats(big_design, outcomes = "response", predictors = "region",
                                   statistics = "w_perc", conf = "se", multicode = FALSE, pairwise = TRUE)

big_unweighted_result <- calc_stats(big_data, outcomes = "response", predictors = "region",
                                     statistics = "perc", conf = "se", multicode = FALSE, pairwise = TRUE)

big_both_result <- calc_stats(big_design, outcomes = "response", predictors = "region",
                               statistics = c("perc", "w_perc"), conf = "se", multicode = FALSE, pairwise = TRUE)

print(big_weighted_result)
print(big_unweighted_result)
print(big_both_result)

big_design_base <- big_design
class(big_design_base) <- setdiff(class(big_design_base), "tbl_svy")

big_props <- survey::svyby(~response, ~region, design = big_design_base, FUN = survey::svymean,
                            keep.var = TRUE, vartype = "se", covmat = TRUE)

check_contrast_big <- function(level_a, level_b) {
  contrast <- survey::svycontrast(big_props, setNames(c(1, -1),
                                    c(paste0(level_a, ":responseYes"), paste0(level_b, ":responseYes"))))
  est <- as.numeric(contrast)
  se  <- as.numeric(survey::SE(contrast))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)
  cat(level_a, "vs", level_b, "- est:", est, " SE:", se, " z:", z, " p:", p, "\n")
}

check_contrast_big("Low", "Mid")
check_contrast_big("Low", "High")
check_contrast_big("Mid", "High")
