# =============================================================================
# check_svyby_mean_naming.R
#
# Diagnostic script, not a test file - run by hand, read the printed output.
# Same purpose check_svyby_nested_naming.R served for the nested-predictor
# coefficient-naming convention: confirm, from real output, exactly how
# svyby()/coef()/vcov() name things for a NUMERIC outcome, before writing
# weighted_mean_svyby()/tidy_svyby_mean() against an assumed convention
# rather than a confirmed one.
#
# STATUS after the first run of Sections 1-3 (mean confirmed clean; median
# confirmed NOT to work via this route - svyquantile doesn't return
# influence functions, and the package's own docs never show a covmat/
# svycontrast() example for it either - see the calc_stats.R conversation
# this script came out of). Scope has since narrowed to WEIGHTED means only:
# unweighted means never touch survey design machinery and have no
# clustering to worry about, so they keep using unweighted_mean()'s existing
# estimate_se as-is - pairwise_test_one_group()'s independent-SE path is
# already exact for that case, not an approximation, so nothing new is
# needed there at all. Section 4 (added after that first run) is the one
# genuinely new open question for the weighted-mean path specifically: NA
# handling in the outcome.
#
# Section 1: single predictor - mean, then median (median kept for the
#   record even though it's now out of scope - see STATUS above).
# Section 2: nested (2-predictor) - mean, then median.
# Section 3: cross-check both against the EXISTING weighted_mean()/
#   weighted_median() implementations already trusted in calc_stats.R - same
#   estimate, same SE (within floating-point tolerance)? Same sanity check
#   test_weighted_perc_svyby.R's tests 1-2 ran for weighted_perc() vs
#   weighted_perc_svyby() before either was trusted for real use.
# Section 4: NA handling in the outcome - weighted mean only.
# =============================================================================

source(here::here("Scripts_new", "calc_stats.R"))

data(api, package = "survey")
apistrat$meals3 <- factor(dplyr::ntile(apistrat$meals, 3), labels = c("Low", "Mid", "High"))
design <- apistrat %>% srvyr::as_survey_design(ids = 1, weights = pw)

design_base <- design
class(design_base) <- setdiff(class(design_base), "tbl_svy")

# Small helper so a genuine error in the median path prints clearly and lets
# the rest of the script keep going, rather than halting the whole run.
try_svyby <- function(label, expr) {
  tryCatch(expr, error = function(e) {
    cat("\n*** ", label, " ERRORED - see message below, everything past this ***\n")
    cat("*** point in this section will be skipped                        ***\n")
    cat(conditionMessage(e), "\n")
    NULL
  })
}


# =============================================================================
# SECTION 1: single predictor (~stype)
# =============================================================================
cat("\n=============================================================\n")
cat("SECTION 1a: single predictor, MEAN\n")
cat("=============================================================\n")

props_mean_flat <- try_svyby("svyby(FUN = svymean), flat", {
  survey::svyby(~api00, ~stype, design = design_base, FUN = survey::svymean,
                keep.var = TRUE, vartype = "se", covmat = TRUE)
})

if (!is.null(props_mean_flat)) {
  cat("\n--- class(props_mean_flat) ---\n"); print(class(props_mean_flat))
  cat("\n--- print(props_mean_flat) ---\n"); print(props_mean_flat)
  cat("\n--- names(coef(props_mean_flat)) ---\n"); print(names(coef(props_mean_flat)))
  cat("\n--- dimnames(vcov(props_mean_flat))[[1]] ---\n"); print(dimnames(vcov(props_mean_flat))[[1]])
  cat("\n--- SE(props_mean_flat) vs sqrt(diag(vcov(props_mean_flat))) - same? ---\n")
  cat("SE():\n"); print(survey::SE(props_mean_flat))
  cat("sqrt(diag(vcov())):\n"); print(sqrt(diag(vcov(props_mean_flat))))
}

cat("\n=============================================================\n")
cat("SECTION 1b: single predictor, MEDIAN\n")
cat("=============================================================\n")

props_median_flat <- try_svyby("svyby(FUN = svyquantile), flat", {
  survey::svyby(~api00, ~stype, design = design_base, FUN = survey::svyquantile,
                quantiles = 0.5, keep.var = TRUE, vartype = "se", covmat = TRUE)
})

if (!is.null(props_median_flat)) {
  cat("\n--- class(props_median_flat) ---\n"); print(class(props_median_flat))
  cat("\n--- str(props_median_flat) - shape matters here, print() alone may not show it ---\n")
  str(props_median_flat)
  cat("\n--- print(props_median_flat) ---\n"); print(props_median_flat)

  coef_ok <- try_svyby("coef(props_median_flat)", coef(props_median_flat))
  if (!is.null(coef_ok)) {
    cat("\n--- names(coef(props_median_flat)) ---\n"); print(names(coef_ok))
  }

  vcov_ok <- try_svyby("vcov(props_median_flat)", vcov(props_median_flat))
  if (!is.null(vcov_ok)) {
    cat("\n--- dimnames(vcov(props_median_flat))[[1]] ---\n"); print(dimnames(vcov_ok)[[1]])
    cat("\n--- sqrt(diag(vcov(props_median_flat))) ---\n"); print(sqrt(diag(vcov_ok)))
  }

  se_ok <- try_svyby("SE(props_median_flat)", survey::SE(props_median_flat))
  if (!is.null(se_ok)) {
    cat("\n--- SE(props_median_flat) ---\n"); print(se_ok)
  }
}


# =============================================================================
# SECTION 2: nested (2-predictor: ~meals3 + stype)
# =============================================================================
cat("\n=============================================================\n")
cat("SECTION 2a: nested (2-predictor), MEAN\n")
cat("=============================================================\n")

props_mean_nested <- try_svyby("svyby(FUN = svymean), nested", {
  survey::svyby(~api00, ~meals3 + stype, design = design_base, FUN = survey::svymean,
                keep.var = TRUE, vartype = "se", covmat = TRUE)
})

if (!is.null(props_mean_nested)) {
  cat("\n--- print(props_mean_nested) ---\n"); print(props_mean_nested)
  cat("\n--- names(coef(props_mean_nested)) ---\n"); print(names(coef(props_mean_nested)))
  cat("\n--- dimnames(vcov(props_mean_nested))[[1]] ---\n"); print(dimnames(vcov(props_mean_nested))[[1]])
}

cat("\n=============================================================\n")
cat("SECTION 2b: nested (2-predictor), MEDIAN\n")
cat("=============================================================\n")

props_median_nested <- try_svyby("svyby(FUN = svyquantile), nested", {
  survey::svyby(~api00, ~meals3 + stype, design = design_base, FUN = survey::svyquantile,
                quantiles = 0.5, keep.var = TRUE, vartype = "se", covmat = TRUE)
})

if (!is.null(props_median_nested)) {
  cat("\n--- print(props_median_nested) ---\n"); print(props_median_nested)

  coef_ok <- try_svyby("coef(props_median_nested)", coef(props_median_nested))
  if (!is.null(coef_ok)) {
    cat("\n--- names(coef(props_median_nested)) ---\n"); print(names(coef_ok))
  }

  vcov_ok <- try_svyby("vcov(props_median_nested)", vcov(props_median_nested))
  if (!is.null(vcov_ok)) {
    cat("\n--- dimnames(vcov(props_median_nested))[[1]] ---\n"); print(dimnames(vcov_ok)[[1]])
  }
}


# =============================================================================
# SECTION 3: cross-check against the existing weighted_mean()/weighted_median()
# =============================================================================
cat("\n=============================================================\n")
cat("SECTION 3a: cross-check MEAN\n")
cat("=============================================================\n")

existing_mean <- weighted_mean(design, outcomes = "api00", predictors = "stype", conf = "se") %>%
  dplyr::filter(p_cat1 != "Total")

cat("\n--- weighted_mean() output (existing implementation) ---\n")
print(existing_mean %>% dplyr::select(p_cat1, estimate, estimate_se))

if (!is.null(props_mean_flat)) {
  # GUESSES the naming convention is "level:api00" (bare level, no "stype"
  # prefix) - the same bare-level pattern already confirmed for a factor
  # outcome. If wrong, this join comes back all-NA rather than lining up -
  # not an error, just a sign to go re-read Section 1a's raw
  # names(coef(props_mean_flat)) output and fix this line before trusting
  # anything past it.
  svyby_mean_tidy <- tibble::tibble(
    p_cat1      = sub(":api00$", "", names(coef(props_mean_flat))),
    estimate    = as.numeric(coef(props_mean_flat)),
    estimate_se = sqrt(diag(vcov(props_mean_flat)))
  )
  cat("\n--- svyby()-based MEAN estimate/SE ---\n"); print(svyby_mean_tidy)

  cat("\n--- MEAN difference (should be ~0 / very small) ---\n")
  print(
    existing_mean %>% dplyr::select(p_cat1, estimate, estimate_se) %>%
      dplyr::left_join(svyby_mean_tidy, by = "p_cat1", suffix = c("_existing", "_svyby")) %>%
      dplyr::mutate(estimate_diff = estimate_existing - estimate_svyby,
                    se_diff       = as.numeric(estimate_se_existing) - estimate_se_svyby)
  )
}

cat("\n=============================================================\n")
cat("SECTION 3b: cross-check MEDIAN\n")
cat("=============================================================\n")

existing_median <- weighted_median(design, outcomes = "api00", predictors = "stype", conf = "se") %>%
  dplyr::filter(p_cat1 != "Total")

cat("\n--- weighted_median() output (existing implementation) ---\n")
print(existing_median %>% dplyr::select(p_cat1, estimate, estimate_se))

if (!is.null(props_median_flat) && !is.null(coef_ok) && !is.null(vcov_ok)) {
  # Same bare-level guess as the mean case above - equally unconfirmed here.
  svyby_median_tidy <- tibble::tibble(
    p_cat1      = sub(":api00$", "", names(coef(props_median_flat))),
    estimate    = as.numeric(coef(props_median_flat)),
    estimate_se = sqrt(diag(vcov(props_median_flat)))
  )
  cat("\n--- svyby()-based MEDIAN estimate/SE ---\n"); print(svyby_median_tidy)

  cat("\n--- MEDIAN difference (should be ~0 / very small) ---\n")
  print(
    existing_median %>% dplyr::select(p_cat1, estimate, estimate_se) %>%
      dplyr::left_join(svyby_median_tidy, by = "p_cat1", suffix = c("_existing", "_svyby")) %>%
      dplyr::mutate(estimate_diff = estimate_existing - estimate_svyby,
                    se_diff       = as.numeric(estimate_se_existing) - estimate_se_svyby)
  )
} else {
  cat("\nSkipped - Section 1b's svyquantile/coef()/vcov() path didn't come back clean.\n")
  cat("Whatever error printed up there is itself the useful answer: it tells us\n")
  cat("medians need a different mechanism (no clean joint covariance available\n")
  cat("the way means have), not just a different formula/FUN into the same one.\n")
}


# =============================================================================
# SECTION 4: NA handling in the outcome - WEIGHTED MEAN ONLY
#
# Scope narrowed since Section 1b/2b/3b above: unweighted means don't go
# through survey() at all - no clustering to worry about (never has been),
# so weighted_mean_svyby()/tidy_svyby_mean() is the only new function this
# NA question is actually relevant to. unweighted_mean() keeps working
# exactly as it already does, and picks up pairwise support for free (its
# existing estimate_se already flows through pairwise_test_one_group()'s
# independent-SE path, which is exact - not approximate - for unclustered
# data, since there's no covariance between groups to miss).
#
# Real project data will have missing numeric values sometimes.
# weighted_mean()'s existing implementation - srvyr::survey_mean() - already
# has SOME answer for this today (whatever it is); the question is whether
# svyby()/svymean() gives the SAME answer, and whether it needs na.rm passed
# explicitly to behave sensibly at all (survey::svymean()'s default is
# na.rm = FALSE, which could mean an error, a silent NA propagation across
# the whole group, or something else - not assumed here, checked directly
# both without and with na.rm = TRUE).
# =============================================================================
cat("\n=============================================================\n")
cat("SECTION 4: NA handling in the outcome (weighted MEAN only)\n")
cat("=============================================================\n")

apistrat_na <- apistrat
set.seed(42)
na_rows <- sample(seq_len(nrow(apistrat_na)), 10)
apistrat_na$api00[na_rows] <- NA
design_na <- apistrat_na %>% srvyr::as_survey_design(ids = 1, weights = pw)
design_na_base <- design_na
class(design_na_base) <- setdiff(class(design_na_base), "tbl_svy")

cat("\n--- how many NAs introduced, and in which stype groups? ---\n")
print(table(apistrat_na$stype, is.na(apistrat_na$api00)))

cat("\n--- svyby(FUN = svymean) on the NA-containing design, NO na.rm passed ---\n")
props_mean_na <- try_svyby("svyby(FUN = svymean), NAs present, no na.rm", {
  survey::svyby(~api00, ~stype, design = design_na_base, FUN = survey::svymean,
                keep.var = TRUE, vartype = "se", covmat = TRUE)
})
if (!is.null(props_mean_na)) {
  cat("\n--- print(props_mean_na) - did this silently work, error, or propagate NA? ---\n")
  print(props_mean_na)
}

cat("\n--- svyby(FUN = svymean) on the same design, na.rm = TRUE passed through to FUN ---\n")
props_mean_na_rm <- try_svyby("svyby(FUN = svymean), NAs present, na.rm = TRUE", {
  survey::svyby(~api00, ~stype, design = design_na_base, FUN = survey::svymean,
                na.rm = TRUE, keep.var = TRUE, vartype = "se", covmat = TRUE)
})
if (!is.null(props_mean_na_rm)) {
  cat("\n--- print(props_mean_na_rm) ---\n")
  print(props_mean_na_rm)
}

cat("\n--- weighted_mean() (EXISTING implementation) on the same NA-containing ---\n")
cat("--- design - what does it already do today, for comparison? ---\n")
existing_mean_na <- weighted_mean(design_na, outcomes = "api00", predictors = "stype", conf = "se") %>%
  dplyr::filter(p_cat1 != "Total")
print(existing_mean_na %>% dplyr::select(p_cat1, estimate, estimate_se))


cat("\n=============================================================\n")
cat("Done. Things to look for in the output above:\n")
cat("MEAN (Sections 1a/2a/3a) - expected to be the straightforward half:\n")
cat("1. names(coef(props_mean_flat)) - \"stypeE:api00\" / bare-level-style /\n")
cat("   something else?\n")
cat("2. Does SE(props_mean_flat) match sqrt(diag(vcov(props_mean_flat))), or\n")
cat("   is it the differently-shaped object the factor case had (meaning\n")
cat("   vcov() has to be used directly, not SE())?\n")
cat("3. Nested case (2a) - same \"level1.level2:api00\" dot-join convention\n")
cat("   already confirmed for a factor outcome?\n")
cat("4. Section 3a - do the svyby()-based numbers match weighted_mean()'s\n")
cat("   existing numbers closely?\n")
cat("\n")
cat("MEDIAN (Sections 1b/2b/3b) - genuinely open question, not assumed:\n")
cat("5. Did svyby(FUN = svyquantile, covmat = TRUE) even run without\n")
cat("   erroring? If it errored, that error message IS the answer - it means\n")
cat("   medians need their own design (maybe the approximate/independent-SE\n")
cat("   path after all, just for medians specifically, or a different\n")
cat("   variance method entirely), not the same recipe as means.\n")
cat("6. If it ran - does coef()/vcov() give a usable flat vector/matrix the\n")
cat("   same shape as the mean case, or something structurally different\n")
cat("   (str() output above should show this directly)?\n")
cat("7. If Section 3b ran - do the numbers match weighted_median()'s\n")
cat("   existing numbers?\n")
cat("\n")
cat("NA HANDLING (Section 4) - weighted mean only, the only path going\n")
cat("through survey() now:\n")
cat("8. Did svyby(FUN = svymean) with NO na.rm passed error, silently drop\n")
cat("   the NA rows correctly, or propagate NA across a whole group's\n")
cat("   estimate? Does passing na.rm = TRUE change that? And does whichever\n")
cat("   one behaves correctly match what weighted_mean() already does today\n")
cat("   on the same NA-containing data (Section 4's last print)?\n")
cat("=============================================================\n")
