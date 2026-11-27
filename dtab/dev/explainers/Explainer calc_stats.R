# Explainer: calc_stats()
# NOTE: this is AI written, sorry.
# The function users call: checks the inputs, then runs calc_stat_engine() for every
# outcome x statistic that fits, and stacks the results.
# p-values, pairwise tests, multicodes and bases get their own explainers.
# Paths use here::here(), so it runs from anywhere in the Descriptives project.

suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
source(here::here("dtab", "dev", "calc_stats_draft.R"))

bsa <- readRDS(here::here("data", "synthetic_bsa2019.rds"))
svy <- srvyr::as_survey_design(bsa, weights = WtFactor)


# ---- Many outcomes and stats in one call ----
# Each stat only runs on outcomes it fits: perc on factors, mean on numerics.
calc_stats(bsa, outcomes = c("MuchPov", "leftrigh"), predictors = "RSex",
           statistics = c("perc", "mean")) %>%
  count(outcome, stat)


# Weighted stats need a survey design; on a plain data frame they're skipped.
calc_stats(bsa, outcomes = "leftrigh", statistics = c("mean", "w_mean")) %>%
  count(stat)
calc_stats(svy, outcomes = "leftrigh", statistics = c("mean", "w_mean")) %>%
  count(stat)


# ---- The forms predictors can take ----
# NULL: Total only.
calc_stats(bsa, "leftrigh", predictors = NULL, statistics = "mean") %>%
  count(cross_break)

# A character vector is treated as one set - so c("RSex", "RAgeCat") is nested.
calc_stats(bsa, "leftrigh", predictors = c("RSex", "RAgeCat"), statistics = "mean") %>%
  count(cross_break)

# A list gives side-by-side sets, here one flat and one nested.
calc_stats(bsa, "leftrigh", predictors = list("RSex", c("RSex", "RAgeCat")),
           statistics = "mean") %>%
  count(cross_break)


# ---- Outcome types ----
# Characters and logicals become factors (alphabetical order, labels kept).
bsa_types <- bsa %>%
  mutate(sex_text = as.character(RSex),
         is_male  = RSex == "Male",
         today    = Sys.Date())

calc_stats(bsa_types, c("sex_text", "is_male"), statistics = "perc") %>%
  select(outcome, o_cat, estimate)

# Dates (or anything else not numeric/factor) stop.
try(calc_stats(bsa_types, "today", statistics = "perc"))


# ---- drop_empty_levels ----
# MuchPov has categories nobody picked ("Don`t know", "Refusal"...).
table(bsa$MuchPov)

calc_stats(bsa, "MuchPov", statistics = "perc", drop_empty_levels = FALSE) %>%
  select(o_cat, estimate)   # kept as 0 rows - a 0 can be a real finding
calc_stats(bsa, "MuchPov", statistics = "perc", drop_empty_levels = TRUE) %>%
  select(o_cat, estimate)   # left out


# ---- Validation ----
# Bad inputs stop early with a plain message.
try(calc_stats(bsa, "leftrigh", statistics = "mode"))                 # unknown stat
try(calc_stats(bsa, "leftrigh", statistics = "mean", conf = "sd"))    # conf must be se/ci
try(calc_stats(bsa, "leftrigh", predictors = "RAgeCat_typo"))         # missing variable
try(calc_stats(bsa, "leftrigh", predictors = bsa["RSex"]))            # data frame, not names

# Some known, some unknown: a warning, and the known ones still run.
calc_stats(bsa, "leftrigh", statistics = c("mean", "mode")) %>% count(stat)


# ---- The extras ----
# Each adds columns; see their own explainers.
calc_stats(bsa, "MuchPov", "RSex", "perc", pval = TRUE) %>%
  select(cross_break, p_cat1, o_cat, estimate, p_method, p_value)

calc_stats(bsa, "MuchPov", "RAgeCat", "perc", pairwise = TRUE) %>%
  select(p_cat1, o_cat, estimate, sig_letter, sig_diff)
