# =============================================================================
# Test data for pw_test_cats() (dev/draft_pairwise.R).
#
# A small made-up survey: `answer` (Yes / No / Unsure) by `region`, with
# counts set by hand so each case below passes or fails for a known reason.
#
#   region   n    Yes  No   Unsure
#   North    40   20   15   5
#   South    40   10   25   5
#   East     40   30    8   2      <- only 2 Unsure: Unsure pairs with East fail
#   West      6    3    3   0      <- only 6 people: every pair with West fails
#
# safe_ttest() needs each group to have 10+ people, and 3+ people both in
# and out of the outcome category.
# =============================================================================

suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
source(here::here("dtab", "dev", "draft_pairwise.R"))
library(srvyr)


# ---- build the data ----------------------------------------------------------

# One row per person, made from the counts in the table above.
counts <- tribble(
  ~region, ~answer,  ~n,
  "North", "Yes",    20,
  "North", "No",     15,
  "North", "Unsure",  5,
  "South", "Yes",    10,
  "South", "No",     25,
  "South", "Unsure",  5,
  "East",  "Yes",    30,
  "East",  "No",      8,
  "East",  "Unsure",  2,
  "West",  "Yes",     3,
  "West",  "No",      3
)

survey <- counts %>%
  uncount(n) %>%
  mutate(
    region = factor(region, levels = c("North", "South", "East", "West")),
    answer = factor(answer, levels = c("Yes", "No", "Unsure")),
    # Alternate men and women, for the nested case.
    sex    = factor(rep(c("Men", "Women"), length.out = n())),
    # Weights for the survey design case.
    wt     = rep(c(0.8, 1.2), length.out = n())
  )

# Check the counts match the table.
count(survey, region, answer) %>% pivot_wider(names_from = answer, values_from = n)

# Without West: the data where most tests can run.
no_west <- survey %>% filter(region != "West")


# ---- 1. everything testable ---------------------------------------------------
# "Yes" has plenty of cases in every group. Expected: 3 rows (N-S, N-E, S-E).

pw_test_cats(outcome = "answer", outcome_level = "Yes", predictor = "region", data = no_west)


# ---- 2. one pair untestable: a small cell -------------------------------------
# East has only 2 Unsure. Expected: North-South tested; the two East pairs
# have an NA p-value and the reason in `method`.

try(pw_test_cats(outcome = "answer", outcome_level = "Unsure", predictor = "region", data = no_west))


# ---- 3. one pair untestable: a small group ------------------------------------
# West has only 6 people. Expected: North-South, North-East, South-East
# tested; the three West pairs have an NA p-value and "n=6 (< 10)".

try(pw_test_cats(outcome = "answer", outcome_level = "Yes", predictor = "region", data = survey))


# ---- 4. missing answers ---------------------------------------------------------
# A few people didn't answer. Expected: all 3 pairs tested, on those who
# answered.

with_missing <- no_west %>% mutate(answer = replace(answer, c(1, 2, 41, 81), NA))
try(pw_test_cats(outcome = "answer", outcome_level = "Yes", predictor = "region", data = with_missing))


# ---- 5. nested predictor ---------------------------------------------------------
# region within sex: 6 groups of 20 each. Expected: 15 rows for "Yes".

pw_test_cats(outcome = "answer", outcome_level = "Yes", predictor = c("sex", "region"), data = no_west)


# ---- 6. survey design ------------------------------------------------------------
# Same as case 1, weighted (svyttest instead of t.test). Expected: 3 rows.

no_west_design <- no_west %>% as_survey_design(weights = wt)
pw_test_cats(outcome = "answer", outcome_level = "Yes", predictor = "region", data = no_west_design)

# Same as case 2, weighted. Expected: as case 2.
try(pw_test_cats(outcome = "answer", outcome_level = "Unsure", predictor = "region", data = no_west_design))
