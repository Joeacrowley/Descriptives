# =============================================================================
# Test data: predictor levels that contain a colon (":").
#
# A made-up survey of 1,000 people: `answer` (Yes / No) by `region` and
# `sex`. `region`'s levels contain colons; `sex`'s don't.
#
#   region          sex      n     Yes   No
#   Area: North     Male     170   119    51
#   Area: North     Female   170   102    68
#   Area: South     Male     170    51   119
#   Area: South     Female   170    68   102
#   Area: East      Male     160    80    80
#   Area: East      Female   160    80    80
#
# Every group is large, so nothing fails for being too small.
# =============================================================================

library(tidyverse)


# ---- build the data ----------------------------------------------------------

# One row per person, made from the counts in the table above.
counts <- tribble(
  ~region,        ~sex,     ~answer, ~n,
  "Area: North",  "Male",   "Yes",   119,
  "Area: North",  "Male",   "No",     51,
  "Area: North",  "Female", "Yes",   102,
  "Area: North",  "Female", "No",     68,
  "Area: South",  "Male",   "Yes",    51,
  "Area: South",  "Male",   "No",    119,
  "Area: South",  "Female", "Yes",    68,
  "Area: South",  "Female", "No",    102,
  "Area: East",   "Male",   "Yes",    80,
  "Area: East",   "Male",   "No",     80,
  "Area: East",   "Female", "Yes",    80,
  "Area: East",   "Female", "No",     80
)

colon_data <- counts %>%
  uncount(n) %>%
  mutate(
    region = factor(region, levels = c("Area: North", "Area: South", "Area: East")),
    sex    = factor(sex,    levels = c("Male", "Female")),
    answer = factor(answer, levels = c("Yes", "No"))
  )

# Check: should match the table above.
colon_data %>% count(region, sex, answer)

colon_data

library(dtab)
make_table(data = colon_data, 
           outcomes = "answer", 
           predictors = list("sex", "region"), 
           pairwise = T,
           statistics = "perc"
           ) %>% 
  huxtable::as_flextable()

calc_stats(data = colon_data, 
           outcomes = "answer", 
           predictors = list("sex", "region"), 
           pairwise = T,
           statistics = "perc"
) 
