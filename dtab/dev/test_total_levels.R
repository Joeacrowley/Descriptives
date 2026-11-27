suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
library(tidyverse)

counts <- tribble(
  ~region,  ~sex,     ~answer, ~n,
  "North",  "Male",   "Yes",   119,
  "North",  "Male",   "No",     51,
  "North",  "Female", "Yes",   102,
  "North",  "Female", "No",     68,
  "South",  "Male",   "Yes",    51,
  "South",  "Male",   "No",    119,
  "South",  "Female", "Yes",    68,
  "South",  "Female", "No",    102,
  "Total",  "Male",   "Yes",    80,
  "Total",  "Male",   "No",     80,
  "Total",  "Female", "Yes",    80,
  "Total",  "Female", "No",     80
)

total_data <- counts %>%
  uncount(n) %>%
  mutate(
    region = factor(region, levels = c("North", "South", "Total")),
    sex    = factor(sex,    levels = c("Male", "Female")),
    answer = factor(answer, levels = c("Yes", "No"))
  )

total_data %>% count(sex, region, answer)

pw_test_cats(outcome = "answer", outcome_level = "Yes",
             predictor = c("sex", "region"), data = total_data)
