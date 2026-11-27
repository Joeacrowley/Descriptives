# Explainer: convert_multicodes() (draft_multicodes_outline.R)
# Runs the helpers and steps on real calc_stats() output. Each step also has
# a tiny reprex of its own inside the function - use those to test a step
# in isolation; use this to see it on real data.
# Paths use here::here(), so it runs from anywhere in the Descriptives project.

suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
source(here::here("dtab", "dev", "calc_stats_draft.R"))   # also sources draft_multicodes_outline.R

load(here::here("data", "synthetic_multicode_data.rds"))  # gives `survey_data`


# ---- The input ----------------------------------------------------------------
# Q1 is a multicode: one Yes/No column per option, labelled "Q1: <option>".
survey_data %>% select(starts_with("Q1")) %>% head()
map_chr(survey_data %>% select(starts_with("Q1")), ~ attr(.x, "label"))

# Multicodes are declared: a list with one character vector per question.
q1        <- paste0("Q1_Option", LETTERS[1:5])
multicode <- list(q1)

# calc_stats() without multicode: each option is a separate outcome.
raw <- calc_stats(survey_data, c("Satisfaction", q1), "Sex", "perc")
raw %>% select(cross_break, p_cat1, outcome, o_lab, o_cat, estimate, base, table_order)


# ---- The helpers, on real labels ------------------------------------------------
labels <- unique(raw$o_lab)
labels
question_part(labels)          # "Q1" for every option; Satisfaction's label comes back whole
option_part(labels)            # the option text; NA for Satisfaction

raw %>%
  distinct(outcome, o_lab, o_cat) %>%
  group_by(outcome) %>%
  summarise(option = looks_like_option(o_lab, o_cat))
# The Q1 options are TRUE; Satisfaction (5 categories, no ": ") is FALSE.


# ---- The steps, on real data ------------------------------------------------------
# Set up what the function starts with...
data <- raw %>% select(-any_of("multicode_group"))
keep <- c("Mentioned", "mentioned", "Yes", "yes", "1")

# ...then, in draft_multicodes_outline.R, run each step's lines inside
# convert_multicodes() in order (skip the commented reprexes). After each
# step, look at what it made:
#
#   Step 1:  questions                         the 5 Q1 options, all question 1
#   Step 2:  option_check                      all TRUE - nothing warned about
#   Step 3:  base_check                        1 TRUE - bases match in every block
#   Step 4:  questions                         multicode_group "multicode1" added
#   Step 5:  folded                            15 "Yes" rows: 5 options x 3 blocks
#   Step 6:  folded %>% select(p_cat1, outcome, table_order, cat_order)
#                                              Male A-E, Female A-E (1-10), Total A-E (1-5)
#                                              - each table numbered on its own
#   Step 7:  data                              Satisfaction, then folded Q1


# ---- The function itself ------------------------------------------------------------
convert_multicodes(raw, multicode) %>%
  select(cross_break, p_cat1, outcome, multicode_group, outcome_type, o_lab, o_cat,
         estimate, table_order, cat_order)
# (row_order is stale here, as raw already had one - calc_stats() renumbers
#  it after this step, so its own output is fine.)

# Through calc_stats(), with perc and count. The options needn't be in
# outcomes too - they're added at the end if not.
calc_stats(survey_data, "Satisfaction", "Sex", c("perc", "count"), multicode = multicode) %>%
  count(multicode_group, outcome_type, stat)

# Only multicodes: outcomes can be left out.
calc_stats(survey_data, predictors = "Sex", statistics = "perc", multicode = multicode) %>%
  distinct(outcome, multicode_group)


# ---- Edge cases --------------------------------------------------------------------

# Two questions with the same text before ": " stay separate.
q2       <- paste0("Q2_Choice", LETTERS[1:5])
same_lab <- survey_data
for (v in q2) attr(same_lab[[v]], "label") <- str_replace(attr(same_lab[[v]], "label"), "^[^:]*", "Q1")
calc_stats(same_lab, q1, NULL, "perc", multicode = list(q1, q2)) %>%
  distinct(outcome, multicode_group, o_lab)

# Options with different bases: warned, and left unfolded.
uneven <- survey_data
uneven$Q1_OptionC[1:5] <- NA
calc_stats(uneven, predictors = "Sex", statistics = "perc", multicode = multicode) %>%
  distinct(outcome, multicode_group, outcome_type)

# An option that can't fold (3 categories): warned, whole question unfolded.
three_cats <- survey_data
three_cats$Q1_OptionE <- structure(
  factor(c("Yes", "No", "Don't know")[(seq_len(nrow(survey_data)) %% 3) + 1]),
  label = attr(survey_data$Q1_OptionE, "label"))
calc_stats(three_cats, predictors = NULL, statistics = "perc", multicode = multicode) %>%
  distinct(outcome, multicode_group, outcome_type)

# No multicode: nothing folded, multicode_group is the variable name.
calc_stats(survey_data, "Satisfaction", "Sex", "perc") %>% distinct(outcome, multicode_group)
