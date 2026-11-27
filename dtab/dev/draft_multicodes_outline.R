# =============================================================================
# DRAFT: multicode handling - helpers, then convert_multicodes() step by step.
#
# A multicode ("tick all that apply") arrives from calc_stats() as one yes/no
# outcome per option, each labelled "<question>: <option>", e.g.
#   Q1_OptionA  "Q1: Selected Option A"   Yes / No
#   Q1_OptionB  "Q1: Selected Option B"   Yes / No
# The job: show them as one question, with one row per option.
# Which outcomes are multicodes is declared by the user, e.g.
#   calc_stats(..., multicode = list(c("Q1_OptionA", "Q1_OptionB")))
# =============================================================================


# ---- Helpers needed ------------------------------------------------------------

# 1. question_part(label)
#    "Q1: Selected Option A" -> "Q1"
#    The text before ": ". Same for every option of a question.

question_part <- function(label) {
  # Split each label at the first ": " and keep the first piece.
  # A label with no ": " comes back whole.
  str_split_i(label, fixed(": "), 1)
}

# Reprex:
# library(stringr)
# labels <- c("Q1: Selected Option A",
#             "Q1: Selected Option B",
#             "Q2: Brand X: Liked",      # two ": " - only the first counts
#             "Satisfaction")            # no ": "
# question_part(labels)
# # "Q1" "Q1" "Q2" "Satisfaction"

# 2. option_part(label)
#    "Q1: Selected Option A" -> "Selected Option A"
#    The text after ": ". Becomes the row's category (o_cat).

option_part <- function(label) {
  # Find where the first ": " ends, and keep everything after it.
  # A label with no ": " has no option, so gives NA.
  colon_end <- str_locate(label, fixed(": "))[, "end"]
  str_sub(label, colon_end + 1)
}

# Reprex:
# library(stringr)
# labels <- c("Q1: Selected Option A",
#             "Q1: Selected Option B",
#             "Q2: Brand X: Liked",      # two ": " - everything after the first
#             "Satisfaction")            # no ": "
# option_part(labels)
# # "Selected Option A" "Selected Option B" "Brand X: Liked" NA

# 3. looks_like_option(outcome's rows)
#    TRUE if an outcome could be one option of a multicode:
#      - its label contains ": "
#      - it has 2 categories
#      - exactly one of them is a "yes" value (Yes / Mentioned / 1 ...)

looks_like_option <- function(o_lab, o_cat,
                              keep = c("Mentioned", "mentioned", "Yes", "yes", "1")) {
  # Takes one outcome's categories (one row per category).
  has_colon    <- all(str_detect(o_lab, fixed(": ")))
  n_categories <- length(o_cat)
  n_yes        <- sum(o_cat %in% keep)

  has_colon && n_categories <= 2 && n_yes == 1
}

# Reprex: four outcomes, all in the same block (Sex / Total / perc).
# library(tidyverse)
# block <- tribble(
#   ~outcome,       ~o_lab,                  ~o_cat,
#   "Q1_OptionA",   "Q1: Selected Option A", "Yes",
#   "Q1_OptionA",   "Q1: Selected Option A", "No",
#   "Q1_OptionB",   "Q1: Selected Option B", "Yes",      # only one level - still fine
#   "Satisfaction", "Satisfaction",          "Yes",      # no ": "
#   "Satisfaction", "Satisfaction",          "No",
#   "Q3_Rating",    "Q3: Rating",            "Good",     # 3 categories, no yes
#   "Q3_Rating",    "Q3: Rating",            "Okay",
#   "Q3_Rating",    "Q3: Rating",            "Poor"
# )
# block %>%
#   group_by(outcome) %>%
#   summarise(option = looks_like_option(o_lab, o_cat))
# # Q1_OptionA TRUE, Q1_OptionB TRUE, Q3_Rating FALSE, Satisfaction FALSE


# ---- Steps, in order -------------------------------------------------------------

# To test a step: run the helpers above, uncomment and run the step's
# reprex, then run the step's lines. Each reprex makes the objects the step
# needs, so steps can be tested in any order.

convert_multicodes <- function(data, multicode,
                               keep = c("Mentioned", "mentioned", "Yes", "yes", "1")) {

  # Any existing multicode_group is rebuilt in Step 7.
  data <- data %>% select(-any_of("multicode_group"))

  # Stats with one row per category (perc, count...) - only these fold.
  categorical_stats <- names(keep(stat_registry, ~ .x$type == "categorical"))

  # Step 1: List each question's options.
  #   One row per option, with the question it belongs to: its position in
  #   the multicode list. Questions are never grouped by label text, so two
  #   questions with the same "<question>: " stay separate.

  # Reprex: run this, then the Step 1 lines below.
  # library(tidyverse)
  # multicode <- list(c("Q2_OptionA", "Q2_OptionB"),     # question 1 in the list...
  #                   c("Q1_OptionA", "Q1_OptionB"))     # ...question 2
  # data <- tibble(outcome = c("Q1_OptionA", "Q1_OptionA", "Q1_OptionB", "Q1_OptionB",
  #                            "Satisfaction", "Q2_OptionA", "Q2_OptionB"))

  questions <- map_df(seq_along(multicode), function(i) {
    tibble(outcome = multicode[[i]], question = i)
  }) %>%
    # Put the options in data order (the row each first appears on), so
    # options and questions are numbered as they appear in the output.
    mutate(first_row = match(outcome, data$outcome)) %>%
    arrange(first_row) %>%
    select(-first_row)

  # Check:
  # questions
  # # Q1_OptionA  2
  # # Q1_OptionB  2
  # # Q2_OptionA  1
  # # Q2_OptionB  1

  # Step 2: Check each option can be folded.
  #   Every option must pass looks_like_option(): labelled "<question>: <option>",
  #   2 or fewer categories, exactly one of them a yes value. If any option
  #   fails, warn and leave its whole question unfolded.

  # Reprex: run the helpers above, then this, then the Step 2 lines below.
  # library(tidyverse)
  # categorical_stats <- c("perc", "count", "w_perc", "w_count")   # made at the top of the function
  # keep <- c("Mentioned", "mentioned", "Yes", "yes", "1")
  # questions <- tribble(                          # as if from Step 1
  #   ~outcome,     ~question,
  #   "Q1_OptionA", 1,
  #   "Q1_OptionB", 1,
  #   "Q2_OptionA", 2,
  #   "Q2_OptionB", 2
  # )
  # data <- tribble(
  #   ~stat,  ~outcome,     ~o_lab,                  ~o_cat,
  #   "perc", "Q1_OptionA", "Q1: Selected Option A", "Yes",
  #   "perc", "Q1_OptionA", "Q1: Selected Option A", "No",
  #   "perc", "Q1_OptionB", "Q1: Selected Option B", "Yes",
  #   "perc", "Q1_OptionB", "Q1: Selected Option B", "No",
  #   "perc", "Q2_OptionA", "Q2: Selected Option A", "Yes",
  #   "perc", "Q2_OptionA", "Q2: Selected Option A", "No",
  #   "perc", "Q2_OptionB", "Q2: Selected Option B", "Yes",
  #   "perc", "Q2_OptionB", "Q2: Selected Option B", "No",
  #   "perc", "Q2_OptionB", "Q2: Selected Option B", "Don't know"   # 3 categories
  # )

  # Every block has the same categories for an outcome (one_table() completes
  # them), so check each option once, on one copy of its categories.
  option_check <- data %>%
    # Only categorical stats can fold - not e.g. the mean of a 0/1 variable.
    filter(stat %in% categorical_stats,
           outcome %in% questions$outcome) %>%
    distinct(outcome, o_lab, o_cat) %>%            # one row per category
    group_by(outcome) %>%
    summarise(option = looks_like_option(o_lab, o_cat, keep))

  failed <- option_check %>% filter(!option) %>% pull(outcome)

  if (length(failed) > 0) {
    warning("Not treated as multicoded, as these options need a \"<question>: <option>\" ",
            "label and 2 or fewer categories, one of them a yes value: ",
            paste(failed, collapse = ", "), ". Their questions are left as separate outcomes.",
            call. = FALSE)

    failed_questions <- questions %>% filter(outcome %in% failed) %>% pull(question)
    questions <- questions %>% filter(!question %in% failed_questions)
  }

  # Check:
  # option_check
  # # Q1_OptionA TRUE, Q1_OptionB TRUE, Q2_OptionA TRUE, Q2_OptionB FALSE
  # questions
  # # Q1_OptionA  1
  # # Q1_OptionB  1
  # (plus a warning naming Q2_OptionB)

  # Step 3: Check each question is safe to fold.
  #   - Within each group (e.g. among males), all its options must have the
  #     same base. Groups needn't match each other.
  #   - If that fails in any group, warn and leave the question unfolded in
  #     every table.

  # Reprex: run this, then the Step 3 lines below.
  # (Only the columns Step 3 uses; "No" rows left out to keep it short.)
  # library(tidyverse)
  # categorical_stats <- c("perc", "count", "w_perc", "w_count")   # made at the top of the function
  # questions <- tribble(                          # as if from Step 2
  #   ~outcome,     ~question,
  #   "Q1_OptionA", 1,
  #   "Q1_OptionB", 1,
  #   "Q2_OptionA", 2,
  #   "Q2_OptionB", 2
  # )
  # data <- tribble(
  #   ~cross_break, ~p_cat1, ~stat,  ~outcome,     ~base,
  #   "Sex",        "Total", "perc", "Q1_OptionA", 200,
  #   "Sex",        "Total", "perc", "Q1_OptionB", 200,
  #   "Sex",        "Male",  "perc", "Q1_OptionA", 100,
  #   "Sex",        "Male",  "perc", "Q1_OptionB", 100,
  #   "Sex",        "Total", "perc", "Q2_OptionA", 200,
  #   "Sex",        "Total", "perc", "Q2_OptionB", 200,
  #   "Sex",        "Male",  "perc", "Q2_OptionA", 100,
  #   "Sex",        "Male",  "perc", "Q2_OptionB",  98   # differs in Male only
  # )

  # Block columns: e.g. "cross_break" "p_cat1" "stat"
  block_cols <- c("cross_break", data %>% select(starts_with("p_cat")) %>% names(), "stat")

  # For each question, in each block: how many different bases do its options have?
  base_check <- data %>%
    filter(stat %in% categorical_stats) %>%
    inner_join(questions, by = "outcome") %>%
    group_by(across(all_of(c(block_cols, "question")))) %>%
    summarise(n_bases = n_distinct(base), .groups = "drop") %>%

    # Keep variable only if there's one base (same case count) in every block.
    group_by(question) %>%
    summarise(same_bases = all(n_bases == 1))

  # Say which questions won't be folded, and drop them.
  unequal <- base_check %>% filter(!same_bases) %>% pull(question)

  if (length(unequal) > 0) {
    unequal_options <- questions %>% filter(question %in% unequal) %>% pull(outcome)
    warning("Not treated as multicoded, as the options' bases differ: ",
            paste(unequal_options, collapse = ", "), ".", call. = FALSE)
  }

  questions <- questions %>% filter(!question %in% unequal)

  # Check:
  # base_check
  # # 1 TRUE, 2 FALSE
  # questions
  # # Q1_OptionA  1
  # # Q1_OptionB  1
  # (plus a warning naming Q2_OptionA, Q2_OptionB)

  # Nothing to fold: every outcome is its own group.
  if (nrow(questions) == 0) {
    return(data %>% mutate(multicode_group = outcome, .after = outcome))
  }

  # Step 4: Name each question's group.
  #   multicode_group = "multicode1", "multicode2"... in the order the
  #   questions first appear. Every other outcome: its own variable name.

  # Reprex: run this, then the Step 4 lines below.
  # library(tidyverse)
  # questions <- tribble(                          # as if from Step 3
  #   ~outcome,     ~question,
  #   "Q3_OptionA", 2,                             # 2nd in the list, but first in the data
  #   "Q3_OptionB", 2,
  #   "Q1_OptionA", 1,
  #   "Q1_OptionB", 1
  # )

  # questions is already in data order (Step 1 put it so), so number the
  # questions in the order they're listed.
  group_names <- tibble(question = unique(questions$question)) %>%
    mutate(multicode_group = paste0("multicode", row_number()))

  questions <- questions %>% left_join(group_names, by = "question")

  # Every other outcome gets its own name as multicode_group in Step 7,
  # when the rows are put back together.

  # Check:
  # questions
  # # Q3_OptionA  2  multicode1
  # # Q3_OptionB  2  multicode1
  # # Q1_OptionA  1  multicode2
  # # Q1_OptionB  1  multicode2

  # Step 5: Fold.
  #   For each option, keep only its "yes" row (the % or count who picked it).
  #   On that row:
  #     o_lab        -> question_part()     e.g. "Q1"
  #     o_cat        -> option_part()       e.g. "Selected Option A"
  #     outcome_type -> "multicoded"
  #     outcome      -> unchanged           e.g. "Q1_OptionA"
  #     o_base       -> the first option's base description

  # Reprex: run the helpers above, then this, then the Step 5 lines below.
  # library(tidyverse)
  # categorical_stats <- c("perc", "count", "w_perc", "w_count")   # made at the top of the function
  # keep <- c("Mentioned", "mentioned", "Yes", "yes", "1")
  # questions <- tribble(                          # as if from Step 4
  #   ~outcome,     ~question, ~multicode_group,
  #   "Q1_OptionA", 1,         "multicode1",
  #   "Q1_OptionB", 1,         "multicode1"
  # )
  # data <- tribble(
  #   ~cross_break, ~p_cat1, ~stat,  ~outcome,       ~outcome_type, ~o_lab,                  ~o_cat, ~estimate, ~o_base,
  #   "Sex",        "Total", "perc", "Q1_OptionA",   "categorical", "Q1: Selected Option A", "Yes",  0.40,      "All adults",
  #   "Sex",        "Total", "perc", "Q1_OptionA",   "categorical", "Q1: Selected Option A", "No",   0.60,      "All adults",
  #   "Sex",        "Male",  "perc", "Q1_OptionA",   "categorical", "Q1: Selected Option A", "Yes",  0.35,      "All adults",
  #   "Sex",        "Male",  "perc", "Q1_OptionA",   "categorical", "Q1: Selected Option A", "No",   0.65,      "All adults",
  #   "Sex",        "Total", "perc", "Q1_OptionB",   "categorical", "Q1: Selected Option B", "Yes",  0.25,      "Adults asked Q1",  # differs
  #   "Sex",        "Total", "perc", "Q1_OptionB",   "categorical", "Q1: Selected Option B", "No",   0.75,      "Adults asked Q1",
  #   "Sex",        "Male",  "perc", "Q1_OptionB",   "categorical", "Q1: Selected Option B", "Yes",  0.30,      "Adults asked Q1",
  #   "Sex",        "Male",  "perc", "Q1_OptionB",   "categorical", "Q1: Selected Option B", "No",   0.70,      "Adults asked Q1",
  #   "Sex",        "Total", "perc", "Satisfaction", "categorical", "Satisfaction",          "Yes",  0.80,      "All adults"     # not folded
  # )

  # Each option's "yes" row, with its question and group attached.
  folded <- data %>%
    filter(stat %in% categorical_stats,
           outcome %in% questions$outcome,
           o_cat %in% keep) %>%
    left_join(questions, by = "outcome") %>%

    # o_cat is worked out first, while o_lab still holds the full label.
    mutate(o_cat        = option_part(o_lab),
           o_lab        = question_part(o_lab),
           outcome_type = "multicoded")

  # One base description per question: the first option's with a non-missing description.
  # (o_base is only there if calc_stats() was given a base.)
  if ("o_base" %in% names(folded)) {
    folded <- folded %>%
      group_by(question) %>%
      mutate(o_base = first(o_base[!is.na(o_base)])) %>%
      ungroup()
  }

  # question is only needed for the base step above.
  folded <- folded %>% select(-question)

  # Check:
  # folded
  # # 4 rows, all "Yes" rows of Q1 - o_lab "Q1", o_cat "Selected Option A"/"B",
  # # outcome_type "multicoded", o_base "All adults" on every row

  # Step 6: Renumber the order columns.
  #   cat_order   -> 1, 2, 3... for the options, within each table block
  #   table_order -> 1, 2, 3... down the folded question's table

  # Reprex: run this, then the Step 6 lines below.
  # library(tidyverse)
  # questions <- tribble(                          # as if from Step 4
  #   ~outcome,     ~question, ~multicode_group,
  #   "Q1_OptionA", 1,         "multicode1",
  #   "Q1_OptionB", 1,         "multicode1"
  # )
  # folded <- tribble(                             # as if from Step 5
  #   ~cross_break, ~p_cat1,  ~stat,  ~outcome,     ~multicode_group, ~o_cat,              ~table_order, ~cat_order,
  #   "Sex",        "Male",   "perc", "Q1_OptionA", "multicode1",     "Selected Option A", 1,            1,
  #   "Sex",        "Female", "perc", "Q1_OptionA", "multicode1",     "Selected Option A", 3,            1,
  #   "Sex",        "Male",   "perc", "Q1_OptionB", "multicode1",     "Selected Option B", 1,            1,
  #   "Sex",        "Female", "perc", "Q1_OptionB", "multicode1",     "Selected Option B", 3,            1
  # )
  # (table_order and cat_order are still each option's own: the "Yes" row
  #  was row 1 of Male and row 3 of the table, under Female.)

  # Option number: the order options are listed in questions (data order).
  option_numbers <- questions %>%
    group_by(multicode_group) %>%
    mutate(option_number = row_number()) %>%
    ungroup() %>%
    select(outcome, option_number)

  folded <- folded %>%
    left_join(option_numbers, by = "outcome") %>%

    # Block number: the order of the blocks in each option's own table
    # (its old table_order, ranked: 1, 3 -> 1, 2).
    group_by(cross_break, stat, outcome) %>%
    mutate(block_number = dense_rank(table_order)) %>%
    ungroup() %>%

    # New table: block by block, options in order within each block.
    arrange(multicode_group, cross_break, stat, block_number, option_number) %>%
    group_by(multicode_group, cross_break, stat) %>%
    mutate(table_order = row_number()) %>%
    ungroup() %>%
    mutate(cat_order = option_number) %>%
    select(-option_number, -block_number)

  # Check:
  # folded
  # # Male   Option A  table_order 1  cat_order 1
  # # Male   Option B  table_order 2  cat_order 2
  # # Female Option A  table_order 3  cat_order 1
  # # Female Option B  table_order 4  cat_order 2

  # Step 7: Put it back together.
  #   Replace each question's option rows with its folded rows, in the place
  #   where its first option was. Every other row stays where it was.

  # Reprex: run this, then the Step 7 lines below.
  # library(tidyverse)
  # categorical_stats <- c("perc", "count", "w_perc", "w_count")   # made at the top of the function
  # questions <- tribble(                          # as if from Step 4
  #   ~outcome,     ~question, ~multicode_group,
  #   "Q1_OptionA", 1,         "multicode1",
  #   "Q1_OptionB", 1,         "multicode1"
  # )
  # data <- tribble(                               # calc_stats() output, before folding
  #   ~cross_break, ~p_cat1,  ~stat,  ~outcome,       ~o_lab,                  ~o_cat, ~estimate, ~table_order,
  #   "Sex",        "Male",   "perc", "Satisfaction", "Satisfaction",          "Yes",  0.80,      1,
  #   "Sex",        "Male",   "perc", "Satisfaction", "Satisfaction",          "No",   0.20,      2,
  #   "Sex",        "Male",   "perc", "Q1_OptionA",   "Q1: Selected Option A", "Yes",  0.35,      1,
  #   "Sex",        "Male",   "perc", "Q1_OptionA",   "Q1: Selected Option A", "No",   0.65,      2,
  #   "Sex",        "Female", "perc", "Q1_OptionA",   "Q1: Selected Option A", "Yes",  0.45,      3,
  #   "Sex",        "Female", "perc", "Q1_OptionA",   "Q1: Selected Option A", "No",   0.55,      4,
  #   "Sex",        "Male",   "perc", "Q1_OptionB",   "Q1: Selected Option B", "Yes",  0.30,      1,
  #   "Sex",        "Male",   "perc", "Q1_OptionB",   "Q1: Selected Option B", "No",   0.70,      2,
  #   "Sex",        "Female", "perc", "Q1_OptionB",   "Q1: Selected Option B", "Yes",  0.20,      3,
  #   "Sex",        "Female", "perc", "Q1_OptionB",   "Q1: Selected Option B", "No",   0.80,      4,
  #   "Sex",        "Male",   "perc", "Q5_Region",    "Q5: Region",            "North",0.60,      1,
  #   "Sex",        "Male",   "perc", "Q5_Region",    "Q5: Region",            "South",0.40,      2
  # )
  # folded <- tribble(                             # as if from Step 6
  #   ~cross_break, ~p_cat1,  ~stat,  ~outcome,     ~multicode_group, ~o_lab, ~o_cat,              ~estimate, ~table_order,
  #   "Sex",        "Male",   "perc", "Q1_OptionA", "multicode1",     "Q1",   "Selected Option A", 0.35,      1,
  #   "Sex",        "Male",   "perc", "Q1_OptionB", "multicode1",     "Q1",   "Selected Option B", 0.30,      2,
  #   "Sex",        "Female", "perc", "Q1_OptionA", "multicode1",     "Q1",   "Selected Option A", 0.45,      3,
  #   "Sex",        "Female", "perc", "Q1_OptionB", "multicode1",     "Q1",   "Selected Option B", 0.20,      4
  # )

  # Remember where every row is now.
  data <- data %>% mutate(row_position = row_number())

  # The rows being replaced: categorical rows of the folded options.
  # (Any other stats for them, e.g. a mean, stay as they are.)
  replaced <- data %>%
    filter(stat %in% categorical_stats,
           outcome %in% questions$outcome) %>%
    left_join(questions, by = "outcome")

  # Where each folded table goes: the first row of its first option's table.
  group_positions <- replaced %>%
    group_by(multicode_group, cross_break, stat) %>%
    summarise(row_position = min(row_position), .groups = "drop")

  folded <- folded %>%
    left_join(group_positions, by = c("multicode_group", "cross_break", "stat"))

  # Everything else stays as it was, and is its own group.
  # (o_cat becomes text, to match the folded rows' option text.)
  kept <- data %>%
    filter(!row_position %in% replaced$row_position) %>%
    mutate(multicode_group = outcome,
           o_cat = as.character(o_cat))

  # Put them together: by position, and folded rows in their new table order.
  data <- kept %>%
    bind_rows(folded) %>%
    arrange(row_position, table_order) %>%
    relocate(multicode_group, .after = outcome) %>%
    select(-row_position)

  # Check:
  # data
  # # Satisfaction (2 rows), then Q1 folded (Male A, Male B, Female A, Female B),
  # # then Q5_Region (2 rows). multicode_group: Satisfaction, multicode1, Q5_Region.

  data
}

# 
# # ---- Reprex: end to end -----------------------------------------------------------
# # calc_stats() without multicode handling, then convert_multicodes() on it.
# # (Inside calc_stats() it's just: calc_stats(..., multicode = list(q1)).)
# # Run from the Descriptives project.
# suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
# source(here::here("dtab", "dev", "calc_stats_draft.R"))    # sources this file
# load(here::here("data", "synthetic_multicode_data.rds"))   # gives `survey_data`
# 
# q1  <- paste0("Q1_Option", LETTERS[1:5])
# raw <- calc_stats(survey_data, c("Satisfaction", q1), "Sex", "perc")
# 
# result <- convert_multicodes(raw, multicode = list(q1))
# result %>%
#   select(cross_break, p_cat1, outcome, multicode_group, o_lab, o_cat,
#          estimate, table_order, cat_order, row_order) %>%
#   print(n = Inf)
