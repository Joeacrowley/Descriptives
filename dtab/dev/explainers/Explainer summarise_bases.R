# Explainer: summarise_bases()
# Turns the base columns calc_stats() adds (o_base, p_base1...) into footnote lines.
# Uses a small hand-made tibble, so each step is easy to see.
# Paths use here::here(), so it runs from anywhere in the Descriptives project.

suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
source(here::here("dtab", "dev", "draft_bases.R"))


# ---- The input ----
# Two outcomes, broken down by Sex. Each label column has a base column next to it.
# The Total rows have no predictor, so p_lab1 is "Total" and p_base1 is NA.
tables <- tribble(
  ~o_lab,           ~o_base,                    ~p_lab1, ~p_base1,
  "Fraud estimate", "Those asked about fraud",  "Total", NA,
  "Fraud estimate", "Those asked about fraud",  "Sex",   "All respondents",
  "Fraud estimate", "Those asked about fraud",  "Sex",   "All respondents",
  "Left-right",     "All respondents",          "Total", NA,
  "Left-right",     "All respondents",          "Sex",   "All respondents",
  "Left-right",     "All respondents",          "Sex",   "All respondents"
)
tables


# ---- Step 1: which base columns are there? ----
base_cols <- tables %>% select(any_of("o_base"), starts_with("p_base")) %>% names()
base_cols


# ---- Step 2: pair each base with its label, one row per variable ----
# o_base goes with o_lab, p_base1 with p_lab1. Stack them, drop Total, keep distinct.
pairs <- map_df(base_cols, function(base_col) {
  lab_col <- if (base_col == "o_base") "o_lab" else str_replace(base_col, "p_base", "p_lab")
  tibble(label = tables[[lab_col]], base = tables[[base_col]])
}) %>%
  filter(label != "Total") %>%
  distinct()
pairs
# Three variables: two have "All respondents", one has a specific base.


# ---- Step 3: the most common base ----
pairs %>% count(base, sort = TRUE)
# "All respondents" covers most variables, so it becomes "All other variables".


# ---- Step 4: the result ----
# The other bases get a line each, listing their variables; then "All other variables".
summarise_bases(tables)


# ---- Other cases ----

# Every variable has the same base: one line.
tables %>% mutate(o_base = "All respondents") %>% summarise_bases()

# Some variables have no base (no general base given): no "All other variables"
# line, since it would wrongly cover them too.
tables %>% mutate(p_base1 = NA) %>% summarise_bases()

# No base columns at all: NULL.
tables %>% select(o_lab, p_lab1) %>% summarise_bases()
