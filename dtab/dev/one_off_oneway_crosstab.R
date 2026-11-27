# =============================================================================
# One-off: one-way crosstabs with %, 95% CI, Sig. diff and unweighted count
# grouped under each outcome level, exported to Excel.
#
#   Level 1   %           29        32       27
#             95% CI      27 - 31   29 - 35  25 - 30
#             Sig. diff             b        a
#             Count       932       476      456
#   Level 2   ...
#
# Uses calc_stats() for the numbers, then reshapes them here rather than
# through the pivot functions (which group rows by statistic, not by level).
# =============================================================================

suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
library(tidyverse)
library(srvyr)


# ---- settings ----------------------------------------------------------------

bsa    <- readRDS(here::here("data", "synthetic_bsa_clean.rds"))
design <- bsa %>% as_survey_design(weights = WtFactor)

outcomes  <- c("MuchPov2", "welfare3")
predictor <- "RSex"
out_file  <- here::here("oneway_crosstabs.xlsx")


# ---- 1. calculate the statistics ---------------------------------------------

stats <- calc_stats(design,
                    outcomes   = outcomes,
                    predictors = predictor,
                    statistics = c("w_perc", "count"),
                    conf       = "ci",
                    pairwise   = TRUE)


# ---- 2. one row per statistic to show, as text ---------------------------------

# Percentages and their CIs as whole numbers; counts with a thousands comma.
pct <- function(x) formatC(x * 100, digits = 0, format = "f")

perc_rows <- stats %>%
  filter(stat == "w_perc") %>%
  transmute(outcome, o_lab, o_cat, cat_order, p_cat1,
            `%`         = pct(estimate),
            `95% CI`    = paste(pct(estimate_low), "-", pct(estimate_upp)),
            `Sig. diff` = sig_diff) %>%
  pivot_longer(c(`%`, `95% CI`, `Sig. diff`), names_to = "Statistics", values_to = "value")

count_rows <- stats %>%
  filter(stat == "count") %>%
  transmute(outcome, o_lab, o_cat, cat_order, p_cat1,
            Statistics = "Count",
            value      = formatC(unweighted_n, big.mark = ",", format = "d"))

long <- bind_rows(perc_rows, count_rows) %>%
  mutate(Statistics = factor(Statistics, levels = c("%", "95% CI", "Sig. diff", "Count")))


# ---- 3. wide: one column per predictor level -----------------------------------

# Columns in the order calc_stats() gives them: Total first, then each level.
column_order <- unique(stats$p_cat1)

wide <- long %>%
  arrange(match(outcome, outcomes), cat_order, Statistics) %>%
  pivot_wider(names_from = p_cat1, values_from = value) %>%
  select(outcome, o_lab, o_cat, Statistics, all_of(column_order))


# ---- 4. add a title row per outcome, and letter and base rows -----------------

letters_row <- stats %>%
  filter(cross_break != "Total") %>%
  distinct(p_cat1, sig_letter) %>%
  pivot_wider(names_from = p_cat1, values_from = sig_letter) %>%
  mutate(Variable = "Column reference", Statistics = NA_character_)

table_rows <- map_df(outcomes, function(out) {
  rows <- wide %>% filter(outcome == out)

  title_row <- tibble(Variable = rows$o_lab[1])

  # Show each level's name on its first row only.
  body_rows <- rows %>%
    mutate(Variable = if_else(Statistics == "%", o_cat, NA_character_),
           Statistics = as.character(Statistics)) %>%
    select(Variable, Statistics, all_of(column_order))

  base_row <- stats %>%
    filter(outcome == out, stat == "count") %>%
    distinct(p_cat1, base) %>%
    mutate(base = formatC(base, big.mark = ",", format = "d")) %>%
    pivot_wider(names_from = p_cat1, values_from = base) %>%
    mutate(Variable = "Base (unweighted)", Statistics = NA_character_)

  bind_rows(title_row, body_rows, base_row, tibble(Variable = NA_character_))
})

# Drop the blank row after the last outcome.
table_rows <- head(table_rows, -1)

final_table <- bind_rows(letters_row, table_rows) %>%
  select(Variable, Statistics, all_of(column_order))

# Column headers: the predictor's label before each of its levels,
# e.g. "Sex of respondent: Male".
predictor_label <- stats %>% filter(cross_break != "Total") %>% pull(p_lab1) %>% first()
level_cols      <- setdiff(column_order, "Total")

final_table <- final_table %>%
  rename_with(~ paste0(predictor_label, ": ", .x), all_of(level_cols))


# ---- 5. write to Excel ----------------------------------------------------------

sheet <- "Crosstabs"

wb <- openxlsx::createWorkbook()
openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial")
openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
openxlsx::writeData(wb, sheet, final_table, na.string = "")

n_cols   <- ncol(final_table)
n_rows   <- nrow(final_table) + 1          # + the header row
num_cols <- 3:n_cols

# Sheet row numbers of each kind of row (+1 for the header row).
row_of      <- function(condition) which(condition) + 1
letter_rows <- row_of(final_table$Variable %in% "Column reference")
title_rows  <- row_of(final_table$Variable %in% stats$o_lab)
count_rows  <- row_of(final_table$Statistics %in% "Count")
base_rows   <- row_of(final_table$Variable %in% "Base (unweighted)")

style <- function(..., rows, cols) {
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(...),
                     rows = rows, cols = cols, gridExpand = TRUE, stack = TRUE)
}

# Every cell: text at the top, wrapped if it doesn't fit.
style(valign = "top", wrapText = TRUE, rows = 1:n_rows, cols = 1:n_cols)

# Numbers right-aligned, header included.
style(halign = "right", rows = 1:n_rows, cols = num_cols)

# Header: bold, with a line above and below.
style(textDecoration = "bold", valign = "bottom", border = "TopBottom",
      borderColour = "#404040", rows = 1, cols = 1:n_cols)

# Column letters in grey italics.
style(textDecoration = "italic", fontColour = "#595959", rows = letter_rows, cols = 1:n_cols)

# Outcome titles in bold.
style(textDecoration = "bold", rows = title_rows, cols = 1)

# A light line under each level's group of rows.
style(border = "bottom", borderColour = "#D9D9D9", rows = count_rows, cols = 1:n_cols)

# Base rows: italic, with a line above.
style(textDecoration = "italic", border = "top", borderColour = "#404040",
      rows = base_rows, cols = 1:n_cols)

# Column widths: enough for the longest entry, up to a limit (longer text
# wraps). Never narrower than the longest single word, so words don't split.
max_widths <- c(45, 12, rep(20, n_cols - 2))
cells      <- rbind(names(final_table), as.matrix(final_table))

col_widths <- map_dbl(seq_len(n_cols), function(col) {
  text <- na.omit(cells[, col])
  longest_word <- max(nchar(unlist(str_split(text, " "))))
  max(min(max_widths[col], max(nchar(text)) + 2), longest_word + 2)
})
openxlsx::setColWidths(wb, sheet, cols = seq_len(n_cols), widths = col_widths)

# Row heights: enough lines for the row's most-wrapped cell.
lines_needed <- function(text, width) {
  if (is.na(text) || text == "") return(1L)
  length(strwrap(text, width = width - 1))
}
row_lines <- map_int(seq_len(nrow(cells)), function(r) {
  max(map2_int(cells[r, ], col_widths, lines_needed))
})
openxlsx::setRowHeights(wb, sheet, rows = seq_len(n_rows), heights = 13 * row_lines + 2)

# Keep the header in view when scrolling.
openxlsx::freezePane(wb, sheet, firstRow = TRUE)

openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
