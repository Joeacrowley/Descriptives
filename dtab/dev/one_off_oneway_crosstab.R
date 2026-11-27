# =============================================================================
# One-off: one-way crosstabs with %, 95% CI, Sig. diff and count grouped
# under each outcome level, exported to Excel - one sheet per set of
# outcomes, every sheet crosstabbed by the same predictors.
#
#                          Statistics  Total    RSex: Male  RSex: Female  RAgeCat: 18-24 ...
#   Column reference                            a           b             a
#   <outcome label>
#   Level 1                %           29       32          27            ...
#                          95% CI      27 - 31  29 - 35     25 - 30
#                          Sig. diff            b           a
#                          Count       932      476         456
#   Level 2                ...
#   Base                               3,224    1,492       1,732
#
# Uses calc_stats() for the numbers, then reshapes them here rather than
# through the pivot functions (which group rows by statistic, not by level).
# =============================================================================

suppressMessages(devtools::load_all(here::here("dtab"), quiet = TRUE))
library(tidyverse)


# ---- settings ----------------------------------------------------------------

data <- readRDS(here::here("data", "synthetic_bsa_clean.rds"))

# One sheet per set of outcomes. The names become the sheet names
# (Excel allows 31 characters).
tables <- list(
  "Poverty"  = c("MuchPov2", "skipmeal2"),
  "Politics" = c("leftrigh2", "libauth2", "welfare3")
)

# Every sheet is crosstabbed by these.
predictors <- c("RSex", "RAgeCat")

out_file <- here::here("oneway_crosstabs.xlsx")


# ---- 1. build one table ---------------------------------------------------------

make_crosstab <- function(outcomes) {

  stats <- calc_stats(data,
                      outcomes   = outcomes,
                      predictors = as.list(predictors),
                      statistics = c("perc", "count"),
                      conf       = "ci",
                      pairwise   = TRUE)

  # Each column's name: "Total", or the predictor's variable name and the
  # level, e.g. "RSex: Male". Predictors sharing level names stay apart.
  stats <- stats %>%
    mutate(column = if_else(cross_break == "Total", "Total", paste0(cross_break, ": ", p_cat1)))

  # Columns in the order calc_stats() gives them: Total, then each predictor.
  column_order <- unique(stats$column)

  # One row per statistic to show, as text. Percentages and their CIs as
  # whole numbers; counts with a thousands comma.
  pct <- function(x) formatC(x * 100, digits = 0, format = "f")

  perc_rows <- stats %>%
    filter(stat == "perc") %>%
    transmute(outcome, o_cat, cat_order, column,
              `%`         = pct(estimate),
              `95% CI`    = paste(pct(estimate_low), "-", pct(estimate_upp)),
              `Sig. diff` = sig_diff) %>%
    pivot_longer(c(`%`, `95% CI`, `Sig. diff`), names_to = "Statistics", values_to = "value")

  count_rows <- stats %>%
    filter(stat == "count") %>%
    transmute(outcome, o_cat, cat_order, column,
              Statistics = "Count",
              value      = formatC(unweighted_n, big.mark = ",", format = "d"))

  # Wide: one column per predictor level, the statistics grouped by level.
  wide <- bind_rows(perc_rows, count_rows) %>%
    mutate(Statistics = factor(Statistics, levels = c("%", "95% CI", "Sig. diff", "Count"))) %>%
    arrange(match(outcome, outcomes), cat_order, Statistics) %>%
    pivot_wider(names_from = column, values_from = value) %>%
    select(outcome, o_cat, Statistics, all_of(column_order))

  # The letter each column is referred to by in the Sig. diff rows.
  letters_row <- stats %>%
    filter(cross_break != "Total") %>%
    distinct(column, sig_letter) %>%
    pivot_wider(names_from = column, values_from = sig_letter) %>%
    mutate(Variable = "Column reference")

  # Per outcome: a title row, its levels, and its base row.
  outcome_rows <- map_df(outcomes, function(out) {
    rows <- wide %>% filter(outcome == out)

    title_row <- tibble(Variable = stats$o_lab[stats$outcome == out][1])

    # Each level's name on its first row only.
    body_rows <- rows %>%
      mutate(Variable   = if_else(Statistics == "%", o_cat, NA_character_),
             Statistics = as.character(Statistics)) %>%
      select(Variable, Statistics, all_of(column_order))

    base_row <- stats %>%
      filter(outcome == out, stat == "count") %>%
      distinct(column, base) %>%
      mutate(base = formatC(base, big.mark = ",", format = "d")) %>%
      pivot_wider(names_from = column, values_from = base) %>%
      mutate(Variable = "Base")

    bind_rows(title_row, body_rows, base_row, tibble(Variable = NA_character_))
  })

  # Drop the blank row after the last outcome.
  outcome_rows <- head(outcome_rows, -1)

  bind_rows(letters_row, outcome_rows) %>%
    select(Variable, Statistics, all_of(column_order)) %>%
    mutate(row_kind = case_when(
      Variable %in% "Column reference"   ~ "letters",
      Variable %in% stats$o_lab          ~ "title",
      Variable %in% "Base"               ~ "base",
      Statistics %in% "Count"            ~ "count",
      TRUE                               ~ "other"
    ))
}


# ---- 2. write one table to its own sheet ----------------------------------------

write_crosstab_sheet <- function(wb, sheet, table) {

  row_kind <- table$row_kind
  table    <- table %>% select(-row_kind)

  openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
  openxlsx::writeData(wb, sheet, table, na.string = "")

  n_cols   <- ncol(table)
  n_rows   <- nrow(table) + 1          # + the header row
  num_cols <- 3:n_cols

  # Sheet row numbers of each kind of row (+1 for the header row).
  rows_of <- function(kind) which(row_kind == kind) + 1

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
  style(textDecoration = "italic", fontColour = "#595959", rows = rows_of("letters"), cols = 1:n_cols)

  # Outcome titles in bold.
  style(textDecoration = "bold", rows = rows_of("title"), cols = 1)

  # A light line under each level's group of rows.
  style(border = "bottom", borderColour = "#D9D9D9", rows = rows_of("count"), cols = 1:n_cols)

  # Base rows: italic, with a line above.
  style(textDecoration = "italic", border = "top", borderColour = "#404040",
        rows = rows_of("base"), cols = 1:n_cols)

  # Column widths: enough for the longest entry, up to a limit (longer text
  # wraps). Never narrower than the longest single word, so words don't split.
  max_widths <- c(45, 12, rep(20, n_cols - 2))
  cells      <- rbind(names(table), as.matrix(table))

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
}


# ---- 3. build every table and save ----------------------------------------------

wb <- openxlsx::createWorkbook()
openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = "Arial")

for (table_name in names(tables)) {
  crosstab <- make_crosstab(tables[[table_name]])
  write_crosstab_sheet(wb, substr(table_name, 1, 31), crosstab)
}

openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)
