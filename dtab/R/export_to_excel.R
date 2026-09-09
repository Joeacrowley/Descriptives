# export_to_excel.R: one generic huxtable -> Excel converter, working
# against any format_*() output rather than needing one per table type.
#
# detect_stem_columns() infers how many leading "stem" columns (Variable/
# Statistics) exist from LEFT ALIGNMENT - a convention every format_*()
# function follows (those columns left-aligned, everything else right).
# Breaks silently if that convention ever changes.
#
# KNOWN LIMITATION: cells export as text, not real numbers - the
# format_*() functions render digits as plain strings before as_hux()
# ever runs and drop stat_code before that point, so there's no way to
# recover "this was a percentage vs. a count" from the huxtable alone.


# Widest cell's character length per column, ignoring cells shadowed by a
# colspan/rowspan merge (a shadowed cell's content is a copy of the anchor
# cell's - see pivot_crosstab.R's note on this huxtable behaviour - so
# counting it again would double-count the same text).
longest_per_column <- function(ht) {
  if (!inherits(ht, "huxtable")) stop("longest_per_column() needs a huxtable.")

  df <- as.data.frame(ht)
  rs <- huxtable::rowspan(ht)
  cs <- huxtable::colspan(ht)

  mask <- (rs > 0 & cs > 0)
  df[!mask] <- NA

  cell_widths <- apply(df, 2, function(x) {
    if (all(is.na(x))) return(NA)
    x[which.max(nchar(x))]
  }) %>% purrr::map_int(stringr::str_length)

  cell_widths <- cell_widths + 3
  purrr::map_int(cell_widths, ~ max(.x, 10))
}


# colspan() matrix with every cell COVERED by a span (not just the
# anchor) zeroed out, so a later check sees one non-zero entry per span.
column_spanners <- function(ht) {
  spanners <- huxtable::colspan(ht)
  no_of_cols <- ncol(spanners)
  no_of_rows <- nrow(spanners)

  for (col in seq_len(no_of_cols)) {
    for (row in seq_len(no_of_rows)) {
      cell_value <- spanners[row, col]
      if (cell_value > 1) {
        for (offset in seq_len(cell_value - 1)) {
          spanners[row, col + offset] <- 0
        }
      }
    }
  }

  spanners
}


# How many Excel text-rows a header row's content needs to wrap into,
# given the column width(s) each header cell spans.
recommended_row_span <- function(ht, column_widths, column_spans, row) {
  cols <- ncol(column_spans)
  row_span <- 1

  content_df <- as.data.frame(ht)

  for (i in seq_len(cols)) {
    cols_span <- column_spans[row, i]
    if (cols_span > 0) {
      col_span_end <- i + (cols_span - 1)
      selected_col_widths <- sum(column_widths[i:col_span_end])
      content <- stringr::str_length(content_df[row, i])

      recommended <- 1
      ratio <- content / selected_col_widths
      if (ratio > 1) recommended <- ceiling(ratio)

      if (recommended > row_span) row_span <- recommended
    }
  }

  row_span
}


# Checked on the LAST row deliberately - guaranteed a real body row, never
# a header row (spanning header rows are centered, not left/right).
detect_stem_columns <- function(ht) {
  check_row <- nrow(ht)
  aligns <- huxtable::align(ht)[check_row, ]
  is_left <- aligns == "left"

  if (!isTRUE(is_left[1])) return(0L)

  run_lengths <- rle(is_left)
  as.integer(run_lengths$lengths[1])
}


# Per-column Excel widths: header-only sizing for ordinary columns
# (numeric estimates carry far more internal decimal places than are ever
# displayed, so sizing off the full body would make columns needlessly
# wide), full-column sizing (capped) for the leading stem column(s).
column_widths <- function(ht, stem_cols = detect_stem_columns(ht), max_stem_width = 45) {
  hux_suppressed <- ht
  spanners <- huxtable::colspan(hux_suppressed)
  for (col in seq_len(ncol(spanners))) {
    for (row in seq_len(nrow(spanners))) {
      if (spanners[row, col] > 1) hux_suppressed[row, col] <- ""
    }
  }

  header_row_idx <- which(huxtable::header_rows(ht))
  body_widths <- longest_per_column(hux_suppressed[header_row_idx, , drop = FALSE])
  body_widths <- ifelse(body_widths > 18, 18, body_widths)

  if (stem_cols > 0) {
    stem_widths <- longest_per_column(hux_suppressed[, seq_len(stem_cols), drop = FALSE])
    stem_widths <- ifelse(stem_widths > max_stem_width, max_stem_width, stem_widths)
    body_widths[seq_len(stem_cols)] <- stem_widths
  }

  body_widths
}


# `title` is explicit, not read from huxtable::caption(ht) - no format_*()
# function sets a caption, so relying on it would silently skip every
# title. caption() is still checked as a fallback; `title` wins if given.
add_huxtable_to_workbook <- function(ht, workbook, sheet, title = NULL,
                                      stem_cols = NULL, max_stem_width = 45) {

  if (is.null(stem_cols)) stem_cols <- detect_stem_columns(ht)

  if (is.null(title)) {
    cap <- huxtable::caption(ht)
    if (!is.null(cap) && !is.na(cap)) title <- cap
  }
  huxtable::caption(ht) <- NA   # never let as_Workbook() print its own caption row

  workbook_interim <- ht |>
    huxtable::as_Workbook(Workbook = workbook, start_row = 3, start_col = 2, sheet = sheet)

  end_row <- 2 + nrow(ht)
  end_col <- 1 + ncol(ht)

  openxlsx::showGridLines(workbook_interim, sheet, showGridLines = FALSE)

  openxlsx::addStyle(workbook_interim, sheet,
                      style = openxlsx::createStyle(wrapText = TRUE),
                      cols = 2:end_col, rows = 3:end_row, gridExpand = TRUE, stack = TRUE)

  openxlsx::removeColWidths(workbook_interim, sheet, cols = 2:end_col)
  col_widths <- column_widths(ht, stem_cols = stem_cols, max_stem_width = max_stem_width)
  openxlsx::setColWidths(workbook_interim, sheet, cols = 2:end_col, widths = col_widths)
  openxlsx::setColWidths(workbook_interim, sheet, cols = 1, widths = 4)

  head_row_idx <- which(huxtable::header_rows(ht))
  if (length(head_row_idx) > 0) {
    col_spans <- column_spanners(ht)
    header_row_spans <- purrr::map_int(head_row_idx, ~ recommended_row_span(ht, col_widths, col_spans, .x))
    for (i in seq_along(head_row_idx)) {
      openxlsx::setRowHeights(workbook_interim, sheet, rows = i + 2, heights = header_row_spans[i] * 15)
    }
  }

  if (!is.null(title)) {
    openxlsx::writeData(workbook_interim, sheet = sheet, startRow = 2, startCol = 2, x = title)
    openxlsx::addStyle(workbook_interim, sheet,
                        style = openxlsx::createStyle(wrapText = TRUE, fontName = "Arial", fontSize = 13,
                                                        textDecoration = "bold", valign = "center", halign = "left"),
                        cols = 2:end_col, rows = 2, gridExpand = TRUE, stack = TRUE)
    openxlsx::mergeCells(workbook_interim, sheet = sheet, rows = 2, cols = 2:end_col)

    table_width <- sum(col_widths)
    title_width <- stringr::str_length(title)
    multiplier  <- ceiling(title_width / table_width)
    openxlsx::setRowHeights(workbook_interim, sheet, rows = 2, heights = 30 * multiplier)
  }

  workbook_interim
}


# The Contents sheet is only added when hux_list has more than one table -
# this is what lets export_table_to_excel() below be a genuine
# single-sheet wrapper (delegates here with a length-1 list).

#' Export any number of huxtables to one .xlsx file, one sheet per table
#'
#' Writes each table to its own sheet, and - for 2 or more tables only -
#' adds an auto-generated, hyperlinked Contents sheet placed first. For a
#' single table, use [export_table_to_excel()] instead (a thin wrapper
#' around this function that skips the Contents sheet entirely).
#'
#' @param hux_list A list of `huxtable` objects, as returned by any of this
#'   package's `format_*()` functions (or `make_*()` with
#'   `formatted = TRUE`).
#' @param file Output path for the .xlsx file.
#' @param sheet_names Optional character vector of sheet names, one per
#'   table. Defaults to `"Table 1"`, `"Table 2"`, ...
#' @param titles Optional character vector of title-row text, one per
#'   table (also used for the Contents sheet's listing, if shown).
#' @param stem_cols Optional column width override(s) for the stub
#'   column(s), passed through to the underlying worksheet writer.
#' @param max_stem_width Maximum stub column width, in Excel column-width
#'   units. Default 45.
#'
#' @return Invisibly, the `openxlsx` workbook object. Primarily called for
#'   its side effect of writing `file`.
#'
#' @seealso [export_table_to_excel()]
#' @export
export_tables_to_excel <- function(hux_list, file, sheet_names = NULL, titles = NULL,
                                    stem_cols = NULL, max_stem_width = 45) {

  if (is.null(sheet_names)) sheet_names <- paste0("Table ", seq_along(hux_list))
  if (length(sheet_names) != length(hux_list)) {
    stop("sheet_names must have one entry per table in hux_list.")
  }
  if (!is.null(titles) && length(titles) != length(hux_list)) {
    stop("titles must have one entry per table in hux_list (or be NULL).")
  }

  workbook <- openxlsx::createWorkbook()

  for (i in seq_along(hux_list)) {
    title_i <- if (is.null(titles)) NULL else titles[[i]]
    workbook <- add_huxtable_to_workbook(hux_list[[i]], workbook, sheet = sheet_names[i],
                                          title = title_i, stem_cols = stem_cols,
                                          max_stem_width = max_stem_width)
  }

  if (length(hux_list) > 1) {
    # Contents sheet - one hyperlinked row per table, placed first.
    openxlsx::addWorksheet(workbook, "Contents", gridLines = FALSE)
    openxlsx::writeData(workbook, sheet = "Contents", x = "Table of contents", startRow = 2, startCol = 2)
    openxlsx::addStyle(workbook, sheet = "Contents", cols = 2, rows = 2,
                        style = openxlsx::createStyle(fontName = "Arial", fontSize = 12, textDecoration = "bold"))

    contents_titles <- if (is.null(titles)) sheet_names else titles
    for (i in seq_along(sheet_names)) {
      row <- 3 + i
      openxlsx::writeFormula(workbook, sheet = "Contents",
                              x = paste0('=HYPERLINK("#', sheet_names[i], '!A1", "', sheet_names[i], '")'),
                              startRow = row, startCol = 2)
      openxlsx::writeData(workbook, sheet = "Contents", x = contents_titles[[i]], startRow = row, startCol = 3)
    }
    openxlsx::setColWidths(workbook, "Contents", cols = 1, widths = 4)

    # Added last, since its hyperlinks reference sheets that must already
    # exist - moved to the front afterwards.
    n_sheets <- length(sheet_names) + 1
    openxlsx::worksheetOrder(workbook) <- c(n_sheets, seq_len(n_sheets - 1))
  }

  openxlsx::saveWorkbook(workbook, file = file, overwrite = TRUE)
  invisible(workbook)
}


#' Export a single huxtable to a one-sheet .xlsx file
#'
#' Convenience wrapper around [export_tables_to_excel()] for the common
#' single-table case - delegates to it with a length-1 list, which (via
#' that function's own guard) produces a workbook with exactly the one
#' named sheet, no Contents tab alongside it.
#'
#' @param ht A single `huxtable` object, as returned by any of this
#'   package's `format_*()` functions (or `make_*()` with
#'   `formatted = TRUE`).
#' @param file Output path for the .xlsx file.
#' @param sheet Sheet name. Default `"Table 1"`.
#' @param title Optional title-row text.
#'
#' @return Invisibly, the `openxlsx` workbook object. Primarily called for
#'   its side effect of writing `file`.
#'
#' @seealso [export_tables_to_excel()]
#' @export
export_table_to_excel <- function(ht, file, sheet = "Table 1", title = NULL) {
  export_tables_to_excel(list(ht), file, sheet_names = sheet,
                          titles = if (is.null(title)) NULL else list(title))
}
