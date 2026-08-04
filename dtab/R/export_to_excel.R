# =============================================================================
# export_to_excel.R — one generic huxtable -> Excel converter, working
# against the styled huxtables make_table()/make_numeric_summary_table()
# already return (or any format_*() output directly), rather than needing a
# different converter per table type.
#
# Ported and generalised from the exploratory Excel-export work in
# "0_0_Old/Deprecated_Tables/Tables function/8. Put bits together.Rmd"
# (addh(), make_workbook(), and their helpers longest_per_column(),
# extract_column_spanners(), extract_recommended_row_span(), col_width()).
# That code already answered Joe's own question - is one converter enough,
# or does it need to be per-table-type - correctly: almost everything there
# operates purely on the huxtable's own structure (caption(), colspan(),
# header_rows(), as_Workbook()) with no table-type branching at all. The one
# genuinely type-specific piece was col_width(), which took a `pivot_number`
# string and used it purely to look up how many leading "stem" columns
# (Variable/Statistics-style label columns) should get their width measured
# against their WHOLE column's content rather than just the header row, plus
# a max-width cap for those columns.
#
# ---- the one real generalisation made here ----------------------------------
# detect_stem_columns() replaces that pivot_number lookup - it infers the
# same thing (how many leading columns are "stem" columns) from LEFT
# ALIGNMENT instead of a table-type string. This works because it's already
# a consistent convention across every current format_*() function in this
# project: Variable and Statistics (format_summary()/format_crosstab()/
# format_nested_crosstab()) or just Variable alone (format_numeric_summary(),
# which has no Statistics column - one row per variable instead) are always
# the only left-aligned columns; every other column is right-aligned (see
# each format_*()'s own "alignment: Variable/Statistics left, everything else
# right" comment). So this one function replaces col_width()'s per-table-type
# branch without needing to know or care which format_*() produced the
# huxtable it's given.
#
# ---- known limitation: cells export as TEXT, not real numbers --------------
# The original Deprecated_Tables code went to real lengths (see its own "Well,
# ---- convert_number_format() blanks a cell's numeric formatting and rebuilds
# it as an Excel number format code, applied to the cell via addStyle()"
# section) to solve exactly the problem Notes.Rmd flagged: "Figure out how to
# get numbers, not text, into Excel." That fix depended on its huxtables
# storing REAL numeric values with a display-formatting FUNCTION attached via
# huxtable's own number_format()<- mechanism - the value stays numeric, only
# how it's SHOWN changes.
#
# The current format_*() functions (format_summary()/format_crosstab()/
# format_nested_crosstab()/format_numeric_summary()) don't do that - digit
# formatting happens via format_statistic()/format_pvalue()/format_ci_string()
# as plain CHARACTER STRINGS, before as_hux() ever runs (a design decision
# already extensively tested this session - see each format_*()'s own "digit
# formatting, as plain text, before the huxtable exists" comment). By the
# time a styled huxtable exists, the real numeric value is already gone, and
# so is stat_code (explicitly dropped in every format_*()'s own select() call
# right before as_hux()) - there's no way to recover "this text was
# originally a percentage vs. a count" from the huxtable alone. It's not just
# that the information is dropped, either: format_statistic() renders a
# percentage as a bare "50" with no "%" marker at all (sprintf("%.0f", 100 *
# x)) - textually indistinguishable from a count of 50 - so even a best-effort
# guess from the displayed text can't reliably tell them apart.
#
# Getting real numbers into Excel here would mean refactoring all four
# format_*() functions to preserve numeric values + attach number_format()
# instead of collapsing to text upfront - matching the OLD code's approach,
# but a genuinely bigger and riskier change than this file, given how much
# already-tested code (is.na() checks, set_na_string(), and every existing
# test's exact-string assertions across four test files) assumes text cells.
# Not attempted here - flagged as a real follow-up option if Joe wants it,
# not silently worked around or silently skipped.
#
# I haven't been able to run any of this myself - no R (or openxlsx) available
# in this environment. See test_export_to_excel.R for what's covered - the
# pure-structural helpers only (detect_stem_columns(), column_widths(),
# column_spanners(), recommended_row_span()), not the actual openxlsx/Excel
# file-writing functions, which need a real Excel-reading step to verify at
# all and are left untested here for that reason.
# =============================================================================



# ---- longest_per_column: widest cell's character length per column, -------
# ---- ignoring cells that are shadowed by a colspan/rowspan merge -----------
# (their content is a copy of the anchor cell's - see pivot_crosstab.R's own
# note on this huxtable behaviour - so counting them again would double-count
# the same text). +3 padding, floored at a minimum width of 10, matching the
# original code's own tuning exactly (untouched here - this is about what
# "looks right" in Excel, not something to second-guess without being able
# to actually open a workbook and look).
longest_per_column <- function(ht) {
  if (!inherits(ht, "huxtable")) stop("longest_per_column() needs a huxtable.")

  df <- as.data.frame(ht)
  rs <- huxtable::rowspan(ht)
  cs <- huxtable::colspan(ht)

  # Only "real" (anchor) cells count - rs/cs are 1 for an ordinary cell, and
  # only greater than 1 AT the anchor of a merge (see huxtable's own spans.Rd
  # docs) - so rs > 0 & cs > 0 keeps every anchor cell and drops nothing
  # incorrectly; the actual shadowed cells never reach this mask at all
  # since huxtable's colspan()/rowspan() accessors report 1 there too, not 0
  # - this mask is really just "is a real cell", inherited as-is from the
  # original code.
  mask <- (rs > 0 & cs > 0)
  df[!mask] <- NA

  cell_widths <- apply(df, 2, function(x) {
    if (all(is.na(x))) return(NA)
    x[which.max(nchar(x))]
  }) %>% purrr::map_int(stringr::str_length)

  cell_widths <- cell_widths + 3
  purrr::map_int(cell_widths, ~ max(.x, 10))
}


# ---- column_spanners: colspan() matrix with every cell COVERED by a span --
# ---- (not just the anchor) zeroed out, so a later "is this cell part of a --
# ---- span, and how wide" check only ever sees one non-zero entry per span --
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


# ---- recommended_row_span: how many Excel text-rows a header row's ---------
# ---- content would need to wrap into, given the column width(s) each -------
# ---- header cell spans - the max across every cell in that row -------------
recommended_row_span <- function(ht, column_widths, column_spans, row) {
  cols <- ncol(column_spans)
  row_span <- 1

  # as.data.frame(ht) - not because ht[row, i]/ht[[i]][row] turned out to be
  # wrong (a real R run eventually showed all three extract identical, CORRECT
  # text - the repeated test failure chasing this was actually a test-setup
  # bug, not an extraction bug; see test_export_to_excel.R's own note on
  # huxtable::hux()'s default header row for the real story). Kept as
  # as.data.frame() anyway since it's the same mechanism longest_per_column()
  # already relies on, and there's no reason to prefer three different
  # extraction styles across two functions in the same file once one is
  # confirmed to work.
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


# ---- detect_stem_columns: the ONE piece that used to be table-type- --------
# ---- specific (col_width()'s `pivot_number` lookup) - replaced with a ------
# ---- structural inference instead, see this file's header note for why ----
# Checked on the LAST row deliberately, not the first - guaranteed to be a
# real body row (data, base, or the relocated "Sample sizes" section - see
# each format_*()'s own note that its right-alignment is scoped to
# header_offset:no_of_rows, i.e. every row from the first body row onward,
# not just typical "data" rows), never a header row, which could have its
# own different alignment (spanning header rows are centered, not left/right
# - see the header-alignment fix already made this session).
detect_stem_columns <- function(ht) {
  check_row <- nrow(ht)
  aligns <- huxtable::align(ht)[check_row, ]
  is_left <- aligns == "left"

  if (!isTRUE(is_left[1])) return(0L)

  # Length of the leading run of left-aligned columns - stops at the first
  # non-left column, so a coincidentally-left-aligned column further along
  # (shouldn't happen given current conventions, but not assumed away
  # either) doesn't get counted as part of the stem.
  run_lengths <- rle(is_left)
  as.integer(run_lengths$lengths[1])
}


# ---- column_widths: per-column Excel widths - header-only sizing for -------
# ---- ordinary columns (numeric estimates print with far more decimal -------
# ---- places internally than are ever actually DISPLAYED, so sizing off ----
# ---- the full body would make columns needlessly wide), full-column --------
# ---- sizing (capped) for the leading stem column(s) ------------------------
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


# ---- add_huxtable_to_workbook: one huxtable -> one worksheet in an ---------
# ---- existing (or newly created) openxlsx Workbook ------------------------
# `title` is an explicit argument, not read from huxtable::caption(ht) - none
# of the current format_*() functions set a caption (checked directly, not
# assumed), so relying on it would silently skip every title in practice.
# caption() is still checked as a fallback, in case a caller sets one by hand
# or a future format_*() starts doing so - `title` just always wins if given.
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


# ---- export_tables_to_excel: any number of huxtables -> one .xlsx file, ----
# ---- one sheet per table, plus - for 2+ tables only - an auto-generated, ---
# ---- hyperlinked Contents sheet (placed first) ------------------------------
# `titles`, given, are used for both each sheet's own title row (via
# add_huxtable_to_workbook()) AND the Contents sheet's listing - passing
# them once here, rather than needing to set huxtable::caption() on every
# table beforehand, matches how none of the current format_*() functions set
# captions (see add_huxtable_to_workbook()'s own note).
#
# The Contents sheet is only added when hux_list has more than one table. A
# table-of-contents that just points at the one sheet it's sitting next to is
# pure clutter, and it's the reason export_table_to_excel() (below) can be a
# genuine single-sheet wrapper: it delegates here with a length-1 list, and
# with this guard that produces a workbook with exactly the one sheet it
# named, not a second "Contents" tab alongside it.
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
    # ---- Contents sheet - one hyperlinked row per table, placed first ----
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

    # Contents sheet moved to the front - it's added last (has to be, since
    # its own hyperlinks reference sheets that must already exist), so its
    # position has to be fixed up afterwards rather than at creation.
    n_sheets <- length(sheet_names) + 1
    openxlsx::worksheetOrder(workbook) <- c(n_sheets, seq_len(n_sheets - 1))
  }

  openxlsx::saveWorkbook(workbook, file = file, overwrite = TRUE)
  invisible(workbook)
}


# ---- export_table_to_excel: convenience single-table wrapper ----
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
