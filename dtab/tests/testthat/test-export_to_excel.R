# =============================================================================
# Tests for export_to_excel.R
# I haven't run these myself - same caveat as every other test file in this
# project. Run with devtools::test() or testthat::test_file() on this file.
# Tests 1-2 cover detect_stem_columns() against REAL huxtables built through
# the actual pipeline (format_summary()/format_crosstab()/
# format_numeric_summary()), not hand-built ones - the whole point of this
# function is inferring the right answer from each format_*()'s own
# established left-alignment convention, so it needs to be checked against
# what those functions actually produce, not an idealised stand-in. Tests
# 3-5 cover the more mechanical helpers (longest_per_column(),
# column_spanners(), recommended_row_span()) against small hand-built
# huxtables, where exact expected values are easy to reason about directly.
# Tests 6-8 cover the actual Excel-writing functions (add_huxtable_to_
# workbook(), export_table_to_excel(), export_tables_to_excel()) - an
# earlier version of this file's header claimed these "aren't testable" at
# all, which was wrong and got called out directly: openxlsx round-trips
# through a real file just fine. Each test saves to a real tempfile(), reads
# it back with openxlsx::read.xlsx()/getSheetNames() (not huxtable - the
# point is confirming what actually landed in the .xlsx file, independent of
# how huxtable itself represents anything), and checks for expected text
# rather than exact cell coordinates - the exact row/col offset
# as_Workbook(start_row = 3, start_col = 2, ...) produces isn't something I
# can verify without R, so these check "does the expected content appear
# somewhere on the sheet" rather than pinning an exact address that might be
# off by a row or column. Column widths/row heights/styling aren't checked -
# openxlsx doesn't round-trip those reliably enough through a reload to make
# that a meaningful automated check; visual review is still the right tool
# for that part.
# Two things test 7 caught and fixed in export_to_excel.R itself, not just
# in this test file: (1) export_tables_to_excel() used to add a "Contents"
# sheet unconditionally, so export_table_to_excel() - despite being framed
# as a single-sheet convenience wrapper - always produced a 2-sheet file.
# Fixed by only adding Contents when hux_list has more than one table.
# (2) numeric-looking cell text (e.g. "35.0", format_statistic()'s 1dp
# rendering of a mean) doesn't round-trip through read.xlsx() as an exact
# string - it auto-coerces numeric-looking text back to a real number,
# dropping the trailing ".0" ("50" in test 6, with no decimal to lose, is
# unaffected). Tests 7-8 check via as.numeric() instead of an exact string
# match for that reason.
# =============================================================================


# Tests 6-8 save their .xlsx output here instead of a tempfile() - real files
# you can open and visually check (styling, widths, wrapping - the things
# these tests can't verify programmatically), not deleted on exit. Now that
# this lives in the package's own tests/testthat/ folder (rather than a
# plain script sourced from Scripts_new/), writing into a subfolder of
# tempdir() instead of the package source tree - message() prints the exact
# path so it's easy to find and open after a test run.
test_output_dir <- file.path(tempdir(), "dtab_test_output")
dir.create(test_output_dir, showWarnings = FALSE, recursive = TRUE)
message("export_to_excel test output written to: ", test_output_dir)

test_that("1. detect_stem_columns() finds 2 stem columns (Variable, Statistics) on a real format_crosstab() huxtable", {
  data <- tibble(
    region = factor(c("North", "North", "South", "South", "South", "North")),
    sex    = factor(c("Male", "Male", "Male", "Female", "Female", "Female"))
  )
  stats_table <- calc_stats(data, outcomes = "region", predictors = "sex",
                             statistics = "perc", multicode = FALSE)
  ht <- format_crosstab(pivot_crosstab(stats_table))

  expect_equal(detect_stem_columns(ht), 2L)
})

test_that("2. detect_stem_columns() finds 1 stem column (Variable only) on a real format_numeric_summary() huxtable", {
  data <- tibble(age = c(10, 20, 30, 40, 50, 60), income = c(100, 200, 300, 400, 500, 600))
  ht <- format_numeric_summary(pivot_numeric_summary(data, outcomes = c("age", "income"),
                                                       statistics = c("mean", "sd")))

  # format_numeric_summary() has no Statistics column at all (one row per
  # variable, one column per statistic instead) - Variable is the only
  # left-aligned column, so this should come back 1, not 2.
  expect_equal(detect_stem_columns(ht), 1L)
})

test_that("3. longest_per_column() applies the +3 padding / min-10 floor rule from the widest cell in each column", {
  # Deliberately no merged cells here - longest_per_column()'s masking of
  # merge-SHADOWED cells (rs > 0 & cs > 0, inherited unchanged from the
  # original Deprecated_Tables code) depends on exactly what huxtable's own
  # rowspan()/colspan() accessors report at a shadowed cell, which I can't
  # confirm without R (unlike merge_cells()'s "anchor copies its content
  # into the covered cell" behaviour, which IS confirmed - see huxtable's
  # own spans.Rd doc, cited elsewhere in this project - whether that copied
  # content then gets counted or excluded here specifically is a separate,
  # unverified question). Rather than assert on a guess, this test sticks to
  # the part that's unambiguous either way: the plain padding/floor
  # arithmetic on ordinary, unmerged cells.
  ht <- huxtable::hux(a = c("short", "a much longer piece of text here"),
                       b = c("x", "y"))

  widths <- longest_per_column(ht)

  # Column b: longest content is "x"/"y", both 1 char -> 1 + 3 = 4, floored
  # to the minimum of 10.
  expect_equal(widths[["b"]], 10L)

  # Column a: longest content is the 33-character string -> 33 + 3 = 36,
  # past the floor.
  expect_equal(widths[["a"]], stringr::str_length("a much longer piece of text here") + 3L)
})

test_that("4. column_spanners() zeroes every cell covered by a colspan except the anchor", {
  ht <- huxtable::hux(a = "x", b = "y", c = "z")
  ht <- huxtable::set_colspan(ht, 1, 1, 3)   # one cell spanning all 3 columns

  spans <- column_spanners(ht)

  expect_equal(spans[1, 1], 3)   # anchor keeps the real colspan value
  expect_equal(spans[1, 2], 0)   # covered cells zeroed, not left at 1
  expect_equal(spans[1, 3], 0)
})

test_that("5. recommended_row_span() recommends more than 1 row when content is wider than the column(s) it spans", {
  # huxtable::hux() adds a real header row of column names by DEFAULT -
  # unlike as_hux(), which only does that with add_colnames = TRUE - so
  # hux(a = "some string") is a 2-row table, not 1: row 1 is the literal
  # column name "a" (as real, addressable cell content, not just a display
  # label), row 2 is the actual string passed in. Confirmed via a real R
  # run after three separate "fixes" to recommended_row_span() itself all
  # produced the exact same failure - it turned out the function was never
  # broken, this test was asking for row 1 (the 1-character column name)
  # instead of row 2 (the real content), so content was always length 1
  # regardless of which cell-extraction method recommended_row_span() used
  # internally. row = 2 below, not row = 1, is the actual fix.
  ht <- huxtable::hux(a = "a very long header that will not fit on one line at all")
  spans <- column_spanners(ht)

  # A narrow column (width 10) against a long header string should recommend
  # wrapping across more than one row.
  narrow_span <- recommended_row_span(ht, column_widths = 10, column_spans = spans, row = 2)
  expect_true(narrow_span > 1)

  # The same content against a very wide column should fit on one row.
  wide_span <- recommended_row_span(ht, column_widths = 200, column_spans = spans, row = 2)
  expect_equal(wide_span, 1)
})

test_that("6. add_huxtable_to_workbook() writes a real huxtable into an openxlsx Workbook that round-trips through a saved file", {
  data <- tibble(
    region = factor(c("North", "North", "South", "South", "South", "North")),
    sex    = factor(c("Male", "Male", "Male", "Female", "Female", "Female"))
  )
  stats_table <- calc_stats(data, outcomes = "region", predictors = "sex",
                             statistics = "perc", multicode = FALSE)
  ht <- format_crosstab(pivot_crosstab(stats_table))

  wb <- openxlsx::createWorkbook()
  wb <- add_huxtable_to_workbook(ht, wb, sheet = "Table 1")

  file <- file.path(test_output_dir, "test6_crosstab.xlsx")
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  expect_true(file.exists(file))
  expect_true("Table 1" %in% openxlsx::getSheetNames(file))

  sheet_data <- openxlsx::read.xlsx(file, sheet = "Table 1", colNames = FALSE, skipEmptyRows = FALSE)
  all_cells  <- unlist(sheet_data)

  # Column names from the huxtable made it into the sheet somewhere -
  # checked by presence anywhere on the sheet, not an exact cell address,
  # since the precise row/col offset as_Workbook(start_row = 3, start_col =
  # 2, ...) produces isn't something I can verify without R.
  expect_true("Variable" %in% all_cells)
  expect_true("Statistics" %in% all_cells)

  # North's Total percentage (0.5 -> "50", same value already confirmed in
  # test_pivot_crosstab.R's own format_crosstab() tests) made it across too
  # - real evidence the underlying data, not just the table's shape, reached
  # the actual .xlsx file.
  expect_true("50" %in% all_cells)
})

test_that("7. export_table_to_excel() creates a real .xlsx file with the given sheet name and title", {
  data <- tibble(age = c(10, 20, 30, 40, 50, 60))
  ht <- format_summary(pivot_summary(calc_stats(data, outcomes = "age", statistics = "mean")))

  file <- file.path(test_output_dir, "test7_single_table.xlsx")

  export_table_to_excel(ht, file, sheet = "Age summary", title = "Age - summary statistics")

  expect_true(file.exists(file))
  expect_equal(openxlsx::getSheetNames(file), "Age summary")

  sheet_data <- openxlsx::read.xlsx(file, sheet = "Age summary", colNames = FALSE, skipEmptyRows = FALSE)
  all_cells  <- unlist(sheet_data)

  expect_true("Age - summary statistics" %in% all_cells)
  # Mean of 10..60 = 35 -> format_statistic() renders "35.0" (1dp) in the
  # huxtable itself, but that's a character string that LOOKS like a number,
  # and openxlsx::read.xlsx() auto-detects and coerces numeric-looking text
  # columns back to real numerics on read - which drops the trailing ".0" (as.
  # character(35) is "35", not "35.0"). Confirmed against test 6 above, where
  # "50" (no decimal to lose) round-tripped as an exact string match fine, but
  # this value doesn't survive the same way - so the check here goes through
  # as.numeric() instead of matching the exact display string. This is a
  # read-back artefact of read.xlsx()'s own type-guessing, not proof either
  # way about how the cell is actually typed inside the saved .xlsx file.
  expect_true(35 %in% suppressWarnings(as.numeric(all_cells)))
})

test_that("8. export_tables_to_excel() builds one sheet per table plus a Contents sheet, placed first", {
  data1 <- tibble(age = c(10, 20, 30, 40, 50, 60))
  ht1   <- format_summary(pivot_summary(calc_stats(data1, outcomes = "age", statistics = "mean")))

  data2 <- tibble(income = c(100, 200, 300))
  ht2   <- format_summary(pivot_summary(calc_stats(data2, outcomes = "income", statistics = "mean")))

  file <- file.path(test_output_dir, "test8_multi_table.xlsx")

  export_tables_to_excel(list(ht1, ht2), file,
                          sheet_names = c("Age", "Income"),
                          titles = c("Age summary", "Income summary"))

  expect_true(file.exists(file))
  sheet_names <- openxlsx::getSheetNames(file)

  # Contents sheet exists and comes first - the whole point of the
  # worksheetOrder() fix-up in export_tables_to_excel() (Contents has to be
  # added LAST, since its own hyperlinks reference sheets that must already
  # exist, so its position needs fixing up afterwards rather than at
  # creation - this is the check that fix-up actually worked).
  expect_equal(sheet_names[1], "Contents")
  expect_true(all(c("Age", "Income") %in% sheet_names))

  # Both tables' titles are listed on the Contents sheet - written via
  # writeData(), not the HYPERLINK() formula cell next to it (openxlsx
  # doesn't evaluate formulas on write, so a formula cell's cached display
  # value isn't reliably readable back this way - the plain-text title cell
  # is what's actually being checked here).
  contents_cells <- unlist(openxlsx::read.xlsx(file, sheet = "Contents", colNames = FALSE, skipEmptyRows = FALSE))
  expect_true("Age summary" %in% contents_cells)
  expect_true("Income summary" %in% contents_cells)

  # Each table's own sheet still has its own real content on it, not just
  # an empty shell.
  age_cells    <- unlist(openxlsx::read.xlsx(file, sheet = "Age", colNames = FALSE, skipEmptyRows = FALSE))
  income_cells <- unlist(openxlsx::read.xlsx(file, sheet = "Income", colNames = FALSE, skipEmptyRows = FALSE))
  # as.numeric(), not exact-string, per test 7's note above - read.xlsx()
  # coerces numeric-looking text cells back to real numbers, dropping the
  # ".0" that format_statistic() renders in the source huxtable.
  expect_true(35 %in% suppressWarnings(as.numeric(age_cells)))     # mean of 10..60
  expect_true(200 %in% suppressWarnings(as.numeric(income_cells))) # mean of 100,200,300
})
