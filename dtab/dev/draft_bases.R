# =============================================================================
# DRAFT: base descriptions for calc_stats().
#
# base_information() builds the `base` object you pass to calc_stats();
# add_base_descriptions() adds a base column per variable column (o_base,
# p_base1...) to calc_stats()'s results, just before multicodes.
#
# Changes from the package:
#   - The object's elements are named, not read by position.
#   - Only the bases are stored - no variable labels, as nothing needs them.
#   - Inputs are checked up front; specific_bases is optional.
#
# Not part of the package - source it after devtools::load_all() to try it.
# =============================================================================


# ---- base_information --------------------------------------------------------

base_information <- function(data, general_base = NULL, specific_bases = NULL) {

  # ---- validate inputs ----

  # Data: a data frame, or a survey design (its data frame is used).
  if (any(str_detect(class(data), "survey"))) {
    data <- data[["variables"]]
    message("data is a survey design; using its underlying data frame to check variable names.")
  } else if (!is.data.frame(data)) {
    stop("data must be a data frame or a survey design object.")
  }

  # Need at least one of the two.
  if (is.null(general_base) && is.null(specific_bases)) {
    stop("Give general_base, specific_bases, or both.")
  }

  # General base: optional. One piece of text.
  if (!is.null(general_base) &&
      (!is.character(general_base) || length(general_base) != 1 || is.na(general_base))) {
    stop("general_base must be a single character string.")
  }

  # Specific bases: optional. A named character vector, names in the data.
  if (!is.null(specific_bases)) {
    if (!is.character(specific_bases) || is.null(names(specific_bases)) ||
        any(names(specific_bases) == "") || any(is.na(specific_bases))) {
      stop("specific_bases must be a named character vector, ",
           "e.g. c(NatFrEst = \"Those asked about benefit fraud\").")
    }
    if (any(duplicated(names(specific_bases)))) {
      stop("specific_bases names a variable more than once.")
    }
    missing_vars <- setdiff(names(specific_bases), names(data))
    if (length(missing_vars) > 0) {
      stop("specific_bases names variables not in the data: ",
           paste(missing_vars, collapse = ", "), ".")
    }
  }

  # ---- build ----

  list(general_base   = general_base,     # e.g. "All respondents"
       specific_bases = specific_bases)   # e.g. c(NatFrEst = "Those asked about benefit fraud")
}


# ---- add_base_descriptions ---------------------------------------------------
# Adds a base column for each variable column: o_base for outcome,
# p_base1 for predictor1, and so on. Specific base if the variable has one,
# otherwise the general base.

add_base_descriptions <- function(tables, base_info) {

  # No base information given: nothing to add.
  if (is.null(base_info)) return(tables)

  # A single string: use it as the general base for every variable.
  if (is.character(base_info) && length(base_info) == 1 && !is.na(base_info)) {
    base_info <- list(general_base = base_info, specific_bases = NULL)
  }

  # Otherwise it must be the object base_information() makes.
  if (!is.list(base_info) || !"specific_bases" %in% names(base_info)) {
    stop("base must be a single character string or made with base_information().")
  }

  # Columns holding variable names, e.g. "outcome" "predictor1" "predictor2"
  vars_of_interest <- tables %>%
    select(outcome, starts_with("predictor")) %>%
    names()

  for (var_col in vars_of_interest) {

    # Name of the new column: outcome -> o_base, predictor1 -> p_base1, ...
    base_col <- if (var_col == "outcome") "o_base" else str_replace(var_col, "predictor", "p_base")

    # Variable names in this column, e.g. "NatFrEst", "RSex"
    variable_names <- tables[[var_col]]

    # Each variable's specific base, NA where it has none. With no
    # specific_bases at all, NULL[...] gives NULL, so fall back to all NA.
    specific <- unname(base_info$specific_bases[variable_names]) %||%
      rep(NA_character_, length(variable_names))

    # Their specific base if they have one, otherwise the general base (NA
    # if there's none). "Total" isn't a variable - it's the no-breakdown
    # rows - so it gets NA.
    tables[[base_col]] <- case_when(
      variable_names == "Total" ~ NA_character_,
      variable_names %in% names(base_info$specific_bases) ~ specific,
      TRUE ~ base_info$general_base %||% NA_character_
    )
  }

  tables
}


# ---- summarise_bases ---------------------------------------------------------
# Turns the base columns (o_base, p_base1...) into footnote lines, e.g.
#   "Those asked about benefit fraud: Estimate of fraud"
#   "All other variables: All respondents"
# Returns a character vector of lines, or NULL if there are no bases.
# See dev/explainers/Explainer summarise_bases.R for a worked example.

summarise_bases <- function(tables) {

  # Base columns present, e.g. "o_base" "p_base1" "p_base2"
  base_cols <- tables %>% select(any_of("o_base"), starts_with("p_base")) %>% names()
  if (length(base_cols) == 0) return(NULL)

  # Pair each base with its variable's label: o_base with o_lab, p_base1 with p_lab1...
  # "Total" rows aren't a variable, so leave them out.
  pairs <- map_df(base_cols, function(base_col) {
    lab_col <- if (base_col == "o_base") "o_lab" else str_replace(base_col, "p_base", "p_lab")
    tibble(label = tables[[lab_col]], base = tables[[base_col]])
  }) %>%
    filter(label != "Total") %>%
    distinct()

  # Some variables may have no base (no general base was given).
  some_without_base <- any(is.na(pairs$base))
  pairs <- pairs %>% filter(!is.na(base))
  if (nrow(pairs) == 0) return(NULL)

  # Every variable has the same base: one line.
  if (!some_without_base && n_distinct(pairs$base) == 1) return(paste0("Base: ", pairs$base[1]))

  # The most common base covers "all other variables" - but only if every
  # variable has a base, otherwise it would claim too much.
  most_common <- if (some_without_base) NA_character_ else
    pairs %>% count(base, sort = TRUE) %>% slice(1) %>% pull(base)

  # One line per other base, listing the variables it applies to.
  lines <- pairs %>%
    filter(is.na(most_common) | base != most_common) %>%
    group_by(base) %>%
    summarise(labels = paste(label, collapse = ", "), .groups = "drop") %>%
    mutate(line = paste0(base, ": ", labels)) %>%
    pull(line)

  if (!is.na(most_common)) lines <- c(lines, paste0("All other variables: ", most_common))
  lines
}
