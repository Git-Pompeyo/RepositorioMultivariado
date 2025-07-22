library(tidyverse)
library(devtools)

library(dplyr)

#' Convert codes in a tibble column to a labeled factor using a lookup table
#'
#' @param data_tbl A tibble containing the data with codes to convert.
#' @param code_col Unquoted name of the column in `data_tbl` with the codes.
#' @param lookup_tbl A tibble with code-label pairs for lookup.
#' @param lookup_code_col Unquoted name of the code column in `lookup_tbl`.
#' @param lookup_label_col Unquoted name of the label column in `lookup_tbl`.
#' @param factor_levels Optional character vector specifying the levels order
#' for the output factor. Defaults to all labels in `lookup_tbl`, in order.
#' @param new_factor_col_name Optional name for the new factor column.
#'
#' @return A tibble with a new factor vector with labels corresponding to codes,
#' same length/order as `data_tbl`.
#' Codes not found in the lookup table result in NA and trigger a warning.
#' @export
convert_codes_to_factor <- function(
    data_tbl,
    code_col, # Column in data_tbl to match on
    lookup_tbl, # The lookup table
    lookup_code_col, # Column in lookup_tbl that holds the codes
    lookup_label_col, # Column in lookup_tbl that holds the labels
    factor_levels = NULL, # Optional: explicit order of factor levels
    new_factor_col_name = NULL) {
  # Capture unquoted column names for tidy evaluation
  code_col_sym <- rlang::ensym(code_col)
  lookup_code_col_sym <- rlang::ensym(lookup_code_col)
  lookup_label_col_sym <- rlang::ensym(lookup_label_col)

  new_factor_col_name_input <- rlang::enquo(new_factor_col_name)
  if (rlang::quo_is_null(new_factor_col_name_input)) {
    new_factor_col_name_sym <- rlang::sym(
      paste0(rlang::as_label(code_col_sym), "_factor")
    )
  } else {
    new_factor_col_name_sym <- rlang::sym(
      rlang::as_label(new_factor_col_name_input)
    )
  }
  # Store original column names to ensure they are kept
  original_col_names <- names(data_tbl)

  # Perform the join to get the labels
  join_by_args <- stats::setNames(
    rlang::as_label(lookup_code_col_sym),
    rlang::as_label(code_col_sym)
  )

  joined_tbl <- data_tbl %>%
    left_join(
      lookup_tbl,
      by = join_by_args
    )

  # Check for missing codes and issue a warning
  missing_codes <- joined_tbl %>%
    filter(is.na(!!lookup_label_col_sym)) %>%
    distinct(!!code_col_sym) %>%
    pull(!!code_col_sym)

  if (length(missing_codes) > 0) {
    warning(
      "The following codes were not found in the lookup table ",
      "and will be set to NA in the factor: ",
      paste(unique(missing_codes), collapse = ", ")
    )
  }

  # Determine factor levels if not provided
  if (is.null(factor_levels)) {
    factor_levels <- lookup_tbl %>%
      pull(!!lookup_label_col_sym) %>%
      unique()
  }

  # Add the labeled factor column
  final_tbl <- joined_tbl %>%
    mutate(
      !!new_factor_col_name_sym := factor(
        !!lookup_label_col_sym,
        levels = factor_levels
      )
    )

  # Now, select the desired columns: original columns + the new factor column.
  # We need to explicitly pick the *original* columns that were in data_tbl,
  # and then add the new factor column.
  # The label column from the lookup_tbl that was joined is temporary.
  final_tbl <- final_tbl %>%
    select(
      all_of(original_col_names), # Selects all columns from the original data
      !!new_factor_col_name_sym # Add the newly created factor column
    )

  return(final_tbl)
}





#' Summarise numeric variables in a tidy format
#'
#' Computes min, quartiles, median, mean, and max for all numeric columns,
#' returning a tibble with statistics as rows and variables as columns.
#'
#' @param df A data frame or tibble.
#' @param na.rm Logical, whether to remove NA values. Default TRUE.
#'
#' @return A tibble with rows = statistics and columns = variables.
#' @export
summarise_numeric_tidy <- function(df, na.rm = TRUE) {
  # Check input
  if (!is.data.frame(df)) {
    stop("Input must be a data.frame or tibble.")
  }

  num_cols <- df %>%
    select(where(is.numeric)) %>%
    names()

  if (length(num_cols) == 0) {
    stop("No numeric columns found in input.")
  }

  # Define summary functions with safe wrappers

  # Function for finding the minimal value
  safe_min <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      min(x, na.rm = na.rm)
    }
  }

  # Function for finding the first quartile
  safe_q1 <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      quantile(x, 0.25, na.rm = na.rm)
    }
  }

  # Function for finding the median value
  safe_median <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      median(x, na.rm = na.rm)
    }
  }

  # Function for finding the third quartile
  safe_q3 <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      quantile(x, 0.75, na.rm = na.rm)
    }
  }

  # Function for finding the maximal value
  safe_max <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      max(x, na.rm = na.rm)
    }
  }

  # Function for finding the mean value
  safe_mean <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      mean(x, na.rm = na.rm)
    }
  }

  # Function for finding the mean value
  safe_var <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      var(x, na.rm = na.rm)
    }
  }


  # Compute summaries wide, then reshape long and wide as requested
  df %>%
    summarise(
      across(
        all_of(num_cols),
        list(
          min = safe_min,
          q1 = safe_q1,
          median = safe_median,
          q3 = safe_q3,
          max = safe_max,
          mean = safe_mean,
          var = safe_var
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to = c("variable", "statistic"),
      names_sep = "_",
      values_to = "value"
    ) %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    mutate(
      statistic = factor(
        statistic,
        levels = c("min", "q1", "median", "q3", "max", "mean", "var")
      )
    ) %>%
    arrange(statistic)
}


#' Modifies the color of the grid lines in ggplot2 plots
#' @export
theme_krul <- function() {
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray80")
  )
}
