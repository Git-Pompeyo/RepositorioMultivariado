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
#' @param factor_levels Optional character vector specifying the levels order for the output factor. Defaults to all labels in `lookup_tbl`, in order.
#' 
#' @return A factor vector with labels corresponding to codes, same length/order as `data_tbl`.
#'         Codes not found in the lookup table result in NA and trigger a warning.
#' @export
convert_codes_to_factor <- function(data_tbl,
                                    code_col,
                                    lookup_tbl,
                                    lookup_code_col,
                                    lookup_label_col,
                                    factor_levels = NULL,
                                    label_col = label) {
  # Join on the code columns
  joined_tbl <- data_tbl %>%
    left_join(
      lookup_tbl,
      by = setNames(
        rlang::as_label(rlang::ensym(lookup_code_col)),
        rlang::as_label(rlang::ensym(code_col))
      )
    )

  # Extract label vector
  labels_vec <- joined_tbl %>% pull({{ lookup_label_col }})

  # Warn about missing codes
  missing_codes <- joined_tbl %>%
    filter(is.na({{ lookup_label_col }})) %>%
    distinct({{ code_col }}) %>%
    pull({{ code_col }})

  if (length(missing_codes) > 0) {
    warning("The following codes were not found in the lookup table and will be set to NA in the factor: ",
            paste(unique(missing_codes), collapse = ", "))
  }

  # Default factor levels
  if (is.null(factor_levels)) {
    factor_levels <- lookup_tbl %>% pull({{ lookup_label_col }}) %>% unique()
  }

  # Add labeled factor column
  data_tbl %>%
    mutate({{ label_col }} := factor(labels_vec, levels = factor_levels))
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
  
  num_cols <- df %>% select(where(is.numeric)) %>% names()
  
  if (length(num_cols) == 0) {
    stop("No numeric columns found in input.")
  }
  
  # Define summary functions with safe wrappers
  safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = na.rm)
  safe_q1 <- function(x) if (all(is.na(x))) NA_real_ else quantile(x, 0.25, na.rm = na.rm)
  safe_median <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = na.rm)
  safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = na.rm)
  safe_q3 <- function(x) if (all(is.na(x))) NA_real_ else quantile(x, 0.75, na.rm = na.rm)
  safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = na.rm)
  
  # Compute summaries wide, then reshape long and wide as requested
  df %>%
    summarise(
      across(
        all_of(num_cols),
        list(
          min = safe_min,
          q1 = safe_q1,
          median = safe_median,
          mean = safe_mean,
          q3 = safe_q3,
          max = safe_max
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
      statistic = factor(statistic, levels = c("min", "q1", "median", "mean", "q3", "max"))
    ) %>%
    arrange(statistic)
}
