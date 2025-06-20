library(tidyverse)

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
