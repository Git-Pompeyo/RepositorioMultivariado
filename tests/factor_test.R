library(tidyverse)

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

  # Determine the name of the new factor column
  if (is.null(new_factor_col_name)) {
    new_factor_col_name_sym <- rlang::sym(
      paste0(rlang::as_label(lookup_label_col_sym), "_factor")
    )
  } else {
    new_factor_col_name_sym <- rlang::ensym(new_factor_col_name)
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




library(tidyverse)

my_data <- tibble(
  person_id = 1:5,
  gender_code = c("M", "F", "M", "X", "F"), # Added 'X' to test missing codes
  enrollment_status_code = c("FT", "PT", "FT", "NA", "PT")
)
print(my_data)

gender_lookup <- tibble(
  code = c("M", "F"),
  label = c("Male", "Female")
)

enrollment_lookup <- tibble(
  code = c("FT", "PT"),
  label = c("Full-Time", "Part-Time")
)

# Test 1: Basic usage, default factor levels and name
result1 <- convert_codes_to_factor(
  data_tbl = my_data,
  code_col = gender_code,
  lookup_tbl = gender_lookup,
  lookup_code_col = code,
  lookup_label_col = label
)
print(result1)
levels(result1$label_factor) # Checks the default name and levels

# Test 2: Custom factor name and explicit levels
result2 <- convert_codes_to_factor(
  data_tbl = my_data,
  code_col = enrollment_status_code,
  lookup_tbl = enrollment_lookup,
  lookup_code_col = code,
  lookup_label_col = label,
  factor_levels = c("Part-Time", "Full-Time"),
  new_factor_col_name = "enrollment_status_factor"
)
print(result2)
levels(result2$enrollment_status_factor)

# Test 3: Multiple factors in one go (requires chaining calls)
my_data_processed <- my_data %>%
  convert_codes_to_factor(
    code_col = gender_code,
    lookup_tbl = gender_lookup,
    lookup_code_col = code,
    lookup_label_col = label,
    new_factor_col_name = "gender_factor_nice"
  ) %>%
  convert_codes_to_factor(
    code_col = enrollment_status_code,
    lookup_tbl = enrollment_lookup,
    lookup_code_col = code,
    lookup_label_col = label,
    new_factor_col_name = "enrollment_status_factor_nice"
  )
print(my_data_processed)
