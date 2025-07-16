# tests/testthat/test-convert-codes-to-factor.R

# Load necessary libraries
library(testthat)
library(dplyr)
library(tibble)
library(forcats) # Good practice to load if using forcat functions or expecting factors

# --- Source your function here if it's not already in an R package structure ---
# For a quick setup, you can place the function definition directly in this test file
# or source it from its location:
# source("R/convert_codes_to_factor.R") # Assuming your function is in an R folder

# The convert_codes_to_factor function (copied here for self-containment of this example)
convert_codes_to_factor <- function(
    data_tbl,
    code_col, # Column in data_tbl to match on (e.g., gender_code)
    lookup_tbl, # The lookup table (e.g., gender_lookup)
    lookup_code_col, # Column in lookup_tbl that holds the codes (e.g., code)
    lookup_label_col, # Column in lookup_tbl that holds the labels (e.g., label)
    factor_levels = NULL, # Optional: explicit order of factor levels (e.g., c("Male", "Female"))
    new_factor_col_name = NULL # Optional: Name for the new factor column (defaults to lookup_label_col name)
    ) {
  # Capture unquoted column names for tidy evaluation
  code_col_sym <- rlang::ensym(code_col)
  lookup_code_col_sym <- rlang::ensym(lookup_code_col)
  lookup_label_col_sym <- rlang::ensym(lookup_label_col)

  # Determine the name of the new factor column
  if (is.null(new_factor_col_name)) {
    new_factor_col_name_sym <- rlang::sym(paste0(rlang::as_label(lookup_label_col_sym), "_factor"))
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
  # Using `capture_warnings` in the testthat block will catch this
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
    mutate(!!new_factor_col_name_sym := factor(!!lookup_label_col_sym, levels = factor_levels))

  # Now, select the desired columns: original columns + the new factor column.
  final_tbl <- final_tbl %>%
    select(
      all_of(original_col_names), # Selects all columns that were in the original data_tbl
      !!new_factor_col_name_sym # Add the newly created factor column
    )

  return(final_tbl)
}


# --- Define Test Data ---
my_data <- tibble(
  person_id = 1:5,
  gender_code = c("M", "F", "M", "X", "F"),
  enrollment_status_code = c("FT", "PT", "FT", "NA", "PT")
)

gender_lookup <- tibble(
  code = c("M", "F"),
  label = c("Male", "Female")
)

enrollment_lookup <- tibble(
  code = c("FT", "PT"),
  label = c("Full-Time", "Part-Time")
)

# --- Test Blocks ---

test_that("convert_codes_to_factor correctly converts gender codes to factors", {
  # Call the function
  result <- convert_codes_to_factor(
    data_tbl = my_data,
    code_col = gender_code,
    lookup_tbl = gender_lookup,
    lookup_code_col = code,
    lookup_label_col = label
  )

  # 1. Check if the output is a tibble
  expect_s3_class(result, "tbl_df")

  # 2. Check if the new column exists and has the expected default name
  expect_true("label_factor" %in% names(result))

  # 3. Check if the new column is a factor
  expect_true(is.factor(result$label_factor))

  # 4. Check the levels of the factor
  expect_equal(levels(result$label_factor), c("Male", "Female"))

  # 5. Check the values of the factor for known good cases
  expect_equal(result$label_factor[1], factor("Male", levels = c("Male", "Female")))
  expect_equal(result$label_factor[2], factor("Female", levels = c("Male", "Female")))

  # 6. Check that unrecognized codes become NA
  expect_true(is.na(result$label_factor[4])) # 'X' should become NA

  # 7. Check that original columns are preserved
  expect_true("person_id" %in% names(result))
  expect_true("gender_code" %in% names(result))
  expect_true("enrollment_status_code" %in% names(result))
  expect_equal(result$person_id, my_data$person_id)
})

test_that("convert_codes_to_factor handles custom column names and factor levels", {
  result <- convert_codes_to_factor(
    data_tbl = my_data,
    code_col = enrollment_status_code,
    lookup_tbl = enrollment_lookup,
    lookup_code_col = code,
    lookup_label_col = label,
    factor_levels = c("Part-Time", "Full-Time"), # Custom order
    new_factor_col_name = "enrollment_status_factor" # Custom name
  )

  expect_s3_class(result, "tbl_df")
  expect_true("enrollment_status_factor" %in% names(result))
  expect_true(is.factor(result$enrollment_status_factor))
  expect_equal(levels(result$enrollment_status_factor), c("Part-Time", "Full-Time"))
  expect_equal(result$enrollment_status_factor[1], factor("Full-Time", levels = c("Part-Time", "Full-Time")))
  expect_equal(result$enrollment_status_factor[2], factor("Part-Time", levels = c("Part-Time", "Full-Time")))
  expect_true(is.na(result$enrollment_status_factor[4])) # 'NA' code should be NA
})


test_that("convert_codes_to_factor issues warnings for missing codes", {
  # For this test, we expect a warning. testthat has a special helper for this.
  expect_warning(
    {
      convert_codes_to_factor(
        data_tbl = my_data,
        code_col = gender_code,
        lookup_tbl = gender_lookup,
        lookup_code_col = code,
        lookup_label_col = label
      )
    },
    "The following codes were not found in the lookup table.*X"
  )

  expect_warning(
    {
      convert_codes_to_factor(
        data_tbl = my_data,
        code_col = enrollment_status_code,
        lookup_tbl = enrollment_lookup,
        lookup_code_col = code,
        lookup_label_col = label
      )
    },
    "The following codes were not found in the lookup table.*NA"
  )
})

test_that("convert_codes_to_factor handles multiple calls in a pipe", {
  result <- my_data %>%
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

  expect_true("gender_factor_nice" %in% names(result))
  expect_true("enrollment_status_factor_nice" %in% names(result))
  expect_true(is.factor(result$gender_factor_nice))
  expect_true(is.factor(result$enrollment_status_factor_nice))
  expect_equal(levels(result$gender_factor_nice), c("Male", "Female"))
  expect_equal(levels(result$enrollment_status_factor_nice), c("Full-Time", "Part-Time")) # Default order
  expect_s3_class(result, "tbl_df")
})
