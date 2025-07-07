linters <- lintr::linters_with_defaults(
  object_usage_linter = NULL
)

excluded_files <- function(path) {
  grepl("\\.otter\\.", path)
}
