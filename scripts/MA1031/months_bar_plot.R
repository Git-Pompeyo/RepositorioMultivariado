# Load libraries
library(tidyverse)
library(krulRutils)

# Lookup table
month_lookup_tbl <- tibble(
  code = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Ago", "Sep", "Oct", "Nov", "Dec"),
  label = c("January", "February", "March", "April", "May", "June", 
            "July", "August", "September", "October", "November", "December")
)

# Example raw data (codes only)
data_tbl <- tibble(
  month_code = c("Jan", "Mar", "Feb", "Jan", "Feb", "Mar", "May"),
  bad_data = c(1, 2, 3, 4, 5, 6, 7), # Additional column for demonstration
  more_bad_data = c(10, 20, 30, 40, 50, 60, 70) # Another additional column for demonstration
)

data_labeled_tbl <- data_tbl %>%
  convert_codes_to_factor(
    code_col = month_code,
    lookup_tbl = month_lookup_tbl,
    lookup_code_col = code,
    lookup_label_col = label
  ) 

# Pre-count frequencies with .drop = FALSE to keep missing levels
plot_tbl_1 <- data_labeled_tbl %>%
  count(label, .drop = FALSE)

plot_tbl <- data_tbl %>%
  convert_codes_to_factor(
    code_col = month_code,
    lookup_tbl = month_lookup_tbl,
    lookup_code_col = code,
    lookup_label_col = label
  ) %>%
  count(label, .drop = FALSE)


# Define custom colors (optional)
month_colors <- c(
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a",
  "#66a61e", "#e6ab02", "#a6761d", "#666666",
  "#1f78b4", "#b2df8a", "#fb9a99", "#cab2d6"
)

# The canonical tidyverse bar plot
months_recorded_plot <- ggplot(plot_tbl, aes(x = label, y = n, fill = label)) +
  geom_col(color = "black") +
  scale_fill_manual(values = month_colors) +
  labs(
    x = "Months",
    y = "Frequency",
    title = "Canonical Tidyverse Bar Plot",
    fill = "Months"
  )