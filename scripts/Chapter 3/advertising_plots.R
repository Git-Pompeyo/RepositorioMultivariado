# I want to make some experiments with cowplot here


# Load necessary libraries
library(tidyverse)
library("cowplot")
library("rlang")


# Load and clean data
advertising_tbl <- read_csv("data/Advertising.csv") %>%
  select(-1)

# This function creates a plot for each advertising variable against sales
make_advertising_plot <- function(var) {
  var_name <- enquo(var) |>
    as_name()
  advertising_tbl %>%
    mutate(
      fitted = fitted(lm(reformulate(var_name, response = "sales"), data = .))
    ) %>%
    ggplot(aes(x = {{ var }}, y = sales)) +
    geom_smooth(method = "lm", se = TRUE, color = "#1f77b4") +
    geom_segment(
      aes(
        xend = {{ var }},
        yend = fitted
      ),
      color = "#1f77b4",
      alpha = 1
    ) +
    geom_point(color = "#d62728", size = 1, alpha = 1) +
    labs(
      title = paste(str_to_title(var_name), "Advertising vs Sales"),
      x = paste(str_to_title(var_name), "Advertising Budget"),
      y = "Sales"
    ) +
    theme(
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_line(color = "gray80")
    )
}


# Create individual plots for each advertising variable
advertising_plots <- list(
  make_advertising_plot(TV),
  make_advertising_plot(radio),
  make_advertising_plot(newspaper)
)

# Combine the individual plots into a grid layout
main_advertising_plot <- plot_grid(plotlist = advertising_plots, ncol = 3)

# Final plot with title
final_plot <- (ggdraw() +
  draw_label(
    "Comparative Analysis of Advertising Investment on
    Tv, Radio or Newspaper",
    fontface = "bold",
    x = 0.5,
    y = .93,
    hjust = 0.5,
    size = 18
  ) +
  draw_plot(main_advertising_plot, y = 0, height = 0.82)) %>%
  print()

# Save the final plot
ggsave(
  "images/advertising_plots.png",
  plot = final_plot,
  width = 12,
  height = 4,
  dpi = 300
)
