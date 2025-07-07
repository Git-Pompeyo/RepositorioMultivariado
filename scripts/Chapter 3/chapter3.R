# Chapter 3: Simple Linear Regression
# Load necessary libraries
library("tidyverse")

# Here I'm testing the tidyverse to simply create a plot from the "Advertising" dataset
# that shows the relationship between TV advertising budget and sales. I do it in one 
# line using the pipe operator (%>%) to chain together multiple operations.
# Notice the use or parentheses that are needed because I need to make sure that
# I'm constructing the whole ggplot object before printing it.
(
  read_csv("data/Advertising.csv") %>%
    mutate(fitted = fitted(lm(sales ~ TV, data = .))) %>%
    ggplot(aes(x = TV, y = sales)) +
    geom_segment(
      aes(x = TV, xend = TV, y = sales, yend = fitted),
      color = "steelblue",
      alpha = 1
    ) +
    geom_point(color = "#d62728", size = 2, alpha = 1) +
    labs(
      title = "TV Advertising vs Sales",
      x = "TV Advertising budget",
      y = "Sales"
    ) +
    geom_smooth(method = "lm", se = TRUE, color = "blue")
) %>%
  print()
