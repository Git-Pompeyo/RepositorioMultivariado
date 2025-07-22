# Testing script for chapter 3
# Load necessary libraries
library(tidyverse)
library(here)
library(krulRutils)
library(ISLR2)
library(patchwork)
options(scipen = 999) # Disable scientific notation for better readability



diamonds_density_plot <-
  diamonds %>%
  ggplot(aes(x = price, fill = cut)) +
  geom_density(
    alpha = 1,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("steelblue", "red", "orange", "green", "purple"),
  ) +
  facet_wrap(~cut, nrow = 1, scales = "free_y") + # facet by columns
  coord_cartesian(xlim = c(326, 18823)) +
  labs(
    title = "Density Plots",
    x = "Price (USD)",
    y = "Density",
    fill = ""
  ) +
  theme_krul() +
  theme(strip.background = element_blank(), strip.text = element_blank())

diamonds_histogram <- diamonds %>%
  ggplot(aes(x = price, fill = cut)) +
  geom_histogram(binwidth = 500, color = "black") +
  coord_cartesian(xlim = c(326, 18823)) +
  facet_wrap(~cut, nrow = 1, scales = "free_y") + # facet by columns
  scale_fill_manual(
    values = c("steelblue", "red", "orange", "green", "purple"),
  ) +
  labs(
    title = "Histograms",
    x = "Price (USD)",
    y = "Count",
    fill = "Cut"
  ) +
  theme_krul() +
  theme(strip.background = element_blank(), strip.text = element_blank())

diamonds_box_plot <- diamonds %>%
  ggplot(aes(x = price, y = 1, fill = cut)) +
  geom_boxplot(
    color = "black",
    outlier.shape = NA
  ) +
  geom_jitter(height = 0.3, alpha = 0.05, color = "darkgray", size = 0.1) +
  facet_wrap(~cut, nrow = 1) +
  scale_fill_manual(
    values = c("steelblue", "red", "orange", "green", "purple"),
  ) +
  coord_cartesian(xlim = c(326, 18823)) +
  labs(
    title = "Box Plots",
    x = "Price (USD)",
    y = "",
    fill = "Cut"
  ) +
  theme_krul() +
  theme(strip.background = element_blank(), strip.text = element_blank()) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 0.5)

diamonds_density_plot <- diamonds_density_plot +
  theme(legend.position = "bottom")

diamonds_histogram <- diamonds_histogram +
  theme(legend.position = "none")

diamonds_box_plot <- diamonds_box_plot +
  theme(legend.position = "none")


# Now stack vertically (rows) using patchwork’s `/`
diamonds_combined_plot <- (
  diamonds_box_plot /
    diamonds_histogram /
    diamonds_density_plot
) +
  plot_layout(heights = c(1, 3, 3)) +
  plot_annotation(
    title = "Distribution of Diamond Prices by Cut",
    theme = theme(
      plot.title =
        element_text(
          hjust = 0.5,
          size = 24,
          face = "bold"
        )
    )
  )

ggsave(
  filename = here("images", "diamonds_combined_plot.png"),
  plot = diamonds_combined_plot,
  width = 16,
  height = 9,
  dpi = 300
)
