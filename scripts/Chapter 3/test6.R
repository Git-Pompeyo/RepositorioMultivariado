# Testing script for chapter 3
# Load necessary libraries
library(tidyverse)
library(here)
library(krulRutils)
library(ISLR2)
library(patchwork)
library(GGally)

airquality_tbl <- airquality %>%
  as_tibble() %>%
  drop_na() %>%
  mutate(
    Month = factor(Month, levels = 1:12, labels = month.abb[1:12]),
    Day = factor(Day)
  )

data <- iris %>%
  as_tibble %>%
  select(Species, Sepal.Length)

dt <- data %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(
    mean = mean(Sepal.Length),
    lci = t.test(Sepal.Length, conf.level = 0.95)$conf.int[1],
    uci = t.test(Sepal.Length, conf.level = 0.95)$conf.int[2]
  )


my_plot <- dt %>%
  ggplot(aes(x = Species, y = mean, ymin = lci, ymax = uci)) +
  geom_point(
    color = c_pal("C red"),
    size = 3
  ) +
  geom_errorbar(
    width = 0.3,
    color = c_pal("C red"),
    linewidth = 1
  ) +
  labs(
    title = "Confidence Intervals test",
    x = "Species",
    y = " Sepal Lenght"
  ) +
  theme_krul()
