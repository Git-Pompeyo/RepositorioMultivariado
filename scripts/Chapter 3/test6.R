# Testing script for chapter 3
# Load necessary libraries
library(tidyverse)
library(here)
library(krulRutils)
library(ISLR2)
library(patchwork)
library(GGally)
library(janitor)

pacemaker_tbl <- read_csv(here("data", "pacemaker.csv")) %>%
  drop_na() %>%
  clean_names()

pacemaker_lookup_tbl <- tibble(
  code = c("Sin MP", "Con MP"),
  label = c("With Pacemaker", "Without Pacemaker")
)

pacemaker_factor <- pacemaker_tbl %>%
  convert_codes_to_factor(
    code_col = marcapasos,
    lookup_tbl = pacemaker_lookup_tbl,
    lookup_code_col = code,
    lookup_label_col = label
  )

pacemaker_period_ci_tbl <- pacemaker_factor %>%
  group_by(marcapasos_factor) %>%
  summarise(
    x_bar = mean(periodo_entre_pulsos),
    lci = t.test(periodo_entre_pulsos, conf.level = 0.95)$conf.int[1],
    uci = t.test(periodo_entre_pulsos, conf.level = 0.95)$conf.int[2]
  ) %>%
  rename(
    pacemaker = marcapasos_factor
  )

pacemaker_period_ci_plot <- pacemaker_period_ci_tbl %>%
  ggplot(aes(x = x_bar, y = pacemaker, xmin = lci, xmax = uci)) +
  geom_errorbar(
    width = 0.1,
    color = c_pal("C blue"),
    linewidth = 1
  ) +
  geom_point(
    color = c_pal("C red"),
    size = 3,
    alpha = 1
  ) +
  labs(
    title = paste0(
      "Confidence intervals for the period between pulses with\n",
      "a confidence level of 0.95"
    ),
    x = "Period between pulses",
    y = ""
  ) +
  theme_krul()




airquality_tbl <- airquality %>%
  as_tibble() %>%
  drop_na() %>%
  mutate(
    Month = factor(Month, levels = 1:12, labels = month.abb[1:12]),
    Day = factor(Day)
  )

data <- iris %>%
  as_tibble() %>%
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
