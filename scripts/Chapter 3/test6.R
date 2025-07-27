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
