# Testing script for chapter 3
# Load necessary libraries
library("tidyverse")
library("here")
library("cowplot")
library("krulRutils")
library("ISLR2")

# Load the Advertising dataset

advertising_tbl <- read_csv(here("data", "Advertising.csv")) %>%
  select(-1)

who_tidy <- who %>%
  pivot_longer(
    cols = starts_with("new"),
    names_to = "key",
    values_to = "cases",
    values_drop_na = TRUE
  ) %>%
  mutate(
    key = if_else(
      startsWith(key, "newrel"),
      sub("newrel", "new_rel", key),
      key
    )
  ) %>%
  separate(key, into = c("new", "type", "sexage"), sep = "_") %>%
  separate(sexage, into = c("sex", "age"), sep = 1) %>%
  select(-new) %>%
  mutate(
    sex = recode(sex, f = "female", m = "male"),
    age = case_when(
      age == "014" ~ "0-14",
      age == "1524" ~ "15-24",
      age == "2534" ~ "25-34",
      age == "3544" ~ "35-44",
      age == "4554" ~ "45-54",
      age == "5564" ~ "55-64",
      age == "65" ~ "65+",
      TRUE ~ age
    ),
    cases = as.integer(cases)
  ) %T>%
  saveRDS(here("data", "who_tidy.rds")) %>%
  write_csv(here("data", "who_tidy.csv")) %>%
  sample_n(100) %>%
  print(n = 100)
