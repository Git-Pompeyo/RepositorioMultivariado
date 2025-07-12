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
