# Testing script for chapter 3
# Load necessary libraries
library("tidyverse")
library("here")
library("cowplot")

# Load the Advertising dataset

advertising_tbl <- read_csv(here("data", "Advertising.csv"))
fragile_advertising_tbl <- read_csv("data/Advertising.csv")
