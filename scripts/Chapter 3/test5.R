# Testing script for chapter 3
# Load necessary libraries
library(tidyverse)
library(here)
library(krulRutils)
library(ISLR2)
library(patchwork)
library(GGally)


library(tidyverse)

# Clean the airquality dataset
data(airquality)
airquality_tbl <- airquality %>%
  as_tibble() %>%
  drop_na() %>%
  mutate(
    Month = factor(Month, levels = 1:12, labels = month.abb[1:12]),
    Day = factor(Day)
  )


# Basic scatter plot of Temperature vs Ozone
airquality_plot <- airquality_tbl %>%
  ggplot(aes(x = Temp, y = Ozone)) +
  facet_wrap(~Month, ncol = 3) +
  geom_point(color = c_pal("C red"), size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = c_pal("C blue")) +
  labs(
    title = "Ozone vs Temperature",
    x = "Temperature (F)",
    y = "Ozone (ppb)"
  ) +
  theme_krul()

# Add color and shape aesthetics for Wind and Month
multi_airquality_plot <- airquality_tbl %>%
  ggplot(aes(x = Temp, y = Ozone, color = Wind, shape = Month)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_viridis_c(option = "F") +
  labs(
    title = "Ozone vs Temperature by Wind and Month",
    color = "Wind Speed",
    shape = "Month"
  ) +
  theme_krul()

# Pairwise scatter plot matrix with GGally

airquality_pairs_plot <- airquality_tbl %>%
  select(Ozone, Solar.R, Wind, Temp) %>%
  drop_na() %>%
  ggpairs()

n <- length(airquality_pairs_plot$xAxisLabels)

for (i in 1:n) {
  for (j in 1:i) {
    airquality_pairs_plot[i, j] <- airquality_pairs_plot[i, j] +
      theme_krul()
  }
}

# Create a date column (year is assumed 1973)
airquality_temp_ts_tbl <- airquality %>%
  as_tibble() %>%
  mutate(Date = as.Date(paste(1973, Month, Day, sep = "-"))) %>%
  drop_na(Temp) %>%
  select(Date, Temp, Ozone)

# Plot
airquality_temp_ts_plot <- airquality_temp_ts_tbl %>%
  ggplot(aes(x = Date, y = Temp, color = Ozone)) +
  geom_line() +
  labs(
    title = "Daily Temperature in New York (1973)",
    x = "Date",
    y = "Temperature (F)",
    color = "Ozone (ppb)"
  ) +
  theme_krul()
