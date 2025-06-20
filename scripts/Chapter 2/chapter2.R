library("ISLR2")
library("tidyverse")
library("krulRutils")
# Load the dataset
data("Auto")

# Convert the dataset to a tibble for easier manipulation
auto_data <- tibble(Auto)
cylinder_count <- auto_data %>% count(cylinders, .drop = FALSE)

cylinder_plot <- ggplot(
  cylinder_count,
  aes(x = cylinders, y = n, fill = cylinders)
) +
  geom_col(color = "black") +
  labs(
    x = "Number of Cylinders",
    y = "Frequency",
    title = "Cylinder Frequency plot for the Auto dataset",
    fill = "Number of Cylinders"
  )