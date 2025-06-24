library("ISLR2")
library("tidyverse")
library("krulRutils")

# Load the dataset
data("Auto")


# Define custom colors for the cylinders
cylinder_colors <- c(
  "4" = "#1b9e77",
  "5" = "#d95f02",
  "6" = "#7570b3",
  "8" = "#e7298a"
)

# Convert the dataset to a tibble for easier manipulation

cylinder_count <- Auto %>%
  as_tibble() %>%
  count(cylinders, .drop = FALSE) %>%
  mutate(cylinders = factor(cylinders))


cylinder_plot <- ggplot(
  cylinder_count,
  aes(x = cylinders, y = n, fill = cylinders, label = n)
) +
  geom_col(color = "black") +
  scale_fill_manual(values = cylinder_colors) +
  geom_text(
    vjust = -0.5,
    color = "black",
    size = 5
  ) +
  labs(
    x = "Number of Cylinders",
    y = "Frequency",
    title = "Cylinder Frequency Plot for the Auto dataset",
    fill = "Number of Cylinders"
  )