# Testing script for chapter 3
# Load necessary libraries
library("tidyverse")
library("here")
library("krulRutils")
library("ISLR2")


mean_mpg <- mean(mtcars$mpg)
sd_mpg <- sd(mtcars$mpg)


combined_plot <- ggplot(mtcars, aes(x = mpg)) +
  # Histogram: shows sample as-is
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 2,
    fill = "gray85",
    color = "white"
  ) +

  # Density plot: smoothed nonparametric estimate
  geom_density(color = "blue", size = 1.2, fill = "blue", alpha = 0.2) +

  # Normal distribution: parametric model estimate
  stat_function(
    fun = dnorm,
    args = list(mean = mean_mpg, sd = sd_mpg),
    color = "red",
    size = 1,
    linetype = "dashed"
  ) +

  # Aesthetics
  labs(
    title = "Empirical vs. Inferred Distribution of mpg",
    subtitle = "Histogram (gray), KDE (blue), Normal fit (red dashed)",
    x = "Miles per Gallon (mpg)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)


ggplot_box_legend <- function(family = "serif") {
  # Create data to use in the boxplot legend:
  set.seed(100)

  sample_df <- data.frame(
    parameter = "test",
    values = sample(500)
  )

  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350

  # Function to calculate important values:
  ggplot2_boxplot <- function(x) {
    quartiles <- as.numeric(quantile(x,
      probs = c(0.25, 0.5, 0.75)
    ))

    names(quartiles) <- c(
      "25th percentile",
      "50th percentile\n(median)",
      "75th percentile"
    )

    IQR <- diff(quartiles[c(1, 3)])

    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])

    upper_dots <- x[x > (quartiles[3] + 1.5 * IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5 * IQR)]

    return(list(
      "quartiles" = quartiles,
      "25th percentile" = as.numeric(quartiles[1]),
      "50th percentile\n(median)" = as.numeric(quartiles[2]),
      "75th percentile" = as.numeric(quartiles[3]),
      "IQR" = IQR,
      "upper_whisker" = upper_whisker,
      "lower_whisker" = lower_whisker,
      "upper_dots" = upper_dots,
      "lower_dots" = lower_dots
    ))
  }

  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)

  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults(
    "text",
    list(
      size = 3,
      hjust = 0,
      family = family
    )
  )
  # Labels don't inherit text:
  update_geom_defaults(
    "label",
    list(
      size = 3,
      hjust = 0,
      family = family
    )
  )

  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(
      data = sample_df,
      aes(x = parameter, y = values),
      geom = "errorbar", width = 0.3
    ) +
    geom_boxplot(
      data = sample_df,
      aes(x = parameter, y = values),
      width = 0.3, fill = "lightgrey"
    ) +
    geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
    geom_text(
      aes(
        x = 1.17, y = 950,
        label = "Number of values"
      ),
      fontface = "bold", vjust = 0.4
    ) +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(
      x = 2.3, xend = 2.3,
      y = ggplot_output[["25th percentile"]],
      yend = ggplot_output[["75th percentile"]]
    )) +
    geom_segment(aes(
      x = 1.2, xend = 2.3,
      y = ggplot_output[["25th percentile"]],
      yend = ggplot_output[["25th percentile"]]
    )) +
    geom_segment(aes(
      x = 1.2, xend = 2.3,
      y = ggplot_output[["75th percentile"]],
      yend = ggplot_output[["75th percentile"]]
    )) +
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),
      label = "Interquartile\nrange", fontface = "bold",
      vjust = 0.4
    ) +
    geom_text(
      aes(
        x = c(1.17, 1.17),
        y = c(
          ggplot_output[["upper_whisker"]],
          ggplot_output[["lower_whisker"]]
        ),
        label = c(
          "Largest value within 1.5 times\ninterquartile range above\n75th percentile",
          "Smallest value within 1.5 times\ninterquartile range below\n25th percentile"
        )
      ),
      fontface = "bold", vjust = 0.9
    ) +
    geom_text(
      aes(
        x = c(1.17),
        y = ggplot_output[["lower_dots"]],
        label = "Outside value"
      ),
      vjust = 0.5, fontface = "bold"
    ) +
    geom_text(
      aes(
        x = c(1.17),
        y = ggplot_output[["lower_dots"]],
        label = "-Value is >1.5 times and \n<3 times the interquartile range\nbeyond either end of the box"
      ),
      vjust = 1.3
    ) +
    geom_label(
      aes(
        x = 1.17, y = ggplot_output[["quartiles"]],
        label = names(ggplot_output[["quartiles"]])
      ),
      vjust = c(0.4, 0.85, 0.4),
      fill = "white", label.size = 0
    ) +
    ylab("") +
    xlab("") +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      aspect.ratio = 4 / 3,
      plot.title = element_text(hjust = 0.5, size = 10)
    ) +
    coord_cartesian(xlim = c(1.4, 3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")

  return(explain_plot)
}


# Histogram of diamond prices
hist_plot <- ggplot(diamonds, aes(x = price)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 500, fill = "steelblue", color = "white") +
  geom_density(color = "red", size = 1.2, fill = "orange", alpha = 0.2) +
  labs(title = "Histogram of Diamond Prices", x = NULL, y = "Count") +
  theme_krul()

# Horizontal boxplot of diamond prices
box_plot <- ggplot(diamonds, aes(x = price, y = 1)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  scale_y_continuous(NULL, breaks = NULL) + # Hide y-axis
  labs(x = "Price (USD)", y = NULL) +
  theme_krul() +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 0.5)

# Stack vertically, sharing x-axis
hist_box_plot <- (hist_plot / box_plot) +
  plot_layout(heights = c(3, 1))
