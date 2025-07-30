library(tidyverse)
library(krulRutils)
library(here)
library(GGally)
library(janitor)
library(patchwork)
library(broom)

pacemaker_tbl <- read_csv(here("data", "pacemaker.csv")) %>%
  drop_na() %>%
  clean_names() %>%
  rename(
    period = periodo_entre_pulsos,
    intensity = intensidad_de_pulso
  )

period_vector <- pacemaker_tbl %>%
  pull(period)

x_bar <- mean(period_vector)
s <- sd(period_vector)
n <- length(period_vector)
s_n <- s / sqrt(n)
df <- n - 1

period_density_tbl <- tibble(
  mu_data = seq(x_bar - 4 * s_n, x_bar + 4 * s_n, length.out = 1000)
) %>%
  mutate(
    mu_density_data = dlst(
      mu = mu_data,
      df = df,
      x_bar = x_bar,
      s_n = s_n
    )
  )

ci_lower <- x_bar - qt(0.975, df) * s_n
ci_upper <- x_bar + qt(0.975, df) * s_n



ci_heights <- with(period_density_tbl, {
  approx(x = mu_data, y = mu_density_data, xout = c(ci_lower, ci_upper))
})


period_density_ci_tbl <- period_density_tbl %>%
  filter(mu_data > ci_lower & mu_data < ci_upper)

period_density_ci_sig_tbl1 <- period_density_tbl %>%
  filter(mu_data < ci_lower)

period_density_ci_sig_tbl2 <- period_density_tbl %>%
  filter(mu_data > ci_upper)

ci_lines_tbl <- tibble(
  x = ci_heights$x,
  y = ci_heights$y
)


fill_levels <- c("Confidence Level = 1 - α", "Significance Level = α")


period_density_ci_tbl <- period_density_ci_tbl %>%
  mutate(fill_group = factor("Confidence Level = 1 - α", levels = fill_levels))

period_density_ci_sig_tbl1 <- period_density_ci_sig_tbl1 %>%
  mutate(fill_group = factor("Significance Level = α", levels = fill_levels))

period_density_ci_sig_tbl2 <- period_density_ci_sig_tbl2 %>%
  mutate(fill_group = factor("Significance Level = α", levels = fill_levels))



ci_lines_tbl <- ci_lines_tbl %>%
  mutate(label = c("CI Lower", "CI Upper")) # Adjust as needed

period_density_plot <- period_density_tbl %>%
  ggplot(aes(x = mu_data, y = mu_density_data)) +
  geom_segment(
    data = ci_lines_tbl,
    aes(x = x, y = 0, xend = x, yend = y),
    color = c_pal("C red"),
    linewidth = 0.8
  ) +
  geom_text(
    data = ci_lines_tbl,
    aes(x = x + (x - x_bar) * 0.3, y = y + 0.6, label = label),
    angle = 0,
    hjust = 0.5,
    vjust = 0.5,
    size = 4,
    color = c_pal("C red")
  ) +
  geom_label(
    data = tibble(x = c(0.92), y = c(12)),
    aes(
      x = x, y = y, label = paste0(
        "This diagram shows the\n",
        "probability density function\n",
        "for the possible expected value"
      )
    ),
    hjust = 0,
  ) +
  geom_line(
    color = c_pal("C blue"),
    linewidth = 1,
    alpha = 1
  ) +
  geom_area(
    data = period_density_ci_tbl,
    aes(x = mu_data, y = mu_density_data, fill = fill_group),
    alpha = 0.6
  ) +
  geom_area(
    data = period_density_ci_sig_tbl1,
    aes(x = mu_data, y = mu_density_data, fill = fill_group),
    alpha = 0.6
  ) +
  geom_area(
    data = period_density_ci_sig_tbl2,
    aes(x = mu_data, y = mu_density_data, fill = fill_group),
    alpha = 0.6
  ) +
  c_scale_fill("C azure", "C violet") +
  labs(
    title = paste0(
      "Probability Density Function for the\n",
      "Possible Expected Value"
    ),
    x = "Possible Expected Value",
    y = "Density",
    fill = "Interpretation of Areas"
  ) +
  theme_krul()
