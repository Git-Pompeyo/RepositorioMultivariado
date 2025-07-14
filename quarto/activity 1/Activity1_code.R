#| output: false
library(tidyverse)
library(krulRutils)
options(scipen = 999) # Disable scientific notation


# Employee codes and types
employee_code_vector <- c("oper_emp", "trust_emp", "area_mgr", "gen_mgr")

employee_type_vector <- c(
  "Operating Employee",
  "Trusted Employee",
  "Area Manager",
  "General Manager"
)

# Employee lookup tibble
employee_lookup_tbl <- tibble(
  employee_code = employee_code_vector,
  employee_type = employee_type_vector
)





# Employee salary data
employee_salary_vector <- c(7000, 25000, 60000, 100000)

# Employee frequency vector
employee_frequency_vector <- c(50, 35, 16, 1)

# Employee salary tibble
employee_salary_tbl <- tibble(
  employee_code = rep(employee_code_vector, employee_frequency_vector),
  employee_salary = rep(employee_salary_vector, employee_frequency_vector)
)




salaries <- employee_salary_tbl$employee_salary
salaries_mean <- mean(salaries)
salaries_median <- median(salaries)
salaries_range <- range(salaries)




mode <- function(x) {
  as.numeric(names(which.max(table(x))))
}
salaries_mode <- mode(salaries)




salaries_sd <- sd(salaries)
salaries_var <- var(salaries)




salaries_within_sd <- salaries[abs(salaries - salaries_mean) <= salaries_sd]



percentage_within_sd <- (length(salaries_within_sd) / length(salaries)) * 100



#Employee salary colors
employee_salary_colors <- c(
  "#1f77b4", # Operating employee
  "#2ca02c", # Trusted employee
  "#ff7f0e", # Area manager
  "#d62728"  # General manager
)



employee_salary_freq_tbl <- employee_salary_tbl %>%
  count(employee_code, employee_salary, name = "frequency", .drop = FALSE) %>%
  arrange(employee_salary) %>%
  convert_codes_to_factor(
    code_col = employee_code,
    lookup_tbl = employee_lookup_tbl,
    lookup_code_col = employee_code,
    lookup_label_col = employee_type,
    label_col = employee_type
  ) %>%
  mutate(employee_salary = factor(
    employee_salary,
    levels = employee_salary_vector,
  )) %>%
  select(-employee_code)



# Plot employee salary frequency
employee_salary_freq_plot <- ggplot(
  employee_salary_freq_tbl,
  aes(
    x = employee_type,
    y = frequency,
    fill = employee_salary,
    label = frequency
  )
) +
  geom_col(
    position = position_dodge(width = 0.9)
  ) +
  geom_text(
    stat = "identity",
    vjust = -0.5,
    position = position_dodge(width = 0.9)
  ) +
  scale_fill_manual(values = employee_salary_colors) +
  labs(
    title = "Employee Salary Frequency Plot",
    x = "Employee Types",
    y = "Frequency",
    fill = "Employee Salaries"
  ) +
  theme_krul()


















#| fig-width: 8
#| fig-height: 6

employee_salary_freq_plot
