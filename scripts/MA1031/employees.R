library(tidyverse)
options(scipen = 999) # Disable scientific notation

employee_type_vector <- c(
  "Operating employee",
  "Trusted employee",
  "Area manager",
  "General manager"
)

employee_salary_vector <- c(7000, 25000, 60000, 100000)

employee_frequency_vector <- c(50L, 35L, 16L, 1L)

employee_salary_tbl <- tibble(
  employee_type = employee_type_vector,
  employee_salary = employee_salary_vector,
  employee_frequency = employee_frequency_vector
)

create_employee_tbl <- function(frequency_vector) {
  tibble(
    employee_type = factor(rep(employee_type_vector, frequency_vector),
                           levels = employee_type_vector),
    Salaries = factor(rep(employee_type_vector, frequency_vector),
                      levels = employee_type_vector,
                      labels = employee_salary_vector)
  )
}


plot_employee_tbl <- function(employee_tbl) {
  employee_tbl %>%
    ggplot(aes(x = Salaries, fill = employee_type)) +
    geom_bar(color = "black") +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
    scale_fill_manual(values = c(
      "#1f77b4",
      "#ff7f0e",
      "#2ca02c",
      "#d62728"
    )) +
    labs(
      title = "Employee Bar Plot",
      x = "Employee Salaries",
      y = "Frequency",
      fill = "Employee Types"
    )
}

frequency_vector <- c(50, 35, 16, 1)
frequency_vector %>%
  create_employee_tbl() %>%
  plot_employee_tbl() %>%
  print()