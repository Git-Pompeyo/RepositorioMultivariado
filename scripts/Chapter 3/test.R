# Testing script for chapter 3
# Load necessary libraries
library("tidyverse")
library("here")
library("cowplot")
library("krulRutils")
library("ISLR2")
library("magrittr")

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
  write_csv(here("data", "who_tidy.csv"))

billboard_tidy <- billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "ranking",
    values_drop_na = TRUE
  ) %>%
  mutate(
    week = if_else(
      startsWith(week, "wk"),
      sub("wk", "", week),
      week
    )
  ) %>%
  mutate(
    week = as.integer(week),
    date = date.entered + weeks(week - 1)
  ) %>%
  arrange(date, ranking) %T>%
  saveRDS(here("data", "billboard_tidy.rds")) %>%
  write_csv(here("data", "billboard_tidy.csv"))

the_great_beyond_path <- billboard_tidy %>%
  filter(track == "The Great Beyond") %>%
  arrange(week)

great_beyond_plot <- ggplot(the_great_beyond_path, aes(x = date, y = ranking)) +
  geom_line(color = "#1f77b4", linewidth = 1) +
  geom_point(color = "#d62728", size = 2) +
  scale_x_date(date_labels = "%B %Y") +
  scale_y_reverse(breaks = seq(0, 100, 10)) + # rank 1 is at the top
  labs(
    title = "Chart Performance of 'The Great Beyond' by R.E.M.",
    x = "Date",
    y = "Billboard Hot 100 Ranking"
  ) +
  theme_krul()


pew <- tibble(
  religion = c(
    "Agnostic", "Atheist", "Buddhist", "Catholic", "Don’t know/refused",
    "Evangelical Prot", "Hindu", "Historically Black Prot", "Jehovah’s Witness",
    "Jewish", "Mainline Prot", "Mormon", "Muslim", "Orthodox",
    "Other Christian", "Other Faiths", "Other World Religions", "Unaffiliated"
  ),
  `$10k` = c(
    27, 12, 27, 418, 15, 575, 1, 228, 20, 19, 289, 29, 6, 13, 9, 20, 5, 217
  ),
  `$10–20k` = c(
    34, 27, 21, 617, 19, 869, 9, 244, 27, 19, 495, 40, 7, 17, 11, 33, 2, 299
  ),
  `$20–30k` = c(
    60, 37, 30, 732, 28, 1064, 7, 236, 24, 25, 655, 48, 9, 17, 10, 40, 3, 374
  ),
  `$30–40k` = c(
    81, 52, 34, 670, 24, 982, 9, 238, 24, 31, 651, 51, 8, 14, 10, 51, 1, 365
  )
)


pew_tidy <- pew %>%
  pivot_longer(
    cols = -religion,
    names_to = "income",
    values_to = "count",
    values_drop_na = TRUE
  ) %>%
  uncount(weights = count) %T>%
  saveRDS(here("data", "pew_tidy.rds")) %>%
  write_csv(here("data", "pew_tidy.csv"))

income_colors <- c(
  "$10k" = "#1b9e77",
  "$10–20k" = "#d95f02",
  "$20–30k" = "#7570b3",
  "$30–40k" = "#e7298a"
)

income_levels <- c(
  "$10k",
  "$10–20k",
  "$20–30k",
  "$30–40k"
)

pew_tidy <- pew_tidy %>%
  mutate(
    religion = if_else(
      startsWith(religion, "Historically Black Prot"),
      sub("Historically Black Prot", "Black Prot", religion),
      religion
    )
  )


pew_tidy <- pew_tidy %>%
  mutate(
    religion = if_else(
      startsWith(religion, "Other World Religions"),
      sub("Other World Religions", "Other Religions", religion),
      religion
    )
  )

pew_tidy <- pew_tidy %>%
  mutate(income = factor(income, levels = income_levels, ordered = TRUE))


pew_grouped <- pew_tidy %>%
  count(religion, income, name = "count")


pew_plot <- pew_grouped %>%
  ggplot(aes(x = religion, y = count, fill = income)) +
  geom_col() +
  scale_fill_manual(values = income_colors) +
  labs(
    title = "Distribution of Religions per Income bracket",
    x = "Religion",
    y = "count",
    fill = "Income Bracket"
  ) +
  theme_krul()
