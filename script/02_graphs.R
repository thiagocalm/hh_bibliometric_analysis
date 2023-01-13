
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(ggridges)
library(formattable)


# Importing datas ---------------------------------------------------------

articles_df <- read_rds("./data/dataset/articles_df.rds")

head(articles_df)


# Hadling datas -----------------------------------------------------------

articles_df_mod <- articles_df |>
  janitor::clean_names() |>
  mutate(across(c(title, authors, infos, kw, abstract), str_to_lower))

articles_df_mod_authors <- articles_df_mod |>
  separate(authors, c("author1","author2","author3","author4"),",") |>
  pivot_longer(cols = "author1":"author4", names_to = "authors", values_to = "authors_name")


# Some viz ----------------------------------------------------------------

# Number of articles by year

articles_df_mod |>
  mutate(month = lubridate::month(date)) |>
  ggplot() +
  aes(x = year) +
  geom_histogram(fill = "#00AFBA", color = "#969696") +
  scale_x_continuous(breaks = seq(2011L,2022L,1L)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  theme_minimal() +
  labs(
    title = "Number of articles with 'household headhsip' by year (2011-2022)",
    subtitle = "Published on Demography",
    y = "N",
    x = "Year",
    caption = "By @thiagocalm"
  ) +
  theme(
    plot.title = element_text(family = "Calibri",face = "bold", size = 12,
                              color = "#636363", hjust = .1),
    plot.subtitle = element_text(family = "Calibri",face = "bold", size = 7,
                                 color = "#969696", hjust = 1),
    axis.title.y = element_text(family = "Calibri",face = "bold", size = 9,
                              color = "#636363", hjust = 0),
    axis.title.x = element_text(family = "Calibri",face = "bold", size = 9,
                                color = "#636363", hjust = 0),
    axis.text = element_text(family = "Calibri", size = 8,
                             color = "#636363"),
    plot.caption = element_text(family = "Calibri",face = "bold", size = 7,
                                color = "#969696", hjust = 1)
  )

# TOP 10 authors

top_authors <- articles_df_mod_authors |>
  na.omit() |>
  group_by(authors_name) |>
  count() |>
  arrange(desc(n))

top_authors[1:10,] |>
  select(`TOP 10 Authors` = authors_name, `Number of articles` = n) |>
  formattable(list(`Number of articles` = color_bar("#00AFBA")),
              align =c("c","c")
              )
