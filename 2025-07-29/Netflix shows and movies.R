library(tidyverse)
library(patchwork)

movies <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/movies.csv') |> 
  mutate(
    Type = "Movie"
  )

shows <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/shows.csv') |> 
  mutate(
    Type = "Show"
  )

all_data <-
  bind_rows(
    movies,
    shows
  ) |> 
  mutate(
    views_millions = views / 1000000
  )

theme_set(theme_minimal())
options(scipen = 999)

top_movies_2025 <-
  ggplot(
  data = 
    all_data |> 
    filter(
      year(release_date) == 2025,
      Type == "Movie"
    ) |> 
    arrange(desc(views_millions)) |> 
    head(10),
  aes(x = fct_reorder(title, views_millions, .fun = max), y = views_millions, fill = "cadetblue1")
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = "cadetblue1") +
  theme(
    legend.position = "None",
    axis.title.x = element_blank()
  ) +
  xlab(element_blank()) +
  scale_y_continuous()

top_shows_2025 <-
  ggplot(
  data = 
    all_data |> 
    filter(
      year(release_date) == 2025,
      Type == "Show"
    ) |> 
    arrange(desc(views_millions)) |> 
    head(10),
  aes(x = fct_reorder(title, views_millions, .fun = max), y = views_millions, fill = "indianred1")
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = "indianred1") +
  theme(
    legend.position = "None",
    axis.title.x = element_blank()
  ) +
  xlab(element_blank()) +
  scale_y_continuous()

top_movies_2024 <-
  ggplot(
  data = 
    all_data |> 
    filter(
      year(release_date) == 2024,
      Type == "Movie"
    ) |> 
    arrange(desc(views_millions)) |> 
    head(10),
  aes(x = fct_reorder(title, views_millions, .fun = max), y = views_millions, fill = "cadetblue1")
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = "cadetblue1") +
  theme(
    legend.position = "None",
    axis.title.x = element_blank()
  ) +
  xlab(element_blank()) +
  scale_y_continuous()

top_shows_2024 <-
  ggplot(
  data = 
    all_data |> 
    filter(
      year(release_date) == 2024,
      Type == "Show"
    ) |> 
    arrange(desc(views_millions)) |> 
    head(10),
  # using fct_reorder instead of just reorder because reorder uses medians, not max values! this was causing top show here to be in 2nd spot
  aes(x = fct_reorder(title, views_millions, .fun = max), y = views_millions, fill = "indianred1")
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = "indianred1") +
  theme(
    legend.position = "None",
    axis.title.x = element_blank()
  ) +
  xlab(element_blank()) +
  scale_y_continuous()

top_movies_2023 <-
  ggplot(
  data = 
    all_data |> 
    filter(
      year(release_date) == 2023,
      Type == "Movie"
    ) |> 
    arrange(desc(views_millions)) |> 
    head(10),
  aes(x = fct_reorder(title, views_millions, .fun = max), y = views_millions, fill = "cadetblue1")
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = "cadetblue1") +
  theme(
    legend.position = "None",
    axis.title.x = element_blank()
  ) +
  xlab(element_blank()) +
  scale_y_continuous()

top_shows_2023 <-
  ggplot(
  data = 
    all_data |> 
    filter(
      year(release_date) == 2023,
      Type == "Show"
    ) |> 
    arrange(desc(views_millions)) |> 
    head(10),
  aes(x = fct_reorder(title, views_millions, .fun = max), y = views_millions, fill = "indianred1")
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = "indianred1") +
  theme(
    legend.position = "None",
    axis.title.x = element_blank()
  ) +
  xlab(element_blank()) +
  scale_y_continuous()

patch <-
  top_movies_2023 + top_movies_2024 + top_movies_2025 + 
  top_shows_2023 + top_shows_2024 + top_shows_2025 +
  plot_annotation(tag_levels = list(c("2023", "2024", "2025")))
