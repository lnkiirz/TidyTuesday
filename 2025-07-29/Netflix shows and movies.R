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
  plot_annotation(
    tag_levels = list(c("2023", "2024", "2025")),
    title = "What did people watch on Netflix?",
    subtitle = "2023-2025, movies in blue, shows in red"
  )

ggsave(
  "Netflix Movies and Shows 2023-2025.jpg",
  plot = patch,
  height = 5,
  width = 15
)

views_by_year <-
  all_data |> 
  group_by(Year = year(release_date)) |> 
  summarize(
    total_views_millions = sum(views) / 1000000
  ) |> 
  mutate(
    views_as_pct_2010 = total_views_millions / total_views_millions[1]
  )

netflix_growth <-
  ggplot(
  data = views_by_year,
  aes(x = Year, y = total_views_millions, color = "seagreen4")
) +
  scale_color_manual(values = "seagreen4") + 
  geom_line() +
  theme(
    legend.position = "none"
  ) +
  ylab("Views (in millions)") +
  labs(
    title = "Netflix's Growth since 2010",
    subtitle = "Total millions of views per year"
  )

netflix_growth_relative_pct_2010 <-
  ggplot(
  data = views_by_year,
  aes(x = Year, y = views_as_pct_2010, color = "darkslategray3")
) +
  scale_color_manual(values = "darkslategray3") + 
  geom_line() +
  theme(
    legend.position = "none"
  ) +
  ylab("Percent change (relative to 2010)") +
  labs(
    title = "Netflix's Growth since 2010",
    subtitle = "Total percent difference since 2010 views - 250% increase in 2024"
  )

ggsave(
  "Netflix Views Growth 2010-2025.jpg",
  plot = netflix_growth,
  height = 5,
  width = 10
)

ggsave(
  "Netflix Growth relative to 2010.jpg",
  plot = netflix_growth_relative_pct_2010,
  height = 5,
  width = 10
)

top_5_shows_all_time <-
  all_data |> 
  filter(
    Type == "Show"
  ) |> 
  group_by(
    title,
    Type
  ) |> 
  summarize(
    total_views_millions = sum(views) / 1000000
  ) |> 
  arrange(desc(total_views_millions)) |> 
  head(5)

top_5_shows_hours <-
  all_data |> 
  filter(
    Type == "Show"
  ) |> 
  group_by(
    title
  ) |> 
  summarize(
    total_hours_millions = sum(hours_viewed) / 1000000
  ) |> 
  arrange(desc(total_hours_millions)) |> 
  head(5)

top_5_movies_all_time <-
  all_data |> 
  filter(
    Type == "Movie"
  ) |> 
  group_by(
    title,
    Type
  ) |> 
  summarize(
    total_views_millions = sum(views) / 1000000
  ) |> 
  arrange(desc(total_views_millions)) |> 
  head(5)