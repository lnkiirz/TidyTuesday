library(tidyverse)
library(broom)
library(effectsize)

# load data
answers <- 
  read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/answers.csv'
  )

color_ranks <- 
  read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/color_ranks.csv'
  )

users <- read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/users.csv'
)

# joining all data
full_data <-
  answers |>
  left_join(color_ranks, by = "rank", suffix = c("_answers", "_ranks")) |> 
  left_join(users, by = "user_id")

xx_vs_xy <-
  full_data |> 
  filter(!is.na(y_chromosome)) |> 
  group_by(y_chromosome) |> 
  summarize(
    Mean_rank = mean(rank)
  )

mdl <-
  t.test(rank ~ y_chromosome, data = full_data)

# tidy output
tidy_mdl <-
  tidy(mdl)

# effect size - extremely small at .02
effect_size <-
  cohens_d(rank ~ y_chromosome, data = full_data)
