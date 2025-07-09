library(tidyverse)
library(broom)
library(effectsize)
library(plotly)

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
  mutate(
    y_chromosome = factor(y_chromosome, labels = c("Female", "Male"))
  ) |> 
  group_by(y_chromosome) |> 
  summarize(
    Mean_rank = mean(rank),
    SD = sd(rank),
    SD_above_mean = Mean_rank + SD,
    SD_below_mean = Mean_rank - SD
  )

mdl <-
  t.test(rank ~ y_chromosome, data = full_data)

# tidy output
tidy_mdl <-
  tidy(mdl)

# effect size - extremely small at .02
effect_size <-
  cohens_d(rank ~ y_chromosome, data = full_data)

# summarize stat results and effect
summary_df <-
  tibble(
    "Mean Difference" = tidy_mdl$estimate,
    "Women's Mean" = tidy_mdl$estimate1,
    "Men's Mean" = tidy_mdl$estimate2,
    "T value" = tidy_mdl$statistic,
    "p value" = tidy_mdl$p.value,
    "Effect size (Cohen's d)" = effect_size$Cohens_d
  )

print(summary_df)

# column chart to compare results
comparison_graph <-
  ggplot(
  data = xx_vs_xy,
  aes(
    x = factor(y_chromosome),
    y = Mean_rank,
  fill = y_chromosome)
) +
  coord_cartesian(ylim = c(2.35, 2.52)) +
  geom_col() +
  labs(
    title = "Men and women don't score significantly different on color identification"
  ) +
  xlab("Biological Sex") +
  ylab("Average color rank") +
  theme(
    legend.position = "none"
  )

# make graph interactive
  interactive_graph <-
  ggplotly(comparison_graph)

print(interactive_graph)