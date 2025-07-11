library(tidyverse)
library(broom)
library(car)
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
  left_join(users, by = "user_id") |> 
  mutate(
    y_chromosome = factor(y_chromosome, labels = c("Female", "Male")),
    colorblind = factor(colorblind, labels = c("No", "Yes"))
  ) |> 
  group_by(user_id) |> 
  mutate(
    user_rank = mean(rank, na.rm = TRUE),
    user_accuracy = 1 / user_rank
  ) |> 
  ungroup()

xx_vs_xy <-
  full_data |> 
  filter(!is.na(y_chromosome), !is.na(colorblind)) |> 
  group_by(y_chromosome) |> 
  summarize(
    mean_rank = mean(user_rank, na.rm = TRUE),
    mean_accuracy = 1 / mean_rank,
    SD = sd(rank),
    SD_above_mean = mean_rank + SD, # was initially going to use for CIs, not needed
    SD_below_mean = mean_rank - SD # same as above
  )

# initial spot checking
# using rank
ggplot(
  data = full_data |> filter(!is.na(y_chromosome), !is.na(colorblind)),
  aes(x = user_rank, fill = y_chromosome)
) +
  geom_density() +
  facet_wrap(~ y_chromosome)

# using accuracy
ggplot(
  data = full_data |> filter(!is.na(y_chromosome), !is.na(colorblind)),
  aes(x = user_accuracy, fill = y_chromosome)
) +
  geom_density() +
  facet_wrap(~ y_chromosome)
# some outliers in both, unlikely to be an issue given sample size

# checking equal variances
leveneTest(rank ~ factor(y_chromosome), data = full_data)
# significant test, but unlikely to be an issue with this sample size

mdl_ttest <-
  t.test(rank ~ y_chromosome, data = full_data)

# tidy output
tidy_ttest <-
  tidy(mdl_ttest)

# effect size - extremely small at .02
effect_size <-
  cohens_d(rank ~ y_chromosome, data = full_data)

# summarize stat results and effect
summary_ttest <-
  tibble(
    "Mean Difference" = tidy_ttest$estimate,
    "Women's Mean" = tidy_ttest$estimate1,
    "Women's CI low" = tidy_ttest$estimate1 - tidy_ttest$conf.low,
    "Women's CI high" = tidy_ttest$estimate1 + tidy_ttest$conf.high,
    "Men's Mean" = tidy_ttest$estimate2,
    "Men's CI low" = tidy_ttest$estimate2 - tidy_ttest$conf.low,
    "Men's CI high" = tidy_ttest$estimate2 + tidy_ttest$conf.high,
    "T value" = tidy_ttest$statistic,
    "p value" = tidy_ttest$p.value,
    "Effect size (Cohen's d)" = effect_size$Cohens_d
  )

# column chart to compare results
comparison_graph <-
  ggplot(
  data = xx_vs_xy,
  aes(
    x = factor(y_chromosome),
    y = mean_rank,
  fill = y_chromosome)
) +
  coord_cartesian(ylim = c(2.35, 2.55)) +
  geom_col() +
  geom_errorbar(
    data = xx_vs_xy |> filter(y_chromosome == "Female"), 
    ymin = summary_ttest$`Women's CI low`, 
    ymax = summary_ttest$`Women's CI high`,
    width = .35
  ) +
  geom_errorbar(
    data = xx_vs_xy |> filter(y_chromosome == "Male"), 
    ymin = summary_ttest$`Men's CI low`, 
    ymax = summary_ttest$`Men's CI high`,
    width = .35
  ) +
  labs(
    title = "Mean Difference in Identifying Colors",
    subtitle = "Men and women don't score significantly different on color identification"
  ) +
  xlab("Biological Sex") +
  ylab("Average color rank") +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

# make graph interactive
  interactive_graph <-
  ggplotly(comparison_graph)

anova_gender_colorblind <-
  aov(rank ~ y_chromosome * colorblind, data = full_data)

anova_details <-
  summary(anova_gender_colorblind)

# anova effect size - once again minimal
anova_effect <-
  eta_squared(anova_gender_colorblind)

anova_summary <-
  tibble(
    "Gender F value" = anova_details[[1]]$`F value`[1],
    "Gender p value" = anova_details[[1]]$`Pr(>F)`[1],
    "Gender effect size" = anova_effect$Eta2_partial[1],
    "Colorblindness F value" = anova_details[[1]]$`F value`[2],
    "Colorblindness p value" = anova_details[[1]]$`Pr(>F)`[2],
    "Colorblindness effect size" = anova_effect$Eta2_partial[2],
    "Gender x colorblindness F value" = anova_details[[1]]$`F value`[3],
    "Gender x colorblindness p value" = anova_details[[1]]$`Pr(>F)`[3],
    "Gender x colorblindness effect size" = anova_effect$Eta2_partial[3]
  )

# all combinations significant except one
Tukey_comps <-
  TukeyHSD(anova_gender_colorblind)

# plot to compare group differences
Tukey_plot <-
  plot(Tukey_comps)

final_results <- 
  list(
    "T test" = tidy_ttest,
    "T test summary" = summary_ttest,
    "Interactive gender graph" = interactive_graph,
    "Gender and colorblindness ANOVA" = anova_details,
    "ANOVA summary" = anova_summary,
    "Tukey post-hoc tests" = Tukey_comps,
    "Tukey means plot" = Tukey_plot
  )

print(final_results)
