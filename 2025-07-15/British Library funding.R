library(tidyverse)
library(geomtextpath)

funding <-
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv")

data_dict <-
  read_tsv("Data dictionary.txt")

funding_non_adjusted <-
  funding |> 
  select(
    year,
    nominal_gbp_millions:other_gbp_millions
  ) |> 
  pivot_longer(
    nominal_gbp_millions:other_gbp_millions,
    names_to = "Funding type",
    values_to = "GBP"
  ) |>
  distinct() |> 
  group_by(
    year,
    `Funding type`
  ) |> 
  summarize(
    "Total GBP" = sum(GBP),
    .groups = "drop"
  )

overall_graph <-
  ggplot(
    data = funding_non_adjusted,
    aes(x = year, y = `Total GBP`, color = `Funding type`, label = `Funding type`)
  ) +
  geom_line() +
  geom_textline(linewidth = 2) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
overall_graph
