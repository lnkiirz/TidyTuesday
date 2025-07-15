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
  mutate(
    "Funding label" = case_match(
      `Funding type`,
      "nominal_gbp_millions" ~ "Total funding",
      "gia_gbp_millions" ~ "Grant in aid",
      "voluntary_gbp_millions" ~ "Voluntary and donations",
      "investment_gbp_millions" ~ "Returns on investments",
      "services_gbp_millions" ~ "Service delivery",
      "other_gbp_millions" ~ "Other"
    )
  ) |> 
  group_by(
    year,
    `Funding label`
  ) |> 
  summarize(
    "Total GBP" = sum(GBP),
    .groups = "drop"
  )

overall_graph <-
  ggplot(
    data = funding_non_adjusted,
    aes(x = year, y = `Total GBP`, color = `Funding label`, label = `Funding label`)
  ) +
  geom_textline(linewidth = 2, size = 5, vjust = -.5, hjust = "auto") +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
overall_graph
