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
      "other_gbp_millions" ~ "Other"),
    funding_group = case_match(
      `Funding label`,
      c("Total funding", "Grant in aid") ~ "Group 1",
      .default = "Group 2"
    )
  ) |> 
  group_by(
    year,
    `Funding label`,
    funding_group
  ) |> 
  summarize(
    "Total GBP" = sum(GBP),
    .groups = "drop"
  )

nonadjusted_graph <- # saved as 1600x1200 when finished
  ggplot(
    data = funding_non_adjusted,
    aes(x = year, y = `Total GBP`, color = `Funding label`, label = `Funding label`)
  ) +
  geom_textpath(linewidth = 2, size = 8, vjust = -1.5, hjust = "auto", text_smoothing = 65) +
  facet_wrap(~ funding_group) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
nonadjusted_graph
