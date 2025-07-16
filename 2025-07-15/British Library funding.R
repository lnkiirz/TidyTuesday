library(tidyverse)
library(geomtextpath)

funding <-
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv") |> 
  rename(
    Year = year
  )

data_dict <-
  read_tsv("Data dictionary.txt")

funding_non_adjusted <-
  funding |> 
  select(
    Year,
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
    Year,
    `Funding label`,
    funding_group
  ) |> 
  summarize(
    "Total GBP" = sum(GBP),
    .groups = "drop"
  )

theme_set(theme_minimal())

nonadjusted_graph <-
  ggplot(
    data = funding_non_adjusted,
    aes(x = Year, y = `Total GBP`, color = `Funding label`, label = `Funding label`)
  ) +
  geom_textpath(linewidth = 1, size = 2, vjust = -2, hjust = "auto", text_smoothing = 65) +
  facet_wrap(~ funding_group) +
  labs(
    title = "Non-adjusted GBP Funding by Source",
    subtitle = "Total moved with GIA funding, showing increase over time"
  ) +
  theme(
    legend.position = "none"
  )

ggsave(
  "Funding graph - non-adjusted GBP.jpg",
  plot = nonadjusted_graph,
  width = 2000,
  height = 1400,
  units = "px"
)

funding_adjusted <-
  funding |> 
  select(
    Year,
    total_y2000_gbp_millions,
    gia_y2000_gbp_millions:other_y2000_gbp_millions
  ) |> 
  pivot_longer(
    c(total_y2000_gbp_millions, gia_y2000_gbp_millions:other_y2000_gbp_millions),
    names_to = "Funding type",
    values_to = "GBP"
  ) |>
  mutate(
    "Funding label" = case_match(
      `Funding type`,
      "total_y2000_gbp_millions" ~ "Total funding",
      "gia_y2000_gbp_millions" ~ "Grant in aid",
      "voluntary_y2000_gbp_millions" ~ "Voluntary and donations",
      "investment_y2000_gbp_millions" ~ "Returns on investments",
      "services_y2000_gbp_millions" ~ "Service delivery",
      "other_y2000_gbp_millions" ~ "Other"),
    funding_group = case_match(
      `Funding label`,
      c("Total funding", "Grant in aid") ~ "Group 1",
      .default = "Group 2"
    )
  ) |> 
  group_by(
    Year,
    `Funding label`,
    funding_group
  ) |> 
  summarize(
    "Total GBP" = sum(GBP),
    .groups = "drop"
  )

adjusted_graph <-
  ggplot(
    data = funding_adjusted,
    aes(x = Year, y = `Total GBP`, color = `Funding label`, label = `Funding label`)
  ) +
  geom_textpath(linewidth = 1, size = 2, vjust = -1, hjust = "auto", text_smoothing = 65) +
  facet_wrap(~ funding_group) +
  labs(
    title = "Adjusted GBP (Year 2000) Funding by Source",
    subtitle = "Total and GIA still move in tandem, but trends are moving down!"
  ) +
  theme(
    legend.position = "none"
  )

ggsave(
  "Funding graph - GBP adjusted to Year 2000.jpg",
  plot = adjusted_graph,
  width = 2000,
  height = 1200,
  units = "px"
)

compare_adjusted_vs_non <-
  funding_adjusted |> 
  inner_join(funding_non_adjusted, by = c("Year", "Funding label", "funding_group"), suffix = c("_adjusted", "_nonadjusted")) |> 
  filter(
    funding_group == "Group 1"
  ) |> 
  pivot_longer(
    c(`Total GBP_adjusted`, `Total GBP_nonadjusted`),
    names_to = "Adjusted_or_not",
    values_to = "GBP"
  ) |> 
  group_by(
    Year,
    `Funding label`,
    Adjusted_or_not
  ) |> 
  summarize(
    "Total GBP" = sum(GBP)
  ) |> 
  mutate(
    Category = case_match(
      Adjusted_or_not,
      "Total GBP_adjusted" ~ "Adjusted",
      "Total GBP_nonadjusted" ~ "Not adjusted"
    )
  )

adjusted_vs_non_graph <-
  ggplot(
    data = compare_adjusted_vs_non,
    aes(x = Year, y = `Total GBP`, color = Category)
  ) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Comparing Opposing Trends, Adjusted vs. Not Adjusted Total and GIA",
    subtitle = "Without inflation adjustment, an increase is actually a dicrease."
  )

ggsave(
  "Adjusted vs. non-adjusted Total and GIA.jpg",
  plot = adjusted_vs_non_graph,
  width = 2000,
  height = 1200,
  unit = "px"
)

gia_graph <-
  ggplot(
    data = funding,
    aes(x = Year, y = gia_as_percent_of_peak_gia)
  ) +
  geom_line(aes(color = "darkred")) +
  scale_color_manual(values = "darkred") +
  labs(
    title = "GIA as a Percentage of Peak GIA",
    subtitle = "Showing drastic decreases over time, slight rebound recently."
  ) +
  ylab("GIA as % of its peak") +
  theme(
    legend.position = "none"
  )

ggsave(
  "GIA as pct of peak GIA.jpg",
  plot = gia_graph
)

percentage_of_y2000_graph <-
  ggplot(
    data = funding,
    aes(x = Year, y = percentage_of_y2000_income)
  ) +
  geom_line(color = "red") + 
  scale_color_manual(values = "red") +
  labs(
    title = "Overall Funding as a Percent of Y2000-Adjusted GBP",
    subtitle = "Showing same decrease and slight recovery as GIA."
  ) +
  ylab("Total funding as % of Y2000 GBP") +
  theme(
    legend.position = "none"
  )

ggsave(
  "Overall funding as pct of Y2000 GBP.jpg",
  plot = percentage_of_y2000_graph,
  width = 2000,
  height = 800,
  units = "px"
)

percentage_and_gia <-
  funding |> 
  select(
    Year,
    percentage_of_y2000_income,
    gia_as_percent_of_peak_gia
  ) |> 
  pivot_longer(
    c(percentage_of_y2000_income, gia_as_percent_of_peak_gia),
    names_to = "pct_gia_long",
    values_to = "GBP"
  ) |> 
  mutate(
    category = 
      recode(
        `pct_gia_long`,
        "percentage_of_y2000_income" = "Percentage of Y2000 Income",
        "gia_as_percent_of_peak_gia" = "GIA Percentage of Peak GIA")
  )

percentage_and_gia_graph <-
  ggplot(
    data = percentage_and_gia,
    aes(x = Year, y = GBP, color = category, label = category)
  ) +
  geom_textpath(linewidth = 1, size = 2, vjust = 2, hjust = "auto", text_smoothing = 45) +
  labs(
    title = "Comparing Inflation-Adjusted Funding and GIA in Y2000 GBP",
    subtitle = "Both showing drastic decreases, maybe promising rebounds recently."
  ) +
  ylab("Totals as % of Y2000 GBP") +
  theme(
    legend.position = "none"
  )

ggsave(
  "Funding and GIA - inflation-adjusted.jpg",
  plot = percentage_and_gia_graph,
  width = 2000,
  height = 800,
  units = "px"
)

final_results <- 
  list(
    nonadjusted_graph,
    adjusted_graph,
    adjusted_vs_non_graph,
    gia_graph,
    percentage_of_y2000_graph,
    percentage_and_gia_graph
  )

print(final_results)