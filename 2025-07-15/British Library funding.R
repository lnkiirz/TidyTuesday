library(tidyverse)

funding <-
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv")

data_dict <-
  read_tsv("Data dictionary.txt")

overall_graph <-
  ggplot(
    data = funding,
    aes(x = year)
  ) +
  geom_line(aes(y = nominal_gbp_millions, color = nominal_gbp_millions)) +
  geom_line(aes(y = gia_gbp_millions, color = gia_gbp_millions)) +
  geom_line(aes(y = voluntary_gbp_millions, color = voluntary_gbp_millions)) +
  geom_line(aes(y = investment_gbp_millions, color = investment_gbp_millions)) +
  geom_line(aes(y = services_gbp_millions, color = services_gbp_millions)) +
  geom_line(aes(y = other_gbp_millions, color = other_gbp_millions)) +
  scale_color_manual(name = "Revenue", values = c("red", "green", "blue", "orange", "yellow", "purle")) +
  theme_minimal()
overall_graph
