                    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
                    #                                 #
                    # Written by Lyubomir N. Kolev II #
                    #          June 30, 2025          #
                    #                                 #
                    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

library(tidyverse)
library(plotly)

weekly_gas_prices <-
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')

# check unique values
unique(weekly_gas_prices$fuel)
unique(weekly_gas_prices$grade)
unique(weekly_gas_prices$formulation)

# check na's
na_s <-
  weekly_gas_prices %>% 
  summarize(
    missing_date = sum(is.na(date)),
    missing_fuel = sum(is.na(fuel)),
    missing_grade = sum(is.na(grade)),
    missing_formulation = sum(is.na(formulation)),
    missing_price = sum(is.na(price))
  )
# 2688 rows missing formulation

summary_df <-
  weekly_gas_prices %>% 
  summarize(
    min_date = min(date),
    max_date = max(date),
    min_price = min(price),
    mean_price = mean(price),
    median_price = median(price),
    max_price = max(price)
  )

grouped_summary <-
  weekly_gas_prices %>% 
  group_by(Year = year(date), fuel, grade, formulation) %>% 
  summarize(
    avg_price = mean(price)
  ) %>% 
  mutate(
    Grade = str_to_sentence(grade),
    formulation = str_to_sentence(formulation)
  ) %>% 
  distinct()

# check overall trends across all years
year_trends <-
  ggplot(
    grouped_summary,
    aes(x = Year, y = avg_price, color = Grade)
  ) +
  geom_line() +
  geom_smooth(method = "lm", na.rm = TRUE, se = FALSE) +
  facet_wrap(~ formulation) +
  theme_minimal() +
  labs(
    title = "Trends for all gas grades: 1990-2025",
    subtitle = "All similar regardless of grade, noticeable dips during 2008 crisis and COVID"
  ) +
  ylab("Average Price")

year_trends

# focus on all since individual trends do not seem to differ
trends_filtered <-
  ggplot(
    grouped_summary %>% 
      filter(
        formulation == "All",
        Grade == "All"
      ),
    aes(x = Year, y = avg_price, color = Grade)
  ) +
  geom_line(color = "green", linewidth = 1) +
  theme_minimal() +
  xlab("Year: 1990-2025") +
  ylab("Avg. Price, all fuels") +
  annotate(
    "rect",
    xmin = 2008,
    xmax = 2009,
    ymin = 0,
    ymax = 4.25,
    alpha = .5,
    fill = "orange"
  ) +
  annotate(
    "rect",
    xmin = 2018,
    xmax = 2020,
    ymin = 0,
    ymax = 4.25,
    alpha = .5,
    fill = "orange"
  ) +
  labs(
    title = "Trends for all gas grades: Recession and COVID",
    subtitle = "Noticeable steep drops during Great Recession and COVID (which is more prolonged)"
  )
trends_filtered

# interactive graph with tooltip
trends_interactive <-
  ggplotly(trends_filtered)

# diving deeper into recession and covid
recession_and_covid <-
  grouped_summary %>%
  ungroup() %>% 
  filter(
    Year %in% c(2008, 2009, 2010, 2018, 2019, 2020, 2021),
    formulation == "All",
    Grade == "All"
  ) %>% 
  arrange(Year) %>% 
  mutate(
    time_period = case_when(
      Year %in% c(2008, 2009, 2010) ~ "Recession",
      Year %in% c(2018, 2019, 2020, 2021) ~ "COVID"),
    price_lag = lag(avg_price),
    pct_change = (avg_price - price_lag) / price_lag * 100
  )

recession <-
  ggplot(
  recession_and_covid %>% filter(time_period == "Recession"),
  aes(x = Year, y = avg_price)
  ) +
  geom_point(color = "orange", size = 2) +
  geom_line(color = "orange", linewidth = 1) +
  scale_x_continuous(breaks = recession_and_covid$Year) +
  theme_minimal() +
  ylab("Average Price") +
  labs(
    title = "All gas grades during Recession",
    subtitle = "27% drop heading into 2009, then a 17% recovery into 2010"
  )
recession
recession_interactive <-
  ggplotly(recession)

covid <-
  ggplot(
    recession_and_covid %>% filter(time_period == "COVID"),
    aes(x = Year, y = avg_price)
  ) +
  geom_point(color = "orange", size = 2) +
  geom_line(color = "orange", linewidth = 1) +
  scale_x_continuous(breaks = recession_and_covid$Year) +
  theme_minimal() +
  ylab("Average Price") +
  labs(
    title = "All gas grades during COVID",
    subtitle = "4% drop for 2019, 16% drop into 2020, but then a swift 37% recovery, almost unphased"
  )
covid
covid_interactive <-
  ggplotly(covid)

# combine results into list for easy printing
summary_graphs <-
  list(
    year_trends,
    trends_filtered,
    recession,
    covid
  )

summary_interactive <-
  list(
    trends_interactive,
    recession_interactive,
    covid_interactive
  )

# print all final graphs (static and interactive)
print(summary_graphs)
print(summary_interactive)

# END :)