library(tidyverse)
library(skimr)
library(wbstats)
library(countrycode)
library(janitor)
library(broom)
library(infer)
library(scales)

set.seed(2001)
olympics_df <- read_csv("https://raw.githubusercontent.com/cosmoduende/r-olympic-games/main/datasets/athleteEvents.csv")
olympics_df

olympic_medals_df <- olympics_df |> 
  # Only look at the most recent year (2016)
  slice_max(Year) |> 
  # Include more general country code
  mutate(iso3c = countrycode(Team, "country.name", "iso3c")) |> 
  # Filter out individuals who didn't win a medal
  drop_na(Medal) |> 
  # Count the number of medals won by each country (team) that year
  count(iso3c, name = "medals_won")

olympic_medals_df
skim(olympic_medals_df)

ggplot(olympic_medals_df, aes(x = medals_won)) +
  geom_histogram() + 
  theme_minimal() + 
  labs(x = "Number of medals won",
       y = "Count of countries")
## GDP of country in the year 2016 

gdp_df <- wb_data("NY.GDP.MKTP.CD", start_date = 2016, end_date = 2016, return_wide = F) |>
  select(iso3c, gdp = value)

gdp_df

ggplot(gdp_df, aes(x = gdp / 1e9)) + 
  geom_histogram() + 
  scale_x_continuous(label = label_dollar()) + 
  labs(x = "Current GDP (billions of US dollars)",
       y = "Count") +
  theme_minimal()

gdp_75th <- skim(gdp_df, gdp) |> 
  pull(numeric.p75)

gdp_df <- gdp_df |> 
  mutate(income_level = if_else(gdp < gdp_75th, "Medium or low", "High"),
         income_level = factor(income_level, ordered = T, levels = c("Medium or low", "High")))
medals_gdp_df <- olympic_medals_df |> 
  left_join(gdp_df, by = "iso3c") |> 
  drop_na(income_level)

medals_gdp_df
