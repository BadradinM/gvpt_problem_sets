library(infer)
library(poliscidata)
library(tidyverse)
library(scales)
library(modelsummary)

gss <- gss |> 
  # Select only the relevant columns
  select(id, partyid_3, natpark) |> 
  # Remove non-complete responses
  drop_na()

datasummary_crosstab(natpark ~ partyid_3, data = gss)


gss |> 
  # Get the number of respondents in each party who indicated each level of 
  # support
  count(partyid_3, natpark) |> 
  # Convert these counts to proportions by party
  group_by(partyid_3) |> 
  mutate(prop = n / sum(n)) |> 
  # Plot these proportions
  ggplot(aes(x = natpark, y = prop)) + 
  geom_col() + 
  facet_wrap(~ partyid_3) + 
  labs(x = "Support for spending on parks and recreation",
       y = "Percentage of respondents") + 
  theme_minimal() + 
  scale_y_continuous(labels = scales::label_percent())


# Calculate the observed count for each category
obs_values <- count(gss, natpark, partyid_3, name = "obs_n")

# Calculate the total number of respondents for each party ID
partyid_3_totals <- count(gss, partyid_3, name = "partyid_total")
# Calculate the total number of respondents for each support level
natpark_totals <- count(gss, natpark, name = "natpark_total")

# Calculate the observed count for each category
obs_values <- count(gss, natpark, partyid_3, name = "obs_n")

# Calculate the total number of respondents for each party ID
partyid_3_totals <- count(gss, partyid_3, name = "partyid_total")
# Calculate the total number of respondents for each support level
natpark_totals <- count(gss, natpark, name = "natpark_total")

obs_exp_counts <- natpark_totals |> 
  expand_grid(partyid_3_totals) |> 
  relocate(partyid_3) |> 
  # Calculated the expected values
  mutate(exp_n = (natpark_total * partyid_total) / nrow(gss)) |>
  # Add the observed values for comparison
  left_join(obs_values, by = c("partyid_3", "natpark"))

obs_exp_counts

# Calculate the observed count for each category
obs_values <- count(gss, natpark, partyid_3, name = "obs_n")

# Calculate the total number of respondents for each party ID
partyid_3_totals <- count(gss, partyid_3, name = "partyid_total")
# Calculate the total number of respondents for each support level
natpark_totals <- count(gss, natpark, name = "natpark_total")




