library(tidyverse)
library(janitor)
set.seed(1234)

possible_outcome <- c("HEADS", "TAILS")
repeat_trials <- sample(possible_outcome, size = 10, replace = T, c(0.5, 0.5))
repeat_trials
table(repeat_trials)
tibble(outcome = sample(possible_outcome, 10, replace = T, prob = c(0.5, 0.5))) |> 
  tabyl(outcome) |> 
  ggplot(aes(y = outcome, x = percent)) + 
  geom_col() + 
  geom_point(aes(x = c(0.5, 0.5)), size = 3, colour = "red") + 
  theme_minimal() + 
  scale_x_continuous(limits = c(0, 1))

pos_outcome <- c("H", "T")
pos_outcome
rep_trial <- sample(pos_outcome, size = 10, replace = T, c(0.5,0.5))
rep_trial
table(rep_trial)

tibble(outcome = sample(possible_outcome, 100, replace = T, prob = c(0.5, 0.5))) |> 
  tabyl(outcome) |> 
  ggplot(aes(y = outcome, x = percent)) + 
  geom_col() + 
  geom_point(aes(x = c(0.5, 0.5)), size = 3, colour = "red") + 
  theme_minimal() + 
  scale_x_continuous(limits = c(0, 1))

tibble(outcome = sample(possible_outcome, 1000, replace = T, prob = c(0.5, 0.5))) |> 
  tabyl(outcome) |> 
  ggplot(aes(y = outcome, x = percent)) + 
  geom_col() + 
  geom_point(aes(x = c(0.5, 0.5)), size = 3, colour = "red") + 
  theme_minimal() + 
  scale_x_continuous(limits = c(0, 1))

tibble(outcome = sample(possible_outcome, 10000, replace = T, prob = c(0.5, 0.5))) |> 
  tabyl(outcome) |> 
  ggplot(aes(y = outcome, x = percent)) + 
  geom_col() + 
  geom_point(aes(x = c(0.5, 0.5)), size = 3, colour = "red") + 
  theme_minimal() + 
  scale_x_continuous(limits = c(0, 1))

tibble(outcome = sample(possible_outcome, 100000, replace = T, prob = c(0.5, 0.5))) |> 
  tabyl(outcome) |> 
  ggplot(aes(y = outcome, x = percent)) + 
  geom_col() + 
  geom_point(aes(x = c(0.5, 0.5)), size = 3, colour = "red") + 
  theme_minimal() + 
  scale_x_continuous(limits = c(0, 1))

library(tidyverse)
library(janitor)
library(poliscidata)
library(ggdist)
library(MetBrewer)
library(DescTools)

set.seed(1234)
sample(possible_outcome, 100, replace = T, prob = c(0.5, 0.5))

coin_flip <- function(possible_outcome, n) {
  
  outcomes <- sample(possible_outcome, size = n, replace = T, prob = c(0.5, 0.5))
  
  return(table(outcomes)["HEADS"])
  
}

results <- tibble(trial = 1:100000) |> 
  rowwise() |> 
  mutate(n_heads = coin_flip(possible_outcome, 100))

results