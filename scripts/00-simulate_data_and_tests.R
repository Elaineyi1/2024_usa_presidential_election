#### Preamble ####
# Purpose: Simulate the data and test the simulated data
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 22 October 2024
# Prerequisites: None

library(tidyverse)

set.seed(123)

# Define parameters for simulation
n_polls <- 100
states <- c(rep("National", 3), 
            "California", "Texas", "Florida", "New York", 
            "Illinois", "Ohio", "Georgia", 
            "North Carolina", "Michigan", "New Jersey")
pollsters <- c("Pollster A", "Pollster B", "Pollster C", "Pollster D", 
               "Pollster E")
candidates <- c("Kamala Harris")
methodologies <- c("Online", "Phone", "In-person")
methodology_probs <- c(0.6, 0.3, 0.1)


# Simulate the dataset
simulated_data_Harris <- tibble(
  poll_id = 1:n_polls,
  pollster = sample(pollsters, n_polls, replace = TRUE),
  numeric_grade = round(runif(n_polls, 0.5, 3.0), 1), 
  start_date = sample(seq(as.Date("2024-08-01"), as.Date("2024-10-01"), by = "day"), n_polls, replace = TRUE),
  end_date_1 = start_date + sample(1:30, n_polls, replace = TRUE),
  end_date = pmin(end_date_1, Sys.Date()),
  state = sample(states, n_polls, replace = TRUE),
  methodology = sample(c("Online", "Phone", "In-person"), n_polls, replace = TRUE),
  sample_size = ifelse(methodology == "Online", 
                       sample(500:2000, sum(methodology == "Online"), replace = TRUE), 
                       sample(100:500, sum(methodology != "Online"), replace = TRUE)),
  population = sample(c("Adults", "Voters", "Likely Voters", "Registered Voters"), n_polls, replace = TRUE),
  candidate_name = sample(candidates, n_polls, replace = TRUE),
  pct = pmin(pmax(round(rnorm(n_polls, mean = 50, sd = 5), 1), 0), 100)
  ) |> select(-end_date_1)

# Create a cleaned dataset, and 
# dataset with only national surveys and dataset with only state surveys
simulated_cleaned <- simulated_data_Harris |> 
  filter(end_date >= (Sys.Date() - 60)) |> 
  mutate(days_since_end = 1 - (as.numeric(Sys.Date() - end_date) / 60)) |> 
  filter(numeric_grade >= 2.0)
simulated_national <- simulated_cleaned |> filter(state == 'National')
simulated_state <- simulated_cleaned |> filter(state != 'National')


# Test the simulated data
# Check that the sum of rows in the filtered datasets equals the original row count
stopifnot(nrow(simulated_national) + nrow(simulated_state) == nrow(simulated_cleaned))

# Check the numeric grades of pollsters are no less than 2.0
stopifnot(simulated_cleaned$numeric_grade >= 2.0)

# Check if pct is numeric and between 0 and 100
stopifnot(is.numeric(simulated_data_Harris$pct))
stopifnot(all(simulated_data_Harris$pct >= 0 & simulated_data_Harris$pct <= 100))

# Check that days_since_end is between 0 and 1, and the validity of dates
stopifnot(is.numeric(simulated_cleaned$days_since_end))
stopifnot(all(simulated_cleaned$days_since_end >= 0 & simulated_cleaned$days_since_end <= 1))
stopifnot(all(simulated_data_Harris$end_date <= Sys.Date()))

# Check there are at most 52 values in the state column
stopifnot(length(unique(simulated_cleaned$state)) <= 52)
