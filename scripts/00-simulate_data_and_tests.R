#### Preamble ####
# Purpose: Simulate the data and test the simulated data
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 22 October 2024
# Prerequisites: None

library(tidyverse)
library(testthat)
library(tibble)

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
  filter(numeric_grade >= 2.5)
simulated_national <- simulated_cleaned |> filter(state == 'National')
simulated_state <- simulated_cleaned |> filter(state != 'National')



# Tests
# Test that critical columns have no missing values
test_that("No missing values in critical columns in simulated data", {
  expect_false(any(is.na(simulated_cleaned$poll_id)), "Missing values found in poll_id")
  expect_false(any(is.na(simulated_cleaned$pollster)), "Missing values found in pollster")
  expect_false(any(is.na(simulated_cleaned$numeric_grade)), "Missing values found in numeric_grade")
  expect_false(any(is.na(simulated_cleaned$pct)), "Missing values found in pct")
  expect_false(any(is.na(simulated_cleaned$sample_size)), "Missing values found in sample_size")
  expect_false(any(is.na(simulated_cleaned$candidate_name)), "Missing values found in candidate_name")
})

# Test that certain columns have the expected data types in simulated data
test_that("Columns have expected data types in simulated data", {
  expect_type(simulated_cleaned$poll_id, "integer")
  expect_type(simulated_cleaned$pollster, "character")
  expect_type(simulated_cleaned$numeric_grade, "double")
  expect_type(simulated_cleaned$start_date, "double")
  expect_type(simulated_cleaned$end_date, "double")
  expect_type(simulated_cleaned$pct, "double")
  expect_type(simulated_cleaned$sample_size, "integer")
  expect_type(simulated_cleaned$days_since_end, "double")
})

# Test that numeric_grade is greater than or equal to 2.5
test_that("numeric_grade is greater than or equal to 2.5", {
  expect_true(all(simulated_cleaned$numeric_grade >= 2.5), 
              "Some numeric_grade values are below 2.5")
})

# Test that pct is between 0 and 100
test_that("pct values are between 0 and 100", {
  expect_true(all(simulated_cleaned$pct >= 0 & simulated_cleaned$pct <= 100), 
              "pct values are out of range (0-100)")
})

# Test that days_since_end is between 0 and 1
test_that("days_since_end is between 0 and 1 in simulated data", {
  expect_true(all(simulated_cleaned$days_since_end >= 0 & simulated_cleaned$days_since_end <= 1), 
              "days_since_end is out of range (0-1)")
})


# Test that simulated_national only contains "National" in the state column
test_that("simulated_national contains only 'National' in state column", {
  expect_true(all(simulated_national$state == "National"), 
              "Some rows in simulated_national have a non-'National' state.")
})

# Test that simulated_state does not contain "National" in the state column
test_that("simulated_state does not contain 'National' in state column", {
  expect_false(any(simulated_state$state == "National"), 
               "Some rows in simulated_state have 'National' as the state.")
})

# Test that both National and state datasets have no missing values in critical columns
test_that("No missing values in critical columns in simulated_national", {
  expect_false(any(is.na(simulated_national$poll_id)), "Missing values found in poll_id in simulated_national")
  expect_false(any(is.na(simulated_national$pollster)), "Missing values found in pollster in simulated_national")
  expect_false(any(is.na(simulated_national$numeric_grade)), "Missing values found in numeric_grade in simulated_national")
  expect_false(any(is.na(simulated_national$pct)), "Missing values found in pct in simulated_national")
})
