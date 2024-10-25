#### Preamble ####
# Purpose: Test the cleaned dataset
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 24 October 2024
# Prerequisites: None

# Load necessary libraries
library(testthat)
library(tibble)

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
  expect_type(simulated_cleaned$start_date, "Date")
  expect_type(simulated_cleaned$end_date, "Date")
  expect_type(simulated_cleaned$pct, "double")
  expect_type(simulated_cleaned$sample_size, "double")
  expect_type(simulated_cleaned$days_since_end, "double")
})

# Test that numeric_grade is greater than or equal to 2
test_that("numeric_grade is greater than or equal to 2", {
  expect_true(all(simulated_cleaned$numeric_grade >= 2.0), 
              "Some numeric_grade values are below 2.0")
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
