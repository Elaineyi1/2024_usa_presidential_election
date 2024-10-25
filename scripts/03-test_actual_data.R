#### Preamble ####
# Purpose: Test the cleaned dataset
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 24 October 2024
# Prerequisites: None

# Load necessary libraries
library(testthat)
library(here)
library(arrow)

# Test that the sum of rows in the filtered datasets equals the original row count
test_that("Sum of rows in national and state cleaned data equals total rows in cleaned data", {
  poll_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data/president_polls_cleaned.parquet"))
  poll_harris_national_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data_with_prediction/national_prediction.parquet"))
  poll_harris_state_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data_with_prediction/state_prediction.parquet"))
  
  expect_equal(nrow(poll_harris_national_cleaned) + nrow(poll_harris_state_cleaned), nrow(poll_cleaned))
})


# Test that critical columns have no missing values
test_that("No missing values in critical columns", {
  poll_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data/president_polls_cleaned.parquet"))
  
  expect_false(any(is.na(poll_cleaned$poll_id)), "Missing values found in poll_id")
  expect_false(any(is.na(poll_cleaned$pollster_id)), "Missing values found in pollster_id")
  expect_false(any(is.na(poll_cleaned$numeric_grade)), "Missing values found in numeric_grade")
  expect_false(any(is.na(poll_cleaned$pct)), "Missing values found in pct")
  expect_false(any(is.na(poll_cleaned$sample_size)), "Missing values found in sample_size")
  expect_false(any(is.na(poll_cleaned$candidate_name)), "Missing values found in candidate_name")
})


# Test that certain columns have the expected data types
test_that("Columns have expected data types", {
  poll_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data/president_polls_cleaned.parquet"))
  
  expect_type(poll_cleaned$poll_id, "character")
  expect_type(poll_cleaned$pollster_id, "character")
  expect_type(poll_cleaned$numeric_grade, "double")
  expect_type(poll_cleaned$pct, "double")
  expect_type(poll_cleaned$sample_size, "double")
  expect_type(poll_cleaned$start_date, "Date")
  expect_type(poll_cleaned$end_date, "Date")
  expect_type(poll_cleaned$harris_support_ratio, "double")
})


# Test that the numeric grades of pollsters are no less than 2.0
test_that("Pollsters' numeric grades are no less than 2.0", {
  poll_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data/president_polls_cleaned.parquet"))
  
  expect_true(all(poll_cleaned$numeric_grade >= 2.0))
})


# Test that harris_support_ratio is between 0 and 1
test_that("harris_support_ratio is numeric and between 0 and 1", {
  poll_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data/president_polls_cleaned.parquet"))
  
  expect_true(is.numeric(poll_cleaned$harris_support_ratio))
  expect_true(all(poll_cleaned$harris_support_ratio >= 0 & poll_cleaned$harris_support_ratio <= 1))
})


# Test that the cleaned dataset only contains rows for Trump and Harris
test_that("Dataset only contains Trump and Harris as candidates", {
  poll_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data/president_polls_cleaned.parquet"))
  
  expect_true(all(poll_cleaned$candidate_name %in% c("Donald Trump", "Kamala Harris")))
})


# Test that there are no negative pct values and pct is within 0 to 100
test_that("pct values are between 0 and 100", {
  poll_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data/president_polls_cleaned.parquet"))
  
  expect_true(is.numeric(poll_cleaned$pct))
  expect_true(all(poll_cleaned$pct >= 0 & poll_cleaned$pct <= 100))
})


# Test that days_since_end is between 0 and 1
test_that("days_since_end is numeric and between 0 and 1", {
  poll_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data/president_polls_cleaned.parquet"))
  
  expect_true(is.numeric(poll_cleaned$days_since_end))
  expect_true(all(poll_cleaned$days_since_end >= 0 & poll_cleaned$days_since_end <= 1))
})


# Test that there are at most two unique values in state_or_national and at most 52 values in state
test_that("At most two unique values in state_or_national and 52 in state", {
  poll_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data/president_polls_cleaned.parquet"))
  
  expect_lte(length(unique(poll_cleaned$state_or_national)), 2)
  expect_lte(length(unique(poll_cleaned$state)), 52)
})
