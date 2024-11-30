#### Preamble ####
# Purpose: Test the simulated data
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 30 November 2024
# Prerequisites: None

library(testthat)
library(here)
library(arrow)

# Test that critical columns have no missing values
test_that("No missing values in critical columns", {
  simulated_data <- read_parquet(here("data", "00-simulated_data", "simulated_data.parquet"))
  
  # Check for missing values in critical columns
  expect_false(any(is.na(simulated_data$depression)), "Missing values found in depression")
  expect_false(any(is.na(simulated_data$age)), "Missing values found in age")
  expect_false(any(is.na(simulated_data$parent_mental_health)), "Missing values found in parent_mental_health")
  expect_false(any(is.na(simulated_data$screentime)), "Missing values found in screentime")
  expect_false(any(is.na(simulated_data$bullied)), "Missing values found in bullied")
  expect_false(any(is.na(simulated_data$friends)), "Missing values found in friends")
  expect_false(any(is.na(simulated_data$hopeful)), "Missing values found in hopeful")
  expect_false(any(is.na(simulated_data$violence)), "Missing values found in violence")
  expect_false(any(is.na(simulated_data$live_with_mental)), "Missing values found in live_with_mental")
  expect_false(any(is.na(simulated_data$poverty)), "Missing values found in poverty")
})


# Test the column type
test_that("Column types are correct", {
  simulated_data <- read_parquet(here("data", "00-simulated_data", "simulated_data.parquet"))
  
  expect_true(is.factor(simulated_data$depression), "The column `depression` is not a factor")
  expect_true(is.numeric(simulated_data$age), "The column `age` is not numeric")
  expect_true(is.numeric(simulated_data$poverty), "The column `poverty` is not numeric")
  expect_true(is.numeric(simulated_data$parent_mental_health), "The column `parent_mental_health` is not numeric")
  expect_true(is.numeric(simulated_data$BIRTH_YR), "The column `BIRTH_YR` is not numeric")
})


# Test the range of variable
test_that("Column value ranges are correct", {
  simulated_data <- read_parquet(here("data", "00-simulated_data", "simulated_data.parquet"))
  
  # Check if `age` does not exceed 17 and is greater than or equal to 6
  expect_true(all(simulated_data$age >= 6 & simulated_data$age <= 17), 
              "The column `age` contains values outside the range [6, 17]")
  # Check if `parent_mental_health` is between 1 and 5 inclusively
  expect_true(all(simulated_data$parent_mental_health >= 1 & simulated_data$parent_mental_health <= 5), 
              "The column `parent_mental_health` contains values outside the range [1, 5]")
})


# Test the unique values in columns
test_that("The length of unique values in columns are correct", {
  simulated_data <- read_parquet(here("data", "00-simulated_data", "simulated_data.parquet"))
  
  expect_lte(length(unique(simulated_data$friends)), 3)
  expect_lte(length(unique(simulated_data$bullied)), 5)
  expect_lte(length(unique(simulated_data$screentime)), 5)
  expect_lte(length(unique(simulated_data$hopeful)), 4)
  expect_lte(length(unique(simulated_data$violence)), 2)
  expect_lte(length(unique(simulated_data$depression)), 2)
})