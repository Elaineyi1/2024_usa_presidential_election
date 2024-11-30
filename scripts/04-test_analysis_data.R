#### Preamble ####
# Purpose: Test the actual data
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 30 November 2024
# Prerequisites: None

library(testthat)
library(here)
library(arrow)

# Test that critical columns have no missing values
test_that("No missing values in critical columns", {
  data_cleaned <- read_parquet(here("data", "02-analysis_data", "children_health.parquet"))
  
  # Check for missing values in critical columns
  expect_false(any(is.na(data_cleaned$depression)), "Missing values found in depression")
  expect_false(any(is.na(data_cleaned$age)), "Missing values found in age")
  expect_false(any(is.na(data_cleaned$parent_mental_health)), "Missing values found in parent_mental_health")
  expect_false(any(is.na(data_cleaned$screentime)), "Missing values found in screentime")
  expect_false(any(is.na(data_cleaned$bullied)), "Missing values found in bullied")
  expect_false(any(is.na(data_cleaned$friends)), "Missing values found in friends")
  expect_false(any(is.na(data_cleaned$hopeful)), "Missing values found in hopeful")
  expect_false(any(is.na(data_cleaned$violence)), "Missing values found in violence")
  expect_false(any(is.na(data_cleaned$live_with_mental)), "Missing values found in live_with_mental")
  expect_false(any(is.na(data_cleaned$poverty)), "Missing values found in poverty")
})


# Test the column type
test_that("Column types are correct", {
  data_cleaned <- read_parquet(here("data", "02-analysis_data", "children_health.parquet"))
  
  expect_true(is.factor(data_cleaned$depression), "The column `depression` is not a factor")
  expect_true(is.numeric(data_cleaned$age), "The column `age` is not numeric")
  expect_true(is.numeric(data_cleaned$poverty), "The column `poverty` is not numeric")
  expect_true(is.numeric(data_cleaned$parent_mental_health), "The column `parent_mental_health` is not numeric")
})


# Test the range of variable
test_that("Column value ranges are correct", {
  data_cleaned <- read_parquet(here("data", "02-analysis_data", "children_health.parquet"))

  # Check if `age` does not exceed 17 and is greater than or equal to 6
  expect_true(all(data_cleaned$age >= 6 & data_cleaned$age <= 17), 
              "The column `age` contains values outside the range [6, 17]")
  # Check if `parent_mental_health` is between 1 and 5 inclusively
  expect_true(all(data_cleaned$parent_mental_health >= 1 & data_cleaned$parent_mental_health <= 5), 
              "The column `parent_mental_health` contains values outside the range [1, 5]")
})


# Test the unique values in columns
test_that("The length of unique values in columns are correct", {
  data_cleaned <- read_parquet(here("data", "02-analysis_data", "children_health.parquet"))
  
  expect_lte(length(unique(data_cleaned$friends)), 3)
  expect_lte(length(unique(data_cleaned$bullied)), 5)
  expect_lte(length(unique(data_cleaned$screentime)), 5)
  expect_lte(length(unique(data_cleaned$hopeful)), 4)
  expect_lte(length(unique(data_cleaned$violence)), 2)
  expect_lte(length(unique(data_cleaned$depression)), 2)
})