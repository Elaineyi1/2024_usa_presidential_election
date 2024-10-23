#### Preamble ####
# Purpose: Test the cleaned dataset
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 22 October 2024
# Prerequisites: None

# Load necessary libraries
library(dplyr)
library(janitor)
library(readr)
library(here)
library(arrow)

# Load the raw and cleaned datasets
poll_raw <- read.csv(here("downloads/election_prediction/inputs/data/president_polls.csv"))
poll_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data/president_polls_cleaned.parquet"))
poll_harris_national_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data_with_prediction/national_prediction.parquet"))
poll_harris_state_cleaned <- read_parquet(here("downloads/election_prediction/inputs/data_with_prediction/state_prediction.parquet"))

# Check that the sum of rows in the filtered datasets equals the original row count
stopifnot(nrow(poll_harris_national_cleaned) + nrow(poll_harris_state_cleaned) == nrow(poll_harris_cleaned))

# Check the numeric grades of pollsters are no less than 2.0
stopifnot(poll_cleaned$numeric_grade >= 2.0)

# Check if harris_support_ratio is between 0 and 1
stopifnot(is.numeric(poll_cleaned$harris_support_ratio))
stopifnot(all(poll_cleaned$harris_support_ratio >= 0 & poll_cleaned$harris_support_ratio <= 1))

# Check that the cleaned dataset does not contain any rows for candidates other than Trump and Harris
stopifnot(all(poll_cleaned$candidate_name %in% c("Donald Trump", "Kamala Harris")))

# Check that there are no negative pct values in the cleaned dataset
stopifnot(is.numeric(poll_cleaned$pct))
stopifnot(all(poll_cleaned$pct >= 0 & poll_cleaned$pct <= 100))

# Check that days_since_end is between 0 and 1
stopifnot(is.numeric(poll_cleaned$days_since_end))
stopifnot(all(poll_cleaned$days_since_end >= 0 & poll_cleaned$days_since_end <= 1))

# Check there are at most two unique values in the state_or_national column,
# and at most 52 values in the state column
stopifnot(length(unique(poll_cleaned$state_or_national)) <= 2)
stopifnot(length(unique(poll_cleaned$state)) <= 52)
