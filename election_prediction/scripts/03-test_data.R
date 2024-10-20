#### Preamble ####
# Purpose: Test the cleaned dataset
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 17 October 2024
# Prerequisites: None

# Load necessary libraries
library(dplyr)
library(janitor)
library(readr)
library(here)

# Load the raw and cleaned datasets
poll_raw <- read.csv(here("downloads/election_prediction/inputs/data/president_polls.csv"))
poll_cleaned <- read.csv(here("downloads/election_prediction/inputs/data/president_polls_cleaned.csv"))
poll_national_cleaned <- read.csv(here("downloads/election_prediction/inputs/data/national_cleaned.csv"))
poll_state_cleaned <- read.csv(here("downloads/election_prediction/inputs/data/state_cleaned.csv"))

# Check that the sum of rows in the filtered datasets equals the original row count
stopifnot(nrow(poll_national_cleaned) + nrow(poll_state_cleaned) == nrow(poll_cleaned))

# Check if harris_support_ratio is between 0 and 1
stopifnot(all(poll_cleaned$harris_support_ratio >= 0 & poll_cleaned$harris_support_ratio <= 1))

# Check that the cleaned dataset does not contain any rows for candidates other than Trump and Harris
stopifnot(all(poll_cleaned$candidate_name %in% c("Donald Trump", "Kamala Harris")))

# Check that there are no negative pct values in the cleaned dataset
stopifnot(all(poll_cleaned$pct >= 0 & poll_cleaned$pct <= 100))

# Check that end_date is not larger than Sys.Date() - 60
stopifnot(all(poll_cleaned$end_date >= (Sys.Date() - 60)))

# Check that days_since_end is between 0 and 1
stopifnot(all(poll_cleaned$days_since_end >= 0 & poll_cleaned$days_since_end <= 1))


