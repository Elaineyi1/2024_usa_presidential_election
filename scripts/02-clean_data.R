#### Preamble ####
# Purpose: Clean the dataset
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 25 October 2024
# Prerequisites: Be familiar with the dataset and its methodology.

library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(janitor)
library(readr)
library(arrow)
library(lubridate)

# Read the dataset
poll_raw <- read_csv(here("inputs", "data", "president_polls.csv"))

# Clean the dataset and eliminate the rows with a missing numeric grade value
poll_cleaned <- 
  poll_raw |>
  clean_names() |>
  select(
    poll_id, pollster_id, pollster, numeric_grade, state, sample_size, 
    population, party, candidate_name, pct, methodology, start_date, end_date) |>
  drop_na(numeric_grade) |> filter(numeric_grade >= 2.5)

# Modify the state column
poll_cleaned <- poll_cleaned |>
  mutate(
    state = ifelse(is.na(state), "National", state),
    state_or_national = ifelse(state == "National", "National", "State"),
    state = str_replace(state, " CD-.*", ""),
    state = str_trim(state))

# Create a new column showing how recent the survey ended
poll_cleaned <- poll_cleaned |> 
  mutate(end_date = mdy(end_date),
         start_date = mdy(start_date)) |> 
  filter(end_date >= (Sys.Date() - 60)) |> 
  mutate(days_since_end = 1 - (as.numeric(Sys.Date() - end_date) / 60))

# Some other modifications
poll_cleaned <- poll_cleaned |> 
  filter(candidate_name %in% c("Donald Trump", "Kamala Harris")) |>
  mutate(pct = as.numeric(pct),
         population = case_when(
           str_detect(population, regex("A", ignore_case = TRUE)) ~ "Adults",
           str_detect(population, regex("V", ignore_case = TRUE)) & population != "LV" & population != "RV" ~ "Voters", 
           str_detect(population, regex("LV", ignore_case = TRUE)) ~ "Likely Voters",
           str_detect(population, regex("RV", ignore_case = TRUE)) ~ "Registered Voters",
           TRUE ~ population
         )) |> 
  drop_na(pct, sample_size, candidate_name, pollster_id, poll_id)

# Find odd poll_ids and remove these rows
odd_poll_ids <- poll_cleaned |>
  group_by(poll_id) |> 
  summarise(count = n()) |>
  filter(count %% 2 == 1) |>
  pull(poll_id)

poll_cleaned <- poll_cleaned |>
  filter(!poll_id %in% odd_poll_ids)

# Create a new column that represents the support for Kamala Harris 
# as a ratio of the combined support for both her and Donald Trump
poll_cleaned <- poll_cleaned |> 
  group_by(poll_id, pollster_id, numeric_grade, state, sample_size, 
           start_date, end_date) |> 
  mutate(total_pct = sum(pct), 
    harris_pct = sum(pct[candidate_name == "Kamala Harris"])) |> 
  mutate(harris_support_ratio = harris_pct / total_pct) |> 
  ungroup() |> 
  select(-total_pct, -harris_pct)

# Save the cleaned dataset
write_parquet(
  x = poll_cleaned,
  sink = here("inputs", "data", "president_polls_cleaned.parquet")
)

