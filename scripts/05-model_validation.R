#### Preamble ####
# Purpose: Download and Read in data from fivethirtyeight
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 2 November 2024
# Prerequisites: None

library(dplyr)

# The model was created using the polls that ended before Oct 25th.
# This model validation includes the polls that ended after Oct 25th 
# to test the model.

poll_model_validation <- read_csv(
  file = "https://projects.fivethirtyeight.com/polls/data/president_polls.csv",
  show_col_types = FALSE
)

# Clean the dataset and eliminate the rows with a missing numeric grade value
poll_model_validation <- poll_model_validation |>
  clean_names() |>
  select(
    poll_id, pollster_id, pollster, numeric_grade, state, sample_size, 
    population, party, candidate_name, pct, methodology, start_date, end_date) |>
  drop_na(numeric_grade) |> filter(numeric_grade >= 2.5) |>
  mutate(
    state = ifelse(is.na(state), "National", state),
    state_or_national = ifelse(state == "National", "National", "State"),
    state = str_replace(state, " CD-.*", ""),
    state = str_trim(state))

poll_model_validation <- poll_model_validation |> 
  mutate(end_date = mdy(end_date),
         start_date = mdy(start_date)) |> 
  filter(end_date >= (Sys.Date() - 60)) |> 
  mutate(days_since_end = 1 - (as.numeric(Sys.Date() - end_date) / 60))

# Some other modifications
poll_model_validation <- poll_model_validation |> 
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
odd_poll_ids_model <- poll_model_validation |>
  group_by(poll_id) |> 
  summarise(count = n()) |>
  filter(count %% 2 == 1) |>
  pull(poll_id)

poll_model_validation <- poll_model_validation |>
  filter(!poll_id %in% odd_poll_ids_model)

# Create a new column that represents the support for Kamala Harris 
# as a ratio of the combined support for both her and Donald Trump
poll_model_validation <- poll_model_validation |> 
  group_by(poll_id, pollster_id, numeric_grade, state, sample_size, 
           start_date, end_date) |> 
  mutate(total_pct = sum(pct), 
         harris_pct = sum(pct[candidate_name == "Kamala Harris"])) |> 
  mutate(harris_support_ratio = harris_pct / total_pct) |> 
  ungroup() |> 
  select(-total_pct, -harris_pct)

# Predict the Harris support ratio for a different cleaned dataset 
# using the model
poll_harris_model <- poll_model_validation |> filter(candidate_name == "Kamala Harris")
valid_levels <- unique(poll_harris_cleaned$pollster)
poll_harris_model_cleaned <- poll_harris_model |> filter(pollster %in% valid_levels)

model_data <- 
  predict(harris_model, newdata = poll_harris_model_cleaned)
poll_harris_model_cleaned <- poll_harris_model_cleaned |>
  mutate(predicted_harris_ratio = model_data)
