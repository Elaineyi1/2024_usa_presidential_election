#### Preamble ####
# Purpose: Clean the dataset
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 30 November 2024
# Prerequisites: Be familiar with the dataset and its methodology.

library(here)
library(haven)
library(tidyr)
library(janitor)
library(arrow)
library(dplyr) 

follow_up_raw <- read_sas(here("data", "01-raw_data", 
                               "nsch_2023e_topical.sas7bdat"))

# Clean the data
data_cleaned <- follow_up_raw |>
  select(K2Q32A, FORMTYPE, BIRTH_YR, A1_MENTHEALTH, A2_MENTHEALTH, FPL_I1, 
         SCREENTIME, BULLIED_R, MAKEFRIEND, HOPEFUL, ACE7, ACE8, K2Q32B, K2Q32C) |> 
  filter(FORMTYPE != 'T1') |>
  drop_na(-c(K2Q32B, K2Q32C)) |> 
  mutate(
    age = 2023 - BIRTH_YR,
    parent_mental_health = rowMeans(cbind(A1_MENTHEALTH, A2_MENTHEALTH), na.rm = TRUE)
  ) |> 
  rename(
    depression = K2Q32A,
    depression_current = K2Q32B,
    depression_level = K2Q32C,
    screentime = SCREENTIME,
    poverty = FPL_I1,
    bullied = BULLIED_R,
    friends = MAKEFRIEND,
    hopeful = HOPEFUL,
    violence = ACE7, 
    live_with_mental = ACE8
  ) |> filter(age >= 6 & age <= 17) |>
  select(-BIRTH_YR, -A1_MENTHEALTH, -A2_MENTHEALTH, -FORMTYPE)


data_cleaned <- data_cleaned |>
  mutate(
    depression = recode(
      depression,
      `1` = "Yes",
      `2` = "No"
    ),
    depression = factor(depression, levels = c("No", "Yes")),
    screentime = recode(
      screentime,
      `1` = "Less than 1 hour",
      `2` = "1 hour",
      `3` = "2 hours",
      `4` = "3 hours",
      `5` = "4 or more hours"
    ),
    screentime = factor(screentime, levels = c(
      "Less than 1 hour",
      "1 hour",
      "2 hours",
      "3 hours",
      "4 or more hours"
    )),
    bullied = recode(
      bullied,
      `1` = "Never",
      `2` = "1-2 times in the past year",
      `3` = "1-2 times per month",
      `4` = "1-2 times per week",
      `5` = "Almost every day"
    ),
    bullied = factor(bullied, levels = c(
      "1" = "Never",
      "2" = "1-2 times in the past year",
      "3" = "1-2 times per month",
      "4" = "1-2 times per week",
      "5" = "Almost every day"
    )),
    friends = recode(
      friends,
      `1` = "No difficulty",
      `2` = "A little difficulty",
      `3` = "A lot of difficulty"
    ),
    friends = factor(friends, levels = c(
      "No difficulty",
      "A little difficulty",
      "A lot of difficulty"
    )),
    hopeful = recode(
      hopeful,
      `1` = "All of the time",
      `2` = "Most of the time",
      `3` = "Some of the time",
      `4` = "None of the time"
    ),
    hopeful = factor(hopeful, levels = c(
      "All of the time",
      "Most of the time",
      "Some of the time",
      "None of the time"
    )),
    violence = recode(
      violence,
      `1` = "Yes",
      `2` = "No"
    ),
    violence = factor(violence, levels = c("No", "Yes")),
    live_with_mental = recode(
      live_with_mental,
      `1` = "Yes",
      `2` = "No"
    ),
    live_with_mental = factor(live_with_mental, levels = c("Yes", "No")),
    depression_current = recode(
      depression_current,
      `1` = "Yes",
      `2` = "No"
    ),
    depression_current = factor(depression_current, levels = c("No", "Yes")),
    depression_level = recode(
      depression_level,
      `1` = "Mild",
      `2` = "Moderate",
      `3` = "Severe"
    ),
    depression_level = factor(depression_level, levels = c("Mild", "Moderate", "Severe"))
  )

data_cleaned$depression <- as.factor(data_cleaned$depression)

# Testing/Training splits
set.seed(7)
n <- nrow(data_cleaned)
train_indices <- sample(1:n, size = 0.8 * n)

train_data <- data_cleaned[train_indices, ]
test_data <- data_cleaned[-train_indices, ]


# Save as a parquet file
write_parquet(
  x = data_cleaned,
  sink = here("data", "02-analysis_data", "children_health.parquet")
)

write_parquet(
  x = train_data,
  sink = here("data", "02-analysis_data", "train_data.parquet")
)

write_parquet(
  x = test_data,
  sink = here("data", "02-analysis_data", "test_data.parquet")
)
