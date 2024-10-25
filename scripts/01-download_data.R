#### Preamble ####
# Purpose: Download and Read in data from fivethirtyeight
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 25 October 2024
# Prerequisites: None

library(readr)
library(here)

# Read the CSV file from the URL
poll_raw <- read_csv(
  file = "https://projects.fivethirtyeight.com/polls/data/president_polls.csv",
  show_col_types = FALSE
)

# Write the CSV file to a local directory using the 'here' package
write_csv(
  x = poll_raw,
  file = here("inputs", "data", "president_polls.csv")
)

