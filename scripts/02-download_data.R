#### Preamble ####
# Purpose: Download and Read in data
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 30 November 2024
# Prerequisites: None

# The raw datasets are downloaded manually from
# https://www.census.gov/programs-surveys/nsch/data/datasets.html

library(here)
library(haven)

# Read in the SAS dataset
data_raw <- read_sas(here("data", "01-raw_data", 
                              "nsch_2023e_screener.sas7bdat"))
follow_up_raw <- read_sas(here("data", "01-raw_data", 
                          "nsch_2023e_topical.sas7bdat"))
