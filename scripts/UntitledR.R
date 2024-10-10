library(dplyr)
library(tidyverse)
library(here)
library(janitor)
library(knitr)

poll_raw <- read.csv(here("downloads/election_prediction/inputs/data/president_polls.csv"))
head(poll_raw)
