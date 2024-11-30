#### Preamble ####
# Purpose: Create and then test a model to explore how several factors affect
# whether a child has depression
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 30 November 2024
# Prerequisites: Factors and priors are chosen

library(dplyr) 
library(here)
library(rstanarm)
library(modelsummary)
library(arrow)

train_data <- read_parquet(here::here("data", "02-analysis_data", "train_data.parquet"))
test_data <- read_parquet(here::here("data", "02-analysis_data", "test_data.parquet"))

# Model Training
model <-
  stan_glm(
    depression ~ bullied + friends + live_with_mental + violence + poverty,
    data = train_data,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = 
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 7
  )

saveRDS(
  model,
  file = "models/model.rds"
)


# Model Testing
predictions <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions >= 0.5, "Yes", "No")
predicted_classes <- factor(predicted_classes, levels = c("Yes", "No"))

actual_values <- test_data$depression
correctness <- sum(predicted_classes == actual_values)/length(actual_values)

