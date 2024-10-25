#### Preamble ####
# Purpose: Create a model to predict the presidential election result
# Author: Boxuan Yi
# Email: boxuan.yi@mail.utoronto.ca
# Date: 22 October 2024
# Prerequisites: None

library(dplyr) 
library(rstanarm)
library(modelsummary)
library(arrow)

poll_cleaned <- read_parquet(file = here("downloads/election_prediction/inputs/data/president_polls_cleaned.parquet"))
poll_harris_cleaned <- poll_cleaned |> filter(candidate_name == "Kamala Harris")

# Create one dataset for national surveys and one for state surveys
poll_harris_national_cleaned <- poll_cleaned |> 
  filter(state_or_national == 'National') |> 
  filter(candidate_name == "Kamala Harris")
poll_harris_state_cleaned <- poll_cleaned |> 
  filter(state_or_national != 'National') |>
  filter(candidate_name == "Kamala Harris")

# Create a model
harris_model <- stan_glm(
  formula = harris_support_ratio ~ pollster + numeric_grade + days_since_end + population,  # Include national_poll as a predictor
  data = poll_harris_cleaned,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
  prior_aux = exponential(rate = 1, autoscale = TRUE),
  seed = 7
)

# Save the model to an RDS file
saveRDS(
  harris_model,
  file = here("downloads/election_prediction/models/model.rds")
)


# Prediction
national_predict <- predict(harris_model, newdata = poll_harris_national_cleaned)
poll_harris_national_cleaned <- poll_harris_national_cleaned |>
  mutate(predicted_harris_ratio = national_predict)

state_predict <- predict(harris_model, newdata = poll_harris_state_cleaned)
poll_harris_state_cleaned <- poll_harris_state_cleaned |>
  mutate(predicted_harris_ratio = state_predict)

# Save the prediction
write_parquet(x = poll_harris_national_cleaned,
              sink = here("downloads/election_prediction/inputs/data_with_prediction/national_prediction.parquet"))

write_parquet(x = poll_harris_state_cleaned,
  sink = here("downloads/election_prediction/inputs/data_with_prediction/state_prediction.parquet"))

