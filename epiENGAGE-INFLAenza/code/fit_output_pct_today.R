
library(tidyverse)
library(INLA)
library(lubridate)

source("epiENGAGE-INFLAenza/code_bren/prep-fit-data.R")
source("epiENGAGE-INFLAenza/code_bren/model-formulas.R")
source("epiENGAGE-INFLAenza/code_bren/fit-inla-model.R")
source("epiENGAGE-INFLAenza/code_bren/sample-forecasts.R")

df = read.csv('https://raw.githubusercontent.com/reichlab/flu-metrocast/main/target-data/oracle-output.csv') %>%
  filter(target == "Flu ED visits pct") %>%
  mutate(date = as.Date(target_end_date),
         epiweek=week(date), 
         ex_lam=1,
         value = oracle_value/100) %>%
  select(-oracle_value, -target_end_date)



fit_output <- function(forecast_date, graphical_level = 'TX_NSSP_percent'){
  # back a few weeks to compare to truth data
  reference_date = forecast_date + (6 - as.integer(format(forecast_date, "%u"))) %% 7
  print("reference_date is ")
  print(reference_date)
  
  model <- model_formula(
    response="value", seasonal="shared", temporal="ar1", spatial="exchangeable"
  )
  fit_df <- prep_fit_data(df, forecast_date = forecast_date - 7, weeks_ahead=4, ex_lam=ex_lam)
  fit <- fit_inla_model(fit_df, model, family="beta", response=value)
  
  pred_samp <- forecast_samples(fit_df, fit, family="beta", response=value)
  q_wis <- c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)
  
  pred_summ <- summarize_quantiles(pred_samp, q=q_wis) %>%
    mutate(reference_date = reference_date,
           horizon = horizon -2,
           output_type = "quantile",
           target = "Flu ED visits pct",
           value = value * 100) %>%
    rename(target_end_date = date,
           output_type_id = quantile) %>%
    select(reference_date, location, horizon, target, target_end_date, output_type, output_type_id, value)
  
  
  write.csv(pred_summ, paste("epiENGAGE-INFLAenza/model_output/", graphical_level, "/", reference_date, "-epiENGAGE-INFLAenza.csv", sep=""), row.names = FALSE)
  
}

forecast_date = as.Date(today())

fit_output(forecast_date = forecast_date, graphical_level = 'TX_NSSP_percent')

