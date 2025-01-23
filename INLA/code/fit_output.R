library(tidyverse)
library(INLA)
library(lubridate)

source("code/prep-fit-data.R")
source("code/fit-inla-model.R")
source("code/sample-forecasts.R")
source("code/INLA_models.R")


inla.setOption(inla.mode="classic")
df = read.csv('https://github.com/reichlab/flu-metrocast/blob/main/target-data/oracle-output.csv')

filtered_df <- df %>%
  rename(date = target_end_date,
         count = oracle_value) %>%
  mutate(epiweek = week(date),
         epiyear = year(date)) %>%
  mutate(ex_lam = population * 1e-6) %>% 
  arrange(date) %>%
  mutate(t = as.integer(factor(date)),
         t2 = as.integer(factor(date)),
         iloc = as.integer(factor(location)),
         weekly_rate = count/(population/100000))%>%
  select(-X) 

filtered_df %>%
  filter(date >= '2023-08-01', date != max(date)) %>%
  ggplot(aes(as.Date(date), count)) +
  geom_line() +
  facet_wrap(~location, scales = "free_y")

fit_inla_bren <- function(forecast_date, df = filtered_df, weeks_ahead=4, quantiles, graphical_level){
  df$date <- as.Date(df$date)
  reference_date = forecast_date + (6 - as.integer(format(forecast_date, "%u"))) %% 7
  fit_df <- df %>%
    filter(date < forecast_date) |>
    prep_fit_data(weeks_ahead=4, ex_lam=population)
  
  flu_model_shareseason_ar1 <- flu_model_bren(epishare = TRUE, ar = 1)
  fit <- fit_inla_model(fit_df, model = flu_model_shareseason_ar1, forecast_date,
                        q = quantiles)
  
  pred_summ <- forecast_samples(fit_df, fit, forecast_date, nsamp=5000) |> 
    summarize_quantiles(q = quantiles) |> 
    mutate(reference_date = reference_date,
           horizon = as.integer((date - reference_date) / 7),
           target = "ILI ED visits",
           output_type = "quantile") |> 
    rename(target_end_date = date,
           output_type_id = quantile) |>
    select(reference_date, location, horizon, target, target_end_date, output_type, output_type_id, value)
  #pivot_wider(names_from=quantile)  # wide format for ribbon plots
  
  write.csv(pred_summ, paste("model_output/", graphical_level, "/", reference_date, "-epiENGAGE-INLA.csv", sep=""))
}


date_list <- seq.Date(from = as.Date("2023-10-08"), 
                      #to = as.Date("2023-10-05"),
                      to = as.Date("2024-03-31"), 
                      by = "week")


quantiles = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)

date_list = c(as.Date("2025-01-10"), as.Date("2024-12-27"), as.Date("2024-11-29"), as.Date("2024-10-18"))
date_list = c(as.Date("2023-10-12"), as.Date("2023-11-24"), as.Date("2023-12-13"), 
              as.Date("2023-12-20"), as.Date("2024-01-24"), as.Date("2024-03-01") )

for(i in date_list){
  forecast_date <- as.Date(i) # first date where forecasting will begin
  print(forecast_date)
  fit_inla_bren(forecast_date, df = filtered_df, weeks_ahead=4, quantiles = quantiles, graphical_level = "NYC_ED")
  
}
  




