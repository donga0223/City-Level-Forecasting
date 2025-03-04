library(tidyverse)
library(INLA)
library(lubridate)

source("epiENGAGE-INFLAenza/code/prep-fit-data.R")
source("epiENGAGE-INFLAenza/code/fit-inla-model.R")
source("epiENGAGE-INFLAenza/code/sample-forecasts.R")
source("epiENGAGE-INFLAenza/code/INLA_models.R")



inla.setOption(inla.mode="classic")
df = read.csv('https://raw.githubusercontent.com/reichlab/flu-metrocast/main/target-data/oracle-output.csv')
#nyc_population <- read.csv("https://raw.githubusercontent.com/reichlab/flu-metrocast/main/auxiliary-data/nyc_population.csv")
nyc_population <- read.csv("/Users/dk29776/Dropbox/UTAustin/flu-metrocast-local/auxiliary-data/nyc_population.csv")

filtered_df <- df %>%
  left_join(nyc_population, by = "location") %>%
  rename(date = target_end_date,
         count = oracle_value) %>%
  mutate(epiweek = week(date),
         epiyear = year(date)) %>%
  mutate(ex_lam = population * 1e-6) %>% 
  arrange(date) %>%
  mutate(t = as.integer(factor(date)),
         t2 = as.integer(factor(date)),
         iloc = as.integer(factor(location)),
         weekly_rate = count/(population/100000)) %>%
  filter(date < '2020-03-01' | date >= '2022-08-01')  %>%## remove the COVID season
  filter(target == "ILI ED visits",
         location != "NYC")
 
filtered_df %>%
  filter(date >= '2023-08-01', date != max(date)) %>%
  ggplot(aes(as.Date(date), count)) +
  geom_line() +
  facet_wrap(~location, scales = "free_y")

fit_inla_bren <- function(forecast_date, df = filtered_df, weeks_ahead=4, quantiles, graphical_level, aggregate = NULL){
  df$date <- as.Date(df$date)
  reference_date = forecast_date + (6 - as.integer(format(forecast_date, "%u"))) %% 7
  print("reference_date is ")
  print(reference_date)
  fit_df <- df %>%
    filter(date < forecast_date) |>
    prep_fit_data(weeks_ahead=4, ex_lam=population)
  
  flu_model_shareseason_ar1 <- flu_model_bren(epishare = TRUE, ar = 1)
  #flu_model_shareseason_ar1 <- model_formula(seasonal="shared", temporal="ar1", spatial="besagproper")
  
  fit <- fit_inla_model(fit_df, model = flu_model_shareseason_ar1, forecast_date = forecast_date,
                        q = quantiles)
  
  pred_samp <- forecast_samples(fit_df, fit, forecast_date, nsamp=5000)
  
  pred_summ <- pred_samp |> 
    summarize_quantiles(q = quantiles) |> 
    mutate(reference_date = reference_date,
           horizon = as.integer((date - reference_date) / 7),
           target = "ILI ED visits",
           output_type = "quantile") |> 
    rename(target_end_date = date,
           output_type_id = quantile) |>
    select(reference_date, location, horizon, target, target_end_date, output_type, output_type_id, value)
  #pivot_wider(names_from=quantile)  # wide format for ribbon plots
  
  
  if(!is.null(aggregate)){
    pred_samp_us <- aggregate_forecast(pred_samp, tags=tibble_row(location="NYC"))
    pred_summ <- pred_samp |> 
      bind_rows(pred_samp_us) |> 
      summarize_quantiles(q = quantiles) |>
      mutate(reference_date = reference_date,
             horizon = horizon + 3,
             target = "ILI ED visits",
             output_type = "quantile") |>
      rename(target_end_date = date,
             output_type_id = quantile) |>
      select(reference_date, location, horizon, target, target_end_date, output_type, output_type_id, value) 
  }
  
  write.csv(pred_summ, paste("epiENGAGE-INFLAenza/model_output/", graphical_level, "/", reference_date, "-epiENGAGE-INFLAenza.csv", sep=""), row.names = FALSE)
}


forecast_date <- as.Date(today()) 
quantiles = c(0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975)

print(forecast_date)
fit_inla_bren(forecast_date, df = filtered_df, weeks_ahead=4, quantiles = quantiles, graphical_level = "NYC_ED", aggregate = TRUE)


if(2==3){
  pred_summ %>% 
    filter(output_type_id == 0.5) %>%
    ggplot(aes(target_end_date, value, group = location)) +
    geom_line() +
    facet_wrap(~location)
  
  
  
  
  ggplot() +
    geom_line(
      data = pred_summ %>%
        filter(location != "NYC") %>%
        group_by(target_end_date, output_type_id) %>%
        summarise(value = sum(value)), 
      aes(target_end_date, value, group = 1),
      color = "blue"  # Different color for differentiation
    ) +
    
    # Adding NYC line separately
    geom_line(
      data = pred_summ %>% filter(location == "NYC"),
      aes(target_end_date, value, group = output_type_id),
      color = "red"  # Different color for differentiation
    ) +
    facet_wrap( ~output_type_id, scales = "free_y")
  
}
