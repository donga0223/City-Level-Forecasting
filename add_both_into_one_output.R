setwd("/Users/dk29776/Dropbox/UTAustin/City-Level-Forecasting")


forecast_date = as.Date(today())
reference_date = forecast_date + (6 - as.integer(format(forecast_date, "%u"))) %% 7

model = "epiENGAGE-INFLAenza"
model = "epiENGAGE-GBQR"
NYC_forecasts = read.csv(paste(model, "/model_output/NYC_ED/", reference_date, "-", model, ".csv", sep=""))
NYC_forecasts <- NYC_forecasts %>%
  filter(location != "Unknown")

TX_forecasts = read.csv(paste(model, "/model_output/TX_NSSP_percent/", reference_date, "-", model, ".csv", sep=""))

both = rbind(NYC_forecasts, TX_forecasts) 


write.csv(both, paste(model, "/model_output/Both/", reference_date, "-", model, ".csv", sep=""), row.names = FALSE)
