
setwd("/Users/dk29776/Dropbox/UTAustin/Forecasting")
source("INLA/INLA_models.R")
### calculate probability whether they are increasing or decreasing 
cal_prob <- function(d1_one_plus_horizon, forecast_date, city1){
  val <- d1_one_plus_horizon %>%
    filter(collection_week == forecast_date-7, city == city1) %>%
    select(count)
  filtered_data <- d1_one_plus_horizon %>%
    filter(collection_week >= forecast_date, city == city1) %>%
    select(t, starts_with("pred_0."))
  
  filtered_mean <- d1_one_plus_horizon %>%
    filter(collection_week >= forecast_date-7, city == city1) %>%
    select(t, count, pred_mean)
  
  quantiles <- c(0, 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99)
  estimated_prob <- c()
  greater <- c()
  for(i in 1:4){
    values <- c(0, as.numeric(filtered_data[i,-1]))
    if(filtered_mean$count[1] <= filtered_mean$count[i+1]){
      greater <- c(greater, TRUE)
    }else{
      greater <- c(greater, FALSE)
    }
    estimated_prob <- c(estimated_prob, 1 - approx(values, quantiles, xout=val$count)$y)
  }
  
  
  find_interval <- function(row, value) {
    t_val <- row[1]
    preds <- as.numeric(row[-1])
    pred_names <- names(row)[-1]
    
    if (all(is.na(preds))) {
      return(data.frame(t = t_val, lower_name = NA, lower_bound = NA, upper_name = NA, upper_bound = NA))
    }
    
    lower_index <- max(which(preds <= value), na.rm = TRUE)
    upper_index <- min(which(preds > value), na.rm = TRUE)
    
    lower_bound <- preds[lower_index]
    upper_bound <- preds[upper_index]
    
    lower_name <- pred_names[lower_index]
    upper_name <- pred_names[upper_index]
    
    data.frame(t = t_val, lower_name = lower_name, lower_bound = lower_bound, upper_name = upper_name, upper_bound = upper_bound)
  }
  
  # Apply the function to each row and combine results
  intervals <- do.call(rbind, apply(filtered_data, 1, find_interval, value = val$count))
  intervals <- intervals %>%
    mutate(mean_pred = 1-(as.numeric(sub("pred_", "", lower_name)) + as.numeric(sub("pred_", "", upper_name))) / 2)
  
  return(list(val = val, intervals = intervals, greater = greater, estimated_prob = estimated_prob, filtered_mean = filtered_mean))
  
}

### calculate probability state whether they are increasing or decreasing 


cal_state_prob <- function(d1_one_plus_horizon, forecast_date, state1){
  val <- d1_one_plus_horizon %>%
    filter(week_date == forecast_date-7, state == state1) %>%
    select(count)
  filtered_data <- d1_one_plus_horizon %>%
    filter(week_date >= forecast_date, state == state1) %>%
    select(t, starts_with("pred_0."))
  
  filtered_mean <- d1_one_plus_horizon %>%
    filter(week_date >= forecast_date-7, state == state1) %>%
    select(t, count, pred_mean)
  
  quantiles <- c(0, 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99)
  estimated_prob <- c()
  greater <- c()
  for(i in 1:4){
    values <- c(0, as.numeric(filtered_data[i,-1]))
    if(filtered_mean$count[1] <= filtered_mean$count[i+1]){
      greater <- c(greater, TRUE)
    }else{
      greater <- c(greater, FALSE)
    }
    estimated_prob <- c(estimated_prob, 1 - approx(values, quantiles, xout=val$count)$y)
  }
  
  
  find_interval <- function(row, value) {
    t_val <- row[1]
    preds <- as.numeric(row[-1])
    pred_names <- names(row)[-1]
    
    if (all(is.na(preds))) {
      return(data.frame(t = t_val, lower_name = NA, lower_bound = NA, upper_name = NA, upper_bound = NA))
    }
    
    lower_index <- max(which(preds <= value), na.rm = TRUE)
    upper_index <- min(which(preds > value), na.rm = TRUE)
    
    lower_bound <- preds[lower_index]
    upper_bound <- preds[upper_index]
    
    lower_name <- pred_names[lower_index]
    upper_name <- pred_names[upper_index]
    
    data.frame(t = t_val, lower_name = lower_name, lower_bound = lower_bound, upper_name = upper_name, upper_bound = upper_bound)
  }
  
  # Apply the function to each row and combine results
  intervals <- do.call(rbind, apply(filtered_data, 1, find_interval, value = val$count))
  intervals <- intervals %>%
    mutate(mean_pred = 1-(as.numeric(sub("pred_", "", lower_name)) + as.numeric(sub("pred_", "", upper_name))) / 2)
  
  return(list(val = val, intervals = intervals, greater = greater, estimated_prob = estimated_prob, filtered_mean = filtered_mean))
  
}



flu_model_shareseason_state <- flu_model_state(epishare = TRUE)

flu_model_notshareseason_state <- flu_model_state(epishare = FALSE)





flu_model_shareseason_ar1 <- flu_model_severalcity(epishare = TRUE, ar = 1)
flu_model_shareseason_ar2 <- flu_model_severalcity(epishare = TRUE, ar = 2)
flu_model_shareseason_ar3 <- flu_model_severalcity(epishare = TRUE, ar = 3)
flu_model_shareseason_ar4 <- flu_model_severalcity(epishare = TRUE, ar = 4)
flu_model_notshareseason_ar1 <- flu_model_severalcity(epishare = FALSE, ar = 1)
flu_model_notshareseason_ar2 <- flu_model_severalcity(epishare = FALSE, ar = 2)
flu_model_notshareseason_ar3 <- flu_model_severalcity(epishare = FALSE, ar = 3)
flu_model_notshareseason_ar4 <- flu_model_severalcity(epishare = FALSE, ar = 4)




inc_prob <- function(forecast_date, d1, ds1, mycity, mystate, horizon = 4){
  forecast_date <- as.Date(forecast_date)
  
  d1_one <- d1 %>%
    filter(city %in% mycity,
           collection_week < forecast_date) 
  
  d1_one_pred <- make_pred(d1_one, horizon)
  
  ds1_one <- ds1 %>%
    filter(week_date < forecast_date) 
  
  ds1_one_pred <- make_pred_state(ds1_one, horizon)
  
  
  mod_city <- as.formula(flu_model_shareseason_ar1)
  mod_state <- as.formula(flu_model_shareseason_state)
  
  fit_city <- inla(
    mod_city, family="poisson", data=d1_one_pred,
    quantiles = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99),
    control.compute=list(dic=TRUE, waic = TRUE),
    control.predictor=list(link=1, compute=TRUE)
  )
  
  fit_state <- inla(
    mod_state, family="poisson", data=ds1_one_pred,
    quantiles = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99),
    control.compute=list(dic=TRUE, waic = TRUE),
    control.predictor=list(link=1, compute=TRUE)
  )
  
  d1_one_plus_horizon <- d1 %>% 
    filter(city %in% mycity,
           collection_week < forecast_date+7*horizon) %>%
    arrange(city, collection_week) %>%  
    group_by(city) %>%
    mutate(city_id = cur_group_id()) %>%
    ungroup() %>%
    arrange(collection_week) %>%
    group_by(collection_week) %>%
    mutate(t = cur_group_id()) %>%
    ungroup() %>%
    mutate(count = influenza)%>%
    select(-influenza) |> 
    mutate(
      pred_mean=fit_city$summary.fitted.values$mean,
      pred_min=fit_city$summary.fitted.values$`0.025quant`,
      pred_max=fit_city$summary.fitted.values$`0.975quant`,
      pred_0.01=fit_city$summary.fitted.values$`0.01quant`,
      pred_0.025=fit_city$summary.fitted.values$`0.025quant`,
      pred_0.05=fit_city$summary.fitted.values$`0.05quant`,
      pred_0.1=fit_city$summary.fitted.values$`0.1quant`,
      pred_0.15=fit_city$summary.fitted.values$`0.15quant`,
      pred_0.2=fit_city$summary.fitted.values$`0.2quant`,
      pred_0.25=fit_city$summary.fitted.values$`0.25quant`,
      pred_0.3=fit_city$summary.fitted.values$`0.3quant`,
      pred_0.35=fit_city$summary.fitted.values$`0.35quant`,
      pred_0.4=fit_city$summary.fitted.values$`0.4quant`,
      pred_0.45=fit_city$summary.fitted.values$`0.45quant`,
      pred_0.5=fit_city$summary.fitted.values$`0.5quant`,
      pred_0.55=fit_city$summary.fitted.values$`0.55quant`,
      pred_0.6=fit_city$summary.fitted.values$`0.6quant`,
      pred_0.65=fit_city$summary.fitted.values$`0.65quant`,
      pred_0.7=fit_city$summary.fitted.values$`0.7quant`,
      pred_0.75=fit_city$summary.fitted.values$`0.75quant`,
      pred_0.8=fit_city$summary.fitted.values$`0.8quant`,
      pred_0.85=fit_city$summary.fitted.values$`0.85quant`,
      pred_0.9=fit_city$summary.fitted.values$`0.9quant`,
      pred_0.95=fit_city$summary.fitted.values$`0.95quant`,
      pred_0.975=fit_city$summary.fitted.values$`0.975quant`,
      pred_0.99=fit_city$summary.fitted.values$`0.99quant`
    )
  
  ds1_one_plus_horizon <- ds1 %>% 
    filter(as.Date(week_date) < forecast_date+7*horizon) %>%
    arrange(state, week_date) %>%  
    group_by(state) %>%
    mutate(state_id = cur_group_id()) %>%
    ungroup() %>%
    arrange(week_date) %>%
    group_by(week_date) %>%
    mutate(t = cur_group_id()) %>%
    ungroup() %>%
    mutate(count = influenza)%>%
    select(-influenza)|> 
    mutate(
      pred_mean=fit_state$summary.fitted.values$mean,
      pred_min=fit_state$summary.fitted.values$`0.025quant`,
      pred_max=fit_state$summary.fitted.values$`0.975quant`,
      pred_0.01=fit_state$summary.fitted.values$`0.01quant`,
      pred_0.025=fit_state$summary.fitted.values$`0.025quant`,
      pred_0.05=fit_state$summary.fitted.values$`0.05quant`,
      pred_0.1=fit_state$summary.fitted.values$`0.1quant`,
      pred_0.15=fit_state$summary.fitted.values$`0.15quant`,
      pred_0.2=fit_state$summary.fitted.values$`0.2quant`,
      pred_0.25=fit_state$summary.fitted.values$`0.25quant`,
      pred_0.3=fit_state$summary.fitted.values$`0.3quant`,
      pred_0.35=fit_state$summary.fitted.values$`0.35quant`,
      pred_0.4=fit_state$summary.fitted.values$`0.4quant`,
      pred_0.45=fit_state$summary.fitted.values$`0.45quant`,
      pred_0.5=fit_state$summary.fitted.values$`0.5quant`,
      pred_0.55=fit_state$summary.fitted.values$`0.55quant`,
      pred_0.6=fit_state$summary.fitted.values$`0.6quant`,
      pred_0.65=fit_state$summary.fitted.values$`0.65quant`,
      pred_0.7=fit_state$summary.fitted.values$`0.7quant`,
      pred_0.75=fit_state$summary.fitted.values$`0.75quant`,
      pred_0.8=fit_state$summary.fitted.values$`0.8quant`,
      pred_0.85=fit_state$summary.fitted.values$`0.85quant`,
      pred_0.9=fit_state$summary.fitted.values$`0.9quant`,
      pred_0.95=fit_state$summary.fitted.values$`0.95quant`,
      pred_0.975=fit_state$summary.fitted.values$`0.975quant`,
      pred_0.99=fit_state$summary.fitted.values$`0.99quant`
    )
  
  res_city <- matrix(nrow = length(mycity), ncol = 13)
  res_state <- matrix(nrow = length(mystate), ncol = 13)
  
  for(i in 1:length(mycity)){
    aa <- cal_prob(d1_one_plus_horizon, forecast_date, city1 = mycity[i])
    res_city[i,] <- c(aa$val$count, aa$greater, aa$estimated_prob, aa$filtered_mean$pred_mean[-1])
  }
  
  for(i in 1:length(mystate)){
    aa <- cal_state_prob(ds1_one_plus_horizon, forecast_date, state1 = "TX")
    res_state[i,] <- c(aa$val$count, aa$greater, aa$estimated_prob, aa$filtered_mean$pred_mean[-1])
  }
  #rownames(res) <- forecast_date
  colnames(res_city) <- colnames(res_state) <- c("true", "h1 gt true", "h2 gt true", "h3 gt true", "h4 gt true",
                                                 "h1 gt prob", "h2 gt prob", "h3 gt prob", "h4 gt prob",
                                                 "est_mean_h1", "est_mean_h2", "est_mean_h3", "est_mean_h4")
  
  res <- rbind(res_city, res_state)
  rownames(res) <- c(mycity, mystate)
  return(res)
}



