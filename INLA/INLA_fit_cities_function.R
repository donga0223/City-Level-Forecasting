library(gridExtra)
library(ggplot2)
library(scales)

make_pred <- function(d1, horizon){
  d1_arrange <- d1 %>% 
    mutate(epiweek = week(collection_week)) %>%
    arrange(city, collection_week) %>%  
    group_by(city) %>%
    mutate(city_id = cur_group_id()) %>%
    ungroup() %>%
    arrange(collection_week) %>%
    group_by(collection_week) %>%
    mutate(t = cur_group_id()) %>%
    ungroup() 
  
  d1_pred <- expand_grid(
    tibble(t=1:horizon+max(d1_arrange$t), 
           epiweek = (1:horizon + last(d1_arrange$epiweek) - 1) %% max(d1_arrange$epiweek) + 1),
    distinct(d1_arrange, city, city_id)
  )
  d1_pred <- d1_arrange |> 
    bind_rows(d1_pred) |>
    mutate(count = influenza ) |>
    select(-influenza, -collection_week)
  
  return(d1_pred)
}


wis_cal <- function(d1_one_plus_horizon, forecast_date, state = FALSE) {
  wis_levels <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99)
  
  wis_fun <- function(df, level) {
    col_name <- paste0("wis_", level)
    pred_col <- paste0("pred_", level)
    df %>%
      mutate(!!col_name := 2 * (1 * (count <= !!sym(pred_col)) - level) * (!!sym(pred_col) - count)) 
  }
  
  if(state == FALSE){
    aa <- d1_one_plus_horizon %>%
      filter(collection_week >= forecast_date) %>%
      reduce(wis_levels, wis_fun, .init = .) %>%
      rowwise() %>%
      mutate(wis_mean = mean(c_across(starts_with("wis_")), na.rm = TRUE)) %>%
      ungroup()
    res <- aa %>% 
      select(collection_week, city, wis_mean)
  }else if(state == TRUE){
    aa <- d1_one_plus_horizon %>%
      filter(week_date >= forecast_date) %>%
      reduce(wis_levels, wis_fun, .init = .) %>%
      rowwise() %>%
      mutate(wis_mean = mean(c_across(starts_with("wis_")), na.rm = TRUE)) %>%
      ungroup()
    res <- aa %>% 
      select(week_date, state, wis_mean)
  }
  
  return(res)
}

### length(forecast_date) should be 1 and several cities are okay
inla_fit_cities <- function(d1, forecast_date, model, mycity, hyper_epwk, hyper_wk, horizon = 4, 
                            joint_season = TRUE, W.model = NULL, dist = FALSE){
  forecast_date <- as.Date(forecast_date)
  if(length(forecast_date) > 1){
    stop("forecast date should be one day")
  }
  dic <- waic <- c()
  d1_one <- d1 %>%
    filter(city %in% mycity,
           collection_week < forecast_date) 
  
  d1_one_pred <- make_pred(d1_one, horizon)
  
  mod <- as.formula(model)
  
  fit <- inla(
    mod, family="poisson", data=d1_one_pred,
    quantiles = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99),
    control.compute=list(dic=TRUE, waic = TRUE),
    control.predictor=list(link=1, compute=TRUE)
  )
  
  if(joint_season == TRUE){
    ep_wk_fx <- fit$summary.random$epiweek |> 
      as_tibble() |>
      mutate(city="all city") # assumes states are in correct order in dat_pred
  }else{
    ep_wk_fx <- fit$summary.random$epiweek |> 
      as_tibble() |>
      mutate(city=rep(unique(d1_one_pred$city), each=max(d1_one_pred$epiweek))) # assumes states are in correct order in dat_pred
  }
  
  if(dist == FALSE){
    ep_t_rd <- fit$summary.random$t |> 
      as_tibble() |>
      mutate(city=rep(unique(d1_one_pred$city), each=max(d1_one_pred$t)), t = ID) # assumes states are in correct order in dat_pred
    }else{
    ep_t_rd <- fit$summary.random$city_id |> 
      as_tibble() |>
      mutate(t=rep(unique(d1_one_pred$t), each=max(d1_one_pred$city_id)),
             city = rep(mycity, max(d1_one_pred$t))) # assumes states are in correct order in dat_pred
  }
  
  
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
      pred_mean=fit$summary.fitted.values$mean,
      pred_min=fit$summary.fitted.values$`0.025quant`,
      pred_max=fit$summary.fitted.values$`0.975quant`,
      pred_0.01=fit$summary.fitted.values$`0.01quant`,
      pred_0.025=fit$summary.fitted.values$`0.025quant`,
      pred_0.05=fit$summary.fitted.values$`0.05quant`,
      pred_0.1=fit$summary.fitted.values$`0.1quant`,
      pred_0.15=fit$summary.fitted.values$`0.15quant`,
      pred_0.2=fit$summary.fitted.values$`0.2quant`,
      pred_0.25=fit$summary.fitted.values$`0.25quant`,
      pred_0.3=fit$summary.fitted.values$`0.3quant`,
      pred_0.35=fit$summary.fitted.values$`0.35quant`,
      pred_0.4=fit$summary.fitted.values$`0.4quant`,
      pred_0.45=fit$summary.fitted.values$`0.45quant`,
      pred_0.5=fit$summary.fitted.values$`0.5quant`,
      pred_0.55=fit$summary.fitted.values$`0.55quant`,
      pred_0.6=fit$summary.fitted.values$`0.6quant`,
      pred_0.65=fit$summary.fitted.values$`0.65quant`,
      pred_0.7=fit$summary.fitted.values$`0.7quant`,
      pred_0.75=fit$summary.fitted.values$`0.75quant`,
      pred_0.8=fit$summary.fitted.values$`0.8quant`,
      pred_0.85=fit$summary.fitted.values$`0.85quant`,
      pred_0.9=fit$summary.fitted.values$`0.9quant`,
      pred_0.95=fit$summary.fitted.values$`0.95quant`,
      pred_0.975=fit$summary.fitted.values$`0.975quant`,
      pred_0.99=fit$summary.fitted.values$`0.99quant`
    )
  
  wis <- wis_cal(d1_one_plus_horizon, forecast_date)
  
  q1 <- ggplot(ep_wk_fx, aes(ID, mean)) +
    geom_ribbon(aes(ymin=`0.025quant`, ymax=`0.975quant`), col="gray70", alpha=0.6) +
    geom_line() +
    geom_vline(xintercept = week(forecast_date), col = 'orange', linetype = 'dashed', size = 2) +
    facet_wrap(~city) +
    labs(x="Week of the year", y="Seasonal effect (log scale)") +
    theme_bw() +
    theme(legend.position="none")+
    theme(axis.text=element_text(size=30),
          axis.title=element_text(size=30,face="bold"))
  
  q11 <- ggplot(ep_wk_fx, aes(ID, exp(mean))) +
    geom_ribbon(aes(ymin=exp(`0.025quant`), ymax=exp(`0.975quant`)), col="gray70", alpha=0.6) +
    geom_line() +
    geom_vline(xintercept = week(forecast_date), col = 'orange', linetype = 'dashed', size = 2) +
    facet_wrap(~city) +
    labs(x="Week of the year", y="Seasonal effect") +
    theme_bw() +
    theme(legend.position="none")+
    theme(axis.text=element_text(size=30),
          axis.title=element_text(size=30,face="bold"))

  
  
  
  q2 <- ggplot(ep_t_rd, aes(t, mean))+
    geom_ribbon(aes(ymin=`0.025quant`, ymax=`0.975quant`), col="gray70", alpha=0.6) +
    geom_line() +
    geom_vline(xintercept = max(ep_t_rd$t)-horizon, col = 'orange', linetype = 'dashed', size = 1) +
    facet_wrap(~city) +
    labs(x="t", y="t effect (log scale)") +
    theme_bw() +
    theme(legend.position="none") +
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=20,face="bold"),
          strip.text = element_text(size = 20))

  
  q22 <- ep_t_rd %>%
    filter(t > max(t)-50) %>%
    ggplot(aes(t, mean))+
    geom_ribbon(aes(ymin=`0.025quant`, ymax=`0.975quant`), col="gray70", alpha=0.6) +
    geom_line() +
    geom_vline(xintercept = max(ep_t_rd$t)-horizon, col = 'orange', linetype = 'dashed', size = 1) +
    facet_wrap(~city) +
    labs(x="t", y="t effect (log scale)") +
    theme_bw() +
    theme(legend.position="none") +
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=20,face="bold"),
          strip.text = element_text(size = 20))
  
  q222 <- ep_t_rd %>%
    filter(t > max(t)-50) %>%
    ggplot(aes(t, exp(mean)))+
    geom_ribbon(aes(ymin=exp(`0.025quant`), ymax=exp(`0.975quant`)), col="gray70", alpha=0.6) +
    geom_line() +
    geom_vline(xintercept = max(ep_t_rd$t)-horizon, col = 'orange', linetype = 'dashed', size = 1) +
    facet_wrap(~city) +
    labs(x="t", y="t effect") +
    theme_bw() +
    theme(legend.position="none") +
    theme(axis.text=element_text(size=15),
          axis.title=element_text(size=20,face="bold"),
          strip.text = element_text(size = 20))
  

  q3 <- d1_one_plus_horizon |> 
    mutate(collection_week = as.Date(collection_week)) |> 
    filter(collection_week > as.Date(forecast_date) - 60) |>
    ggplot(aes(collection_week, count)) +
    geom_line(aes(y=pred_mean, col=city, group = city)) +
    geom_ribbon(aes(ymin=pred_min, ymax=pred_max, group = city), col="gray70", alpha=0.6) +
    #geom_vline(xintercept = max(as.Date(d1_one_plus_horizon$collection_week))-horizon*7, col = 'orange', linetype = 'dashed', size = 1) +
    #geom_point(data = d1_big5_plus4, aes(x=t, y=influenza, col="gray30", size=0.5, shape=1), alpha=0.9) +
    geom_point(col="gray30", size=0.95, shape=1, alpha=0.9) +
    facet_wrap(~city, scales="free_y") +
    #scale_x_continuous(sec.axis=sec_axis(~.x / 53, "years", breaks=1:3)) +
    theme_bw() +
    theme(panel.spacing=unit(0, "mm"),
          legend.position = "none",
          axis.text.y=element_text(size=15),
          axis.text.x=element_text(size=10),
          axis.title=element_text(size=20,face="bold"),
          strip.text = element_text(size = 20)) 
  if(!is.null(W.model)){
    q3 <- q3 + ggtitle(paste("Weekly effect =", W.model)) +
      theme(plot.title = element_text(size = 30))
  }
  
  
  if(length(mycity)==1){
    grid.arrange(q1, q2, q3, ncol=3)
  }else{
    print(q1)
    print(q11)
    print(q2)
    print(q22)
    print(q222)
    print(q3)
  }
  return(list(summary = summary(fit), wis = wis))
  
}



inla_fit_several_forecastdate_cities <- function(d1, forecast_date, model, mycity, hyper_epwk, hyper_wk, horizon = 4, 
                                                 joint_season = TRUE, W.model = NULL, dist = FALSE){
  forecast_date <- as.Date(forecast_date)
  dic <- waic <- c()
  for(i in 1:length(forecast_date)){
    d1_one <- d1 %>%
      filter(city %in% mycity,
             collection_week < forecast_date[i]) 
    
    d1_one_pred <- make_pred(d1_one, horizon)
    
    mod <- as.formula(model)
    
    fit <- inla(
      mod, family="poisson", data=d1_one_pred,
      quantiles = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99),
      control.compute=list(dic=TRUE, waic = TRUE),
      control.predictor=list(link=1, compute=TRUE)
    )
    #dic <- c(dic, fit$dic$dic)
    #waic <- c(waic, fit$waic$waic)
    
    
    
    if(joint_season == TRUE){
      ep_wk_fx_tmp <- fit$summary.random$epiweek |> 
        as_tibble() |>
        mutate(city="all city",
               forecast_num = as.factor(i)) 
    }else{
      ep_wk_fx_tmp <- fit$summary.random$epiweek |> 
        as_tibble() |>
        mutate(city=rep(unique(d1_one_pred$city), each=max(d1_one_pred$epiweek)), # assumes states are in correct order in dat_pred
               forecast_num = as.factor(i))
    }
    if(i == 1){
      ep_wk_fx <- ep_wk_fx_tmp
    }else{
      ep_wk_fx <- rbind(ep_wk_fx, ep_wk_fx_tmp)
    }
    
    if(dist==FALSE){
      ep_t_rd_tmp <- fit$summary.random$t |> 
        as_tibble() |>
        mutate(city=rep(unique(d1_one_pred$city), each=max(d1_one_pred$t)), # assumes states are in correct order in dat_pred
               forecast_num = as.factor(i),
               t = ID)
    }else{
      ep_t_rd_tmp <- fit$summary.random$city_id |> 
        as_tibble() |>
        mutate(t=rep(unique(d1_one_pred$t), each=max(d1_one_pred$city_id)),
               city = rep(mycity, max(d1_one_pred$t)),
               forecast_num = as.factor(i)) # assumes states are in correct order in dat_pred
    }
    
    
    if(i == 1){
      ep_t_rd <- ep_t_rd_tmp
    }else{
      ep_t_rd <- rbind(ep_t_rd, ep_t_rd_tmp)
    }
    
    
    d1_one_plus_horizon_tmp <- d1 %>% 
      filter(city %in% mycity,
             collection_week < as.Date(forecast_date[i])+7*horizon) %>%
      arrange(city, collection_week) %>%  
      group_by(city) %>%
      mutate(city_id = cur_group_id()) %>%
      ungroup() %>%
      arrange(collection_week) %>%
      group_by(collection_week) %>%
      mutate(t = cur_group_id()) %>%
      ungroup() %>%
      mutate(count = influenza,
             forecast_num = as.factor(i))%>%
      select(-influenza) |> 
      mutate(
        pred_mean=fit$summary.fitted.values$mean,
        pred_min=fit$summary.fitted.values$`0.025quant`,
        pred_max=fit$summary.fitted.values$`0.975quant`,
        pred_0.01=fit$summary.fitted.values$`0.01quant`,
        pred_0.025=fit$summary.fitted.values$`0.025quant`,
        pred_0.05=fit$summary.fitted.values$`0.05quant`,
        pred_0.1=fit$summary.fitted.values$`0.1quant`,
        pred_0.15=fit$summary.fitted.values$`0.15quant`,
        pred_0.2=fit$summary.fitted.values$`0.2quant`,
        pred_0.25=fit$summary.fitted.values$`0.25quant`,
        pred_0.3=fit$summary.fitted.values$`0.3quant`,
        pred_0.35=fit$summary.fitted.values$`0.35quant`,
        pred_0.4=fit$summary.fitted.values$`0.4quant`,
        pred_0.45=fit$summary.fitted.values$`0.45quant`,
        pred_0.5=fit$summary.fitted.values$`0.5quant`,
        pred_0.55=fit$summary.fitted.values$`0.55quant`,
        pred_0.6=fit$summary.fitted.values$`0.6quant`,
        pred_0.65=fit$summary.fitted.values$`0.65quant`,
        pred_0.7=fit$summary.fitted.values$`0.7quant`,
        pred_0.75=fit$summary.fitted.values$`0.75quant`,
        pred_0.8=fit$summary.fitted.values$`0.8quant`,
        pred_0.85=fit$summary.fitted.values$`0.85quant`,
        pred_0.9=fit$summary.fitted.values$`0.9quant`,
        pred_0.95=fit$summary.fitted.values$`0.95quant`,
        pred_0.975=fit$summary.fitted.values$`0.975quant`,
        pred_0.99=fit$summary.fitted.values$`0.99quant`
      )
    
    wis_tmp <- wis_cal(d1_one_plus_horizon_tmp, forecast_date[i])
    
    if(i == 1){
      d1_one_plus_horizon <- d1_one_plus_horizon_tmp
      wis <- wis_tmp
    }else{
      d1_one_plus_horizon <- rbind(d1_one_plus_horizon, d1_one_plus_horizon_tmp)
      wis <- rbind(wis, wis_tmp)
    }
    
  }
  ep_t_rd <- ep_t_rd %>%
    mutate(forecast_num2 = length(forecast_date)+1 - as.numeric(forecast_num))
  mycolor <- scales::hue_pal()(length(forecast_date))
  
  q1 <- ggplot(ep_wk_fx, aes(ID, mean, group = forecast_num, col = forecast_num)) +
    geom_ribbon(aes(ymin=`0.025quant`, ymax=`0.975quant`), alpha=0.2) +
    geom_line() +
    #geom_vline(xintercept = week(forecast_date), col = mycolor) +
    facet_wrap(~city) +
    labs(x="Week of the year", y="Seasonal effect (log scale)") +
    theme_bw() +
    theme(legend.position="none")
  
  for (i in 1:length(forecast_date)) {
    q1 <- q1 + geom_vline(xintercept = week(forecast_date[i]), col = mycolor[i], linetype = 'dashed', size = 1) # Change 'blue' to desired color
  }
  
  
  q2 <- ggplot(ep_t_rd, aes(t, mean, group = as.factor(forecast_num), color = as.factor(forecast_num)))+
    geom_ribbon(aes(ymin=`0.025quant`, ymax=`0.975quant`), alpha=0.1) +
    geom_line() +
    #geom_vline(xintercept = max(ep_t_rd$ID)-horizon, col = mycolor[i]) +
    facet_wrap(~city) +
    labs(x="t", y="t effect (log scale)") +
    theme_bw() +
    theme(legend.position="none")
  for (i in 1:length(forecast_date)) {
    current_max_id <- max(ep_t_rd %>% filter(forecast_num == i) %>% pull(ID))
    q2 <- q2 + geom_vline(xintercept = current_max_id-horizon, col = mycolor[i], linetype = 'dashed', size = 1) 
  } 
  
  
  last_horizon_data <- d1_one_plus_horizon %>%
    group_by(city, forecast_num) %>%
    slice_tail(n = horizon+1) %>%
    ungroup()
  
  q3 <- d1_one_plus_horizon |> 
    filter(collection_week> as.Date(min(forecast_date)) - 60) |>
    ggplot(aes(collection_week, count, group = forecast_num, color = forecast_num)) +
    geom_line(aes(x = collection_week, y=pred_mean, col=forecast_num), col = "gray30") +
    geom_ribbon(aes(ymin=pred_min, ymax=pred_max), col="gray70", alpha=0.2, data = last_horizon_data) +
    geom_line(aes(x = collection_week, y=pred_mean, col= forecast_num), size = 1, data = last_horizon_data) +
    #geom_point(aes(x = collection_week, y=pred_mean, col= forecast_num), size = 1, data = last_horizon_data) +
    geom_point(col="gray30", size=0.95, shape=1, alpha=0.9) +
    facet_wrap(~city, scales="free_y") +
    #scale_x_continuous(sec.axis=sec_axis(~.x / 53, "years", breaks=1:3)) +
    theme_bw() +
    theme(panel.spacing=unit(0, "mm"),
          legend.position = "none",
          axis.text.y=element_text(size=15),
          axis.text.x=element_text(size=10),
          axis.title=element_text(size=20,face="bold"),
          strip.text = element_text(size = 20))
  if(!is.null(W.model)){
    q3 <- q3 + ggtitle(paste("Weekly effect =", W.model)) +
      theme(plot.title = element_text(size = 30))
  }

  
  if(length(mycity)==1){
    grid.arrange(q1, q2, q3, ncol=3)
  }else{
    print(q1)
    print(q2)
    print(q3)
  }
  
  return(list(summary = summary(fit), wis = wis))
}


