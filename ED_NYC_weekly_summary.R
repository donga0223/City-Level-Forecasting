
library(dplyr)
library(tidyverse)
library(here)

forecast_est_data <- function(out, i){
  out <- out %>% mutate(no = as.factor(i)) 
  est_low_data <- out %>%
    filter(output_type_id == 0.025) %>%
    mutate(est_low = value) %>%
    select(-output_type_id, -value)
  
  est_median_data <- out %>%
    filter(output_type_id == 0.5) %>%
    mutate(est_median = value) %>%
    select(-output_type_id, -value)
  
  est_high_data <- out %>%
    filter(output_type_id == 0.975) %>%
    mutate(est_high = value) %>%
    select(-output_type_id, -value)
  
  ribbon_data <- est_low_data %>%
    full_join(est_median_data, by = c("location", "horizon", "target_end_date", "target", "output_type", "no")) %>%
    full_join(est_high_data, by =  c("location", "horizon", "target_end_date", "target", "output_type", "no")) 
  
  ribbon_data$target_end_date = as.Date(ribbon_data$target_end_date)
  return(ribbon_data)
}

forecast_plot <- function(date_list, model_name, myregion = NULL){
  date_length <- length(date_list)
  for (i in 1:date_length) {
    file_url <- paste0("https://raw.githubusercontent.com/reichlab/flu-metrocast/main/model-output/", 
                       model_name, "/", date_list[i], "-", model_name, ".csv")
    
    assign(paste0("out", i), read.csv(file_url, quote="\""))
  }
  
  ribbon_data <- forecast_est_data(out1, 1)
  if(date_length > 1){
    for(j in 2:date_length){
      ribbon_data <- rbind(ribbon_data, forecast_est_data(get(paste0("out",j)), j))
    }
  }
  
  
  if(is.null(myregion)){
    myregion <- unique(ribbon_data$location)
  }else{
    myregion <- myregion
  }
  
  
  df <- c1 %>% filter(location %in% myregion,
                      target_end_date < max(target_end_date)) %>%
    mutate(target_end_date = as.Date(target_end_date)) %>%
    dplyr::rename(inc = oracle_value) %>%
    full_join(ribbon_data, by = c("location", "target_end_date"))
  
  
  p <- df %>% filter(target_end_date >= as.Date('2024-11-01')) %>%
    ggplot(aes(target_end_date, inc)) +
    geom_ribbon(aes(ymin = est_low, ymax = est_high, fill = no), alpha = 0.2) + # Add shaded ribbon with fill by 'no'
    geom_point(col="gray30", size=0.95, shape=1, alpha=0.9) +
    geom_line(col="gray30", alpha=0.9) +
    geom_line(aes(y = est_median, col = no, group = no), size = 1) +
    geom_point(aes(y = est_median, col = no, group = no, alpha = 0.9), size = 1.5) +
    facet_wrap(~location, scales = "free_y") +
    labs(
      x = "Date",  # x-axis label
      y = "ILI ED Visits"      # y-axis label
    ) +
    theme(panel.spacing=unit(0, "mm"),
          legend.position = "none",
          axis.text.y=element_text(size=15),
          axis.text.x=element_text(size=10),
          axis.title=element_text(size=20, face="bold"),
          strip.text = element_text(size = 20)) +
    scale_x_date(date_labels = "%b %y",  # Format dates as "Jan 01, 2023"
                 date_breaks = "4 months") +
    scale_fill_manual(values = scales::hue_pal()(length(unique(df$no))))  # Different colors for 'no'
  print(p)
}




c1 <- read.csv("https://raw.githubusercontent.com/reichlab/flu-metrocast/main/target-data/oracle-output.csv")
c1 %>% ggplot(aes(target_end_date, oracle_value, group = location))+
  geom_line()+
  facet_wrap(~location, scales = "free_y")


date_list <- c("2025-01-25", "2025-02-01")

forecast_plot(date_list, model_name = "epiENGAGE-GBQR")

