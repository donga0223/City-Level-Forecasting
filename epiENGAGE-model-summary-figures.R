

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
    full_join(est_median_data, by = c("reference_date", "location", "horizon", "target_end_date", "target", "output_type", "no")) %>%
    full_join(est_high_data, by =  c("reference_date", "location", "horizon", "target_end_date", "target", "output_type", "no")) 
  
  ribbon_data$target_end_date = as.Date(ribbon_data$target_end_date)
  return(ribbon_data)
}

forecast_plot <- function(date_list, model_name, myregion = NULL, graphic_level, 
                          spec = "-", connect = FALSE, pdf = FALSE, specific_h = NULL){
  date_length <- length(date_list)
  for (i in 1:date_length) {
    #file_url <- paste0("https://raw.githubusercontent.com/reichlab/flu-metrocast/main/model-output/", 
    #                   model_name, "/", date_list[i], "-", model_name, ".csv")
    #
    #assign(paste0("out", i), read.csv(file_url, quote="\""))
    assign(paste0("out", i), read.csv(paste("/Users/dk29776/Dropbox/UTAustin/City-Level-Forecasting/", model_name, "/model_output/", graphic_level, "/", date_list[i], spec, model_name, ".csv", sep = "")))
  }
  
  ribbon_data <- forecast_est_data(out1, 1)
  if(date_length > 1){
    for(j in 2:date_length){
      ribbon_data <- rbind(ribbon_data, forecast_est_data(get(paste0("out",j)), j))
    }
  }
  
  if(connect){
    tmp <- ribbon_data %>%
      distinct(reference_date, location, target, no) %>%
      mutate(reference_date = as.Date(reference_date)-7) %>%
      #mutate(reference_date = as.character(reference_date)) %>%
      left_join(c1, by = c("reference_date" = "target_end_date", "location", "target")) %>%
      mutate(horizon = -1,
             target_end_date = as.Date(reference_date),
             output_type = "true",
             est_low = NA,
             est_median = oracle_value,
             est_high = NA) %>%
      select(reference_date, location, horizon, target, target_end_date, output_type, no,
             est_low, est_median, est_high) 
    
    ribbon_data <- rbind(tmp, ribbon_data)
  }
  
  if(is.null(myregion)){
    myregion <- unique(ribbon_data$location)
  }else{
    myregion <- myregion
  }
  
  
  df <- c1 %>% filter(location %in% myregion) %>%
    mutate(target_end_date = as.Date(target_end_date)) %>%
    dplyr::rename(inc = oracle_value) %>%
    full_join(ribbon_data, by = c("location", "target_end_date"))
  
  if(!is.null(specific_h)){
    df <- df %>% filter(horizon == specific_h)
  }
  
  p <- df %>% filter(target_end_date >= as.Date('2024-09-01')) %>%
    ggplot(aes(target_end_date, inc)) +
    geom_ribbon(aes(ymin = est_low, ymax = est_high, fill = no), alpha = 0.2) + # Add shaded ribbon with fill by 'no'
    geom_point(col="gray30", size=0.95, shape=1, alpha=0.9) +
    geom_line(col="gray30", alpha=0.9) +
    geom_line(aes(y = est_median, col = no, group = no), size = 1) + #, linetype = "dashed") +
    geom_point(aes(y = est_median, col = no, group = no, alpha = 0.9), size = 1.5) +
    facet_wrap(~location, scales = "free_y") +
    labs(
      x = "Date",  # x-axis label
      y = "Flu ED Visits pct"      # y-axis label
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
  
  if(!is.null(specific_h)){
    p <- df %>% filter(target_end_date >= as.Date('2023-09-01')) %>%
      ggplot(aes(target_end_date, inc, group = 1)) +
      geom_point(col="gray30", size=0.95, shape=1, alpha=0.9) +
      geom_line(col="gray30", alpha=0.9) +
      geom_line(aes(y = est_median, col = 'orange'), size = 1) +
      geom_point(aes(y = est_median, col = 'orange', alpha = 0.9), size = 1.5) +
      geom_ribbon(aes(ymin = est_low, ymax = est_high), fill = 'orange', alpha = 0.3) +  # Add shaded ribbon
      facet_wrap(~location, scales = "free_y") + 
      ggtitle(paste("horizon = ", specific_h)) + 
      theme(panel.spacing=unit(0, "mm"),
            legend.position = "none",
            axis.text.y=element_text(size=15),
            axis.text.x=element_text(size=10),
            axis.title=element_text(size=20,face="bold"),
            strip.text = element_text(size = 20),
            plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))
  }
  print(p)
  
  if(pdf){
    pdf(paste(model_name, "/summary_figures/", graphic_level, "-", model_name, ".pdf", sep = ""), width = 15, height = 10)
    print(p)
    dev.off()
  }
}





##### forecasts plot for NYC ILI ED visits 

c1 <- read.csv("https://raw.githubusercontent.com/reichlab/flu-metrocast/main/target-data/oracle-output.csv")
#c1 <- read.csv("/Users/dk29776/Dropbox/UTAustin/City-Level-Forecasting/NSSP_TX/data/ED_5TX_percent.csv")
c1 <- c1 %>%
  mutate(target_end_date = as.Date(target_end_date)) %>%
  filter(target == "ILI ED visits")

date_list <- c("2025-02-08", "2025-02-15", "2025-02-22", "2025-03-01", "2025-03-08")


forecast_plot(date_list, model_name = "epiENGAGE-GBQR", graphic_level = "NYC_ED", 
              spec = "-", connect = FALSE, pdf = FALSE)
forecast_plot(date_list, model_name = "epiENGAGE-INFLAenza", graphic_level = "NYC_ED", 
              spec = "-", connect = FALSE, pdf = FALSE)



### TX_NSSP_percent
c1 <- read.csv("https://raw.githubusercontent.com/reichlab/flu-metrocast/main/target-data/oracle-output.csv")
c1 <- c1 %>%
  mutate(target_end_date = as.Date(target_end_date)) %>%
  filter(target == "Flu ED visits pct")


date_list <- c("2025-02-08", "2025-02-15", "2025-02-22", "2025-03-01", "2025-03-08")


forecast_plot(date_list, model_name = "epiENGAGE-GBQR", graphic_level = "TX_NSSP_percent", 
              spec = "-", connect = FALSE, pdf = FALSE)
forecast_plot(date_list, model_name = "epiENGAGE-INFLAenza", graphic_level = "TX_NSSP_percent", 
              spec = "-", connect = FALSE, pdf = FALSE)

