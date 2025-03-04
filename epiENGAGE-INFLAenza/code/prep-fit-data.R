library(tidyverse)
library(spdep)


prep_fit_data <- function(df, weeks_ahead=4, ex_lam=population) {
  pred_df <- expand_grid( # makes pairs of new weeks X location
    tibble(
      date = max(df$date) + weeks(1:weeks_ahead),
      #date=duration(1:weeks_ahead, "week") + max(df$date),
      t=1:weeks_ahead + max(df$t),
      epiweek=epiweek(date)
    ),
    distinct(df, location, iloc)
  )
  
  location_data <- df |> 
    slice_max(date) |> 
    distinct(location, ex_lam)
  
  pred_df <- left_join(
    pred_df, location_data, 
    by=c("location"), unmatched="error", relationship="many-to-one"
  ) %>%
    mutate(t2 = t)
  
  pred_df <- bind_rows(df, pred_df) # add to data for counts to be NAs
  return(pred_df)
}

