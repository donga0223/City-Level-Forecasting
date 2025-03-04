library(glue)


flu_model_severalcity <- function(epishare = TRUE, ar=1){
  if(epishare == TRUE){
    epimodel = 'f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model = TRUE)'
  }else if(epishare == FALSE){
    epimodel = 'f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, group=city_id,
      control.group=list(model="exchangeable"))'
  }
  if(ar==1){
    weekmodel = 'f(t, model="ar1", hyper=hyper_wk, group=city_id, control.group=list(model="exchangeable"))'
  }else if(ar > 1){
    weekmodel = paste('f(t, model="ar", order = ',ar, ', hyper=hyper_wk, group=city_id, control.group=list(model="exchangeable"))', sep="")
  }
  paste(
    'count ~ 1 +  city +',
    epimodel, ' + ' ,
    weekmodel
  )
}


flu_model_state <- function(epishare = TRUE){
  if(epishare == TRUE){
    epimodel = 'f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model = TRUE)'
  }else if(epishare == FALSE){
    epimodel = 'f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, group=state_id,
      control.group=list(model="exchangeable"))'
  }
  weekmodel = 'f(t, model="ar1", hyper=hyper_wk, group=state_id, control.group=list(model="exchangeable"))'
  paste(
    'count ~ 1 +  state +',
    epimodel, ' + ' ,
    weekmodel
  )
  
}


flu_model_location <- function(epishare = TRUE, ar=1){
  if(epishare == TRUE){
    epimodel = 'f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model = TRUE)'
  }else if(epishare == FALSE){
    epimodel = 'f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, group=location_id,
      control.group=list(model="exchangeable"))'
  }
  if(ar==1){
    weekmodel1 = 'f(t2, model="ar1", hyper=hyper_wk)'
    weekmodel = 'f(t, model="ar1", hyper=hyper_wk, group=location_id, control.group=list(model="exchangeable"))'
  }else if(ar > 1){
    weekmodel1 = paste('f(t2, model="ar", order = ',ar, ', hyper=hyper_wk', sep="")
    weekmodel = paste('f(t, model="ar", order = ',ar, ', hyper=hyper_wk, group=location_id, control.group=list(model="exchangeable"))', sep="")
  }
  paste(
    'count ~ 1 +  location +',
    epimodel, ' + ' ,
    weekmodel1, ' + ' ,
    weekmodel
  )
}

model_formula <- function(
    response="count",
    covars=c("location"),
    seasonal=c("none", "shared", "iid"),
    temporal=c("none", "ar1", "rw1", "rw2"),
    spatial=c("none", "iid", "exchangeable", "besag", "besagproper") # 7/26 you noticed besagproper was indeed faster and also had slighly smaller prediction intervals for covid :)
) {
  if (length(covars) == 0)
    covars <- ""
  else
    covars <- str_c("+ ", covars, collapse=" + ")
  
  seasonal <- case_match(
    seasonal,
    "none" ~ "",
    "shared" ~ '+ f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model=TRUE)',
    "iid" ~ '+ f(epiweek, model="rw2", scale.model=TRUE, cyclic=TRUE, 
            hyper=hyper_epwk, group=iloc, control.group=list(model="iid"))'
  )
  
  scaling <- case_match(
    temporal,
    c("rw1", "rw2") ~ "scale.model=TRUE, ",
    "ar1" ~ ""
  )
  
  weekly_main <- case_match(
    temporal,
    "none" ~ "",
    c("ar1", "rw1", "rw2") ~ glue('+ f(t, model="{temporal}", {scaling}hyper=hyper_wk)')
  )
  
  weekly_interaction <- case_match(
    spatial,
    "none" ~ "",
    "iid" ~ glue('+ f(t2, model="{temporal}", hyper=hyper_wk, 
                     group=iloc, {scaling}control.group=list(model="iid"))'),
    "exchangeable" ~ glue('+ f(t2, model="{temporal}", hyper=hyper_wk, 
                     group=iloc, {scaling}control.group=list(model="exchangeable"))'),
    # "besag" ~ glue('+ f(iloc, model="besag", hyper=hyper_wk, graph=graph, scale.model=TRUE,
    #              group=t2, control.group=list(model="{temporal}"))'), # note the group model automatically scaled
    "besagproper" ~ glue('+ f(iloc, model="besagproper", hyper=hyper_wk, graph=graph,
                     group=t2, control.group=list(model="{temporal}"))')
  )
  
  glue("{response} ~ 1 {covars} {seasonal} {weekly_main} {weekly_interaction}")
}

baseline_rw1 <- function() {
  # 'count ~ 1 + 
  # f(t, model="rw1", group=iloc, scale.model=TRUE, control.group=list(model="iid"))'
  # model_formula(c(), "none", "rw1", "none")
  'count ~ f(t, model="rw1", scale.model=TRUE, hyper=hyper_wk)'
}


flu_model_bren <- function(epishare = TRUE, ar=1){
  if(epishare == TRUE){
    epimodel = 'f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, scale.model = TRUE)'
  }else if(epishare == FALSE){
    epimodel = 'f(epiweek, model="rw2", cyclic=TRUE, hyper=hyper_epwk, group=location_id,
      control.group=list(model="exchangeable"))'
  }
  if(ar==1){
    weekmodel1 = 'f(t, model="ar1", hyper=hyper_wk)'
    weekmodel = 'f(iloc, model="ar1", hyper=hyper_wk, group=t2, control.group=list(model="exchangeable"))'
  }else if(ar > 1){
    weekmodel1 = paste('f(t, model="ar", order = ',ar, ', hyper=hyper_wk', sep="")
    weekmodel = paste('f(iloc, model="ar", order = ',ar, ', hyper=hyper_wk, group=t2, control.group=list(model="exchangeable"))', sep="")
  }
  paste(
    'count ~ 1 +  location +',
    epimodel, ' + ' ,
    weekmodel1, ' + ' ,
    weekmodel
  )
}