
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
