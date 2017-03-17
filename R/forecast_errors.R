forecast_errors = function(fcast, actual) {
  if("forecast" %in% sapply(fcast, class)) {
    component_fcasts = fcast[which(sapply(fcast, class) == "forecast")]
    component_errors = lapply(component_fcasts, function(f) f$mean - actual)
    return(list(hybrid = fcast$mean - actual, components = component_errors))
  } else {
    fcast$mean - actual
  }

}
