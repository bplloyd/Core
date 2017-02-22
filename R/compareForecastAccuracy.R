compareForecastAccuracy = function(test, forecasts = NULL, fits=NULL){
  if(is.null(forecasts)){
    h = length(test)
    forecasts = lapply(fits, function(f)forecast::forecast(f, h=h))
  }
  t(sapply(forecasts, function(f)forecast::accuracy(f, test)[2,]))
}

