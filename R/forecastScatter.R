forecastScatter = function(fcast, actual, ...){
  x = as.vector(fcast$mean)
  y = as.vector(actual)
  plot(x, y, ...)
  lines(x, x)
}
