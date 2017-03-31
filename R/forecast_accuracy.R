forecast_accuracy = function(resids, horizons = 1:12, errFunc=rmse, lastPercent = 1, topN = 10) {
  cnt = min(apply(resids, c(2, 3), function(c) sum(!is.na(c))))
  err_meas = apply(resids[,,horizons], c(2, 3), errFunc)
  return(cbind(err_meas,  mean_f3 = rowMeans(err_meas[, 1:3]),  mean_f6 = rowMeans(err_meas[, 1:6]), mean_f12 = rowMeans(err_meas), mean_l6 = rowMeans(err_meas[, 6:12])))
  #err_meas_norm = apply(resids[(cnt - floor(lastPercent*cnt) + 1):cnt,,horizons], c(2, 3), errFunc)
}


