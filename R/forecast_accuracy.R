forecast_accuracy = function(resids, horizons = 24:36, errFunc=rmse, lastN=NULL, topN = 10) {
  cnt = min(apply(resids, c(2, 3), function(c) sum(!is.na(c))))
  if(!is.null(lastN)){
    err_meas = apply(resids[,,horizons], c(2, 3), function(col){col = na.omit(col); return(errFunc(col[(length(col)-lastN + 1):length(col)]))})
  } else {
    err_meas = apply(resids[,,horizons], c(2, 3), errFunc)
  }

  return(cbind(err_meas,
               mean_f3 = rowMeans(err_meas[, 1:3]),
               mean_f6 = rowMeans(err_meas[, 1:6]),
               mean_f12 = rowMeans(err_meas),
               mean_l6 = rowMeans(err_meas[, 6:12])))
  #err_meas_norm = apply(resids[(cnt - floor(lastPercent*cnt) + 1):cnt,,horizons], c(2, 3), errFunc)
}


