rollingForecasts4 = function (y, forecastfunction, h = 12, level = c(80, 95), fcast_args=NULL, fitfunction=NULL, fit_args = NULL, doPar=T, pkgs = c("forecast"))
{
  forecastfunction = match.fun(forecastfunction)
  y <- as.ts(y)
  n <- length(y)
  fcasts = array(NA, dim = c(n, h, 2*length(level) + 3),
                 dimnames = list(forecast_date = 1:n,
                                 steps_ahead = 1:h,
                                 forecast = c("mean", paste0("lower_", level), paste0("upper_", level), "actual", "error")
                 )
  )

  if(doPar==T) {
    #print(y)
    nCore = parallel::detectCores()
    cl = parallel::makeCluster(nCore)
    parallel::clusterExport(cl, varlist = c("df_data"))
    #parallel::clusterExport(cl, varlist = c("df_data"), envir = environment())
    parallel::clusterEvalQ(cl, expr = {library(prophet)})
    #parallel::clusterExport(cl, varlist = c("y", "n", "h", "level", "fcast_args", "forecastfunction", "fit_args", "fitfunction"), envir = environment())
    #parallel::clusterEvalQ(cl, expr = {for(p in pkgs){library(p)}})



  # fcasts = array(NA, dim = c(n, h, 2*length(level) + 3),
  #                dimnames = list(forecast_date = 1:n,
  #                                steps_ahead = 1:h,
  #                                forecast = c("mean", paste0("lower_", level), paste0("upper_", level), "actual", "error")
  #                                )
  #      )

    result = parallel::parLapply(cl,
                                X = 36:nrow(df_data),
                                fun = function(i){m = prophet::prophet(df=df_data[1:i,], weekly.seasonality = F)
                                                  future = prophet::make_future_dataframe(m, periods = 13, freq = "m")
                                                  res = predict(m, future)})

    # result = parallel::parLapply(cl,
    #                             X = seq_len(n),
    #                             fun = function(i){
    #                                                 if(is.null(fitfunction)) {
    #                                                   try(do.call(forecastfunction, append(list(y = subset(y,end = i),h = h,level = level), fcast_args)),
    #                                                     silent = TRUE)
    #                                                 } else {
    #                                                   fit = try(do.call(fitfunction, append(list(y=subset(y, end = i)), fit_args)),
    #                                                             silent = TRUE)
    #                                                   try(do.call(forecastfunction, append(list(object = fit, h = h, level = level),
    #                                                                                        fcast_args)),
    #                                                               silent = TRUE)
    #                                                 }
    #                                               })
    parallel::stopCluster(cl)
    return(result)

    # for (i in seq_len(n)) {
    #   if (!is.element("try-error", class(result[[i]]))) {
    #     fcasts[i, 1:h, 1] <- result[[i]]$mean[1:h]
    #     if("lower" %in% names(result[[i]])) {
    #       for(j in 1:length(level)) {
    #         fcasts[i, 1:h, 1 + j] <- result[[i]]$lower[1:h, j]
    #         fcasts[i, 1:h, 1 + length(level) + j] <- result[[i]]$upper[1:h, j]
    #       }
    #     }
    #     fcasts[i, 1:h, 1 + 2*length(level) + 1] <- y[(i+1):(i+h)]
    #     fcasts[i, 1:h, 1 + 2*length(level) + 2] <- fcasts[i, 1:h, 1] - y[(i+1):(i+h)]
    #   }
    # }
    #return(result)

  } else if(doPar==F){
    for (i in seq_len(n)) {
      result <- try(forecastfunction(subset(y,end = i), h = h, level = level), silent = TRUE)
      if (!is.element("try-error", class(result))) {
        fcasts[i, 1:h, 1] <- result$mean[1:h]
        for(j in 1:length(level)) {
          fcasts[i, 1:h, 1 + j] <- result$lower[1:h, j]
          fcasts[i, 1:h, 1 + length(level) + j] <- result$upper[1:h, j]
        }
        fcasts[i, 1:h, 1 + 2*length(level) + 1] <- y[(i+1):(i+h)]
        fcasts[i, 1:h, 1 + 2*length(level) + 2] <- fcasts[i, 1:h, 1] - y[(i+1):(i+h)]
      }
    }

  }
  return(fcasts)
}
