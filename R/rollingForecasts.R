rollingForecasts = function (y, forecastfunction, h = 12, level = c(80, 95), doPar=T)
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


    parallel::clusterExport(cl, varlist = c("y", "n", "h", "level", "forecastfunction"), envir = environment())
    parallel::clusterEvalQ(cl, expr = {library(forecast) })

  # fcasts = array(NA, dim = c(n, h, 2*length(level) + 3),
  #                dimnames = list(forecast_date = 1:n,
  #                                steps_ahead = 1:h,
  #                                forecast = c("mean", paste0("lower_", level), paste0("upper_", level), "actual", "error")
  #                                )
  #      )
    result = parallel::parLapply(cl,
                                X = seq_len(n),
                                fun = function(i) try(forecastfunction(subset(y,end = i),
                                                                        h = h,
                                                                        level = level),
                                                      silent = TRUE))
    parallel::stopCluster(cl)

    for (i in seq_len(n)) {
      if (!is.element("try-error", class(result[[i]]))) {
        fcasts[i, 1:h, 1] <- result[[i]]$mean[1:h]
        for(j in 1:length(level)) {
          fcasts[i, 1:h, 1 + j] <- result[[i]]$lower[1:h, j]
          fcasts[i, 1:h, 1 + length(level) + j] <- result[[i]]$upper[1:h, j]
        }
        fcasts[i, 1:h, 1 + 2*length(level) + 1] <- y[(i+1):(i+h)]
        fcasts[i, 1:h, 1 + 2*length(level) + 2] <- fcasts[i, 1:h, 1] - y[(i+1):(i+h)]
      }
    }
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
