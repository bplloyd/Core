rollingForecasts5 = function (y, forecastfunction, h = 12, level = c(80, 95), fcast_args=NULL, fitfunction=NULL, fit_args = NULL, doPar=T)
{
  forecastfunction = match.fun(forecastfunction)
  #y <- as.ts(y)
  if(is.ts(y)) {
    n <- length(y)
  } else {
    n = nrow(y)
  }
  # fcasts = array(NA, dim = c(n, h, 2*length(level) + 3),
  #                dimnames = list(forecast_date = 1:n,
  #                                steps_ahead = 1:h,
  #                                forecast = c("mean", paste0("lower_", level), paste0("upper_", level), "actual", "error")
  #                )
  # )

  if(doPar==T) {
    #print(y)
    nCore = parallel::detectCores()
    cl = parallel::makeCluster(nCore)
    parallel::clusterExport(cl, varlist = c("df_data"))
    parallel::clusterEvalQ(cl, expr = {library(prophet); library(zoo)})

    #parallel::clusterExport(cl, varlist = c("y", "n", "h", "level", "fcast_args", "forecastfunction", "fit_args", "fitfunction"), envir = environment())
    #parallel::clusterEvalQ(cl, expr = {library(forecast) })

  fcasts = array(NA, dim = c(n, h, 2*length(level) + 3),
                 dimnames = list(forecast_date = 1:n,
                                 steps_ahead = 1:h,
                                 forecast = c("mean", paste0("lower_", level), paste0("upper_", level), "actual", "error")
                                 )
       )

    if(is.ts(y)) {
      results = parallel::parLapply(cl,
                                  X = seq_len(n),
                                  fun = function(i){
                                                      if(is.null(fitfunction)) {
                                                        try(do.call(forecastfunction, append(list(y = subset(y, end = i),h = h,level = level), fcast_args)),
                                                          silent = TRUE)
                                                      } else {
                                                        fit = try(do.call(fitfunction, append(list(y=subset(y, end = i)), fit_args)),
                                                                  silent = TRUE)
                                                        try(do.call(forecastfunction, append(list(object = fit, h = h, level = level),
                                                                                             fcast_args)),
                                                                    silent = TRUE)
                                                      }
                                                    })
    } else {
      # results = parallel::parLapply(cl,
                                    # X = seq_len(n),
                                    # fun = function(i){
                                    #   if(is.null(fitfunction)) {
                                    #     try(do.call(forecastfunction, append(list(y = subset(y,ds <= y$ds[i]),h = h), fcast_args)),
                                    #         silent = TRUE)
                                    #   } else {
                                    #     fit = try(do.call(fitfunction, append(list(y = subset(y,ds <= y$ds[i])), fit_args)),
                                    #               silent = TRUE)
                                    #     try(do.call(forecastfunction, append(list(object = fit, h = h),
                                    #                                          fcast_args)),
                                    #         silent = TRUE)
                                    #   }
                                    # })

      results = lapply(37:n,
                                    FUN = function(i){ m = try(prophet::prophet(df = subset(df_data, ds <= df_data$ds[i]), yearly.seasonality = T));
                                                       future = try(prophet::make_future_dataframe(m, 13, freq = "m"));
                                                       future =  try(as.Date.numeric(ifelse(lubridate::day(future$ds) < 10, future$ds - lubridate::day(future$ds), future$ds)));
                                                       return(try(predict(m, future)))


                                                      })
    }

    results = vector(mode = "list", length = nrow(df_data)-37+1)
    for(i in 37:nrow(df_data)) {
      m = try(prophet::prophet(df = subset(df_data, ds <= df_data$ds[i]), yearly.seasonality = T));
      future = try(prophet::make_future_dataframe(m = m, periods = 13, freq = "m", include_history = F));
      future$ds =  try(zoo::as.Date.numeric(ifelse(lubridate::day(future$ds) < 10, future$ds - lubridate::day(future$ds), future$ds)));
      results[[i-37+1]]=try(predict(object = m, df = future))
      saveRDS(object = results, file = "prophet_results.rds")
    }
    parallel::stopCluster(cl)

    for (i in 37:nrow(df_data)) {
      if (!is.element("try-error", class(results[[i-37+1]]))) {
        fcasts[i, 1:h, 1] <- results[[i-37+1]]$yhat[1:h]
        if("lower" %in% names(results[[i-37+1]])) {
          for(j in 1:length(level)) {
            fcasts[i, 1:h, 1 + j] <- results[[i-37+1]][1:h,"yhat_lower"]
            fcasts[i, 1:h, 1 + length(level) + j] <- results[[i-37+1]][1:h,"yhat_upper"]
          }
        }
        fcasts[i, 1:h, 1 + 2*length(level) + 1] <- y[(i+1):(i+h), "y"]
        fcasts[i, 1:h, 1 + 2*length(level) + 2] <- fcasts[i, 1:h, 1] - y[(i+1):(i+h), "y"]
      }
    }
    return(result)

  } else if(doPar==F){
    results = lapply(1:seq_len(n), FUN = try(forecastfunction(subset(y,end = i), h = h, level = level), silent = TRUE))
    # for (i in seq_len(n)) {
      # result <- try(forecastfunction(subset(y,end = i), h = h, level = level), silent = TRUE)
      # if (!is.element("try-error", class(result))) {
      #   fcasts[i, 1:h, 1] <- result$mean[1:h]
      #   for(j in 1:length(level)) {
      #     fcasts[i, 1:h, 1 + j] <- result$lower[1:h, j]
      #     fcasts[i, 1:h, 1 + length(level) + j] <- result$upper[1:h, j]
      #   }
      #   fcasts[i, 1:h, 1 + 2*length(level) + 1] <- y[(i+1):(i+h)]
      #   fcasts[i, 1:h, 1 + 2*length(level) + 2] <- fcasts[i, 1:h, 1] - y[(i+1):(i+h)]
      # }
    # }

  }
  return(results)
}
