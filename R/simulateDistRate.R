simulateDistRate = function(pef, rate="DistributionRate",
                            N=100,
                            fit = NULL,
                            clean = T,
                            t = 12,
                            lambda_bc = F,
                            bootErrs = T,
                            includeNets = F) {
  # rate="DistributionRate"
  # N=100
  # fit = NULL
  # clean = T
  # t=12
  # lambda_bc = F
  #dates = as.Date(row.names(pef@PeriodData)[which(!is.na(pef@PeriodData[, rate]))])
  df_rate = na.omit(get_rate(pef, rate = rate, clean = clean))
  ts_rate = as_ts(df_rate)
  delta = 1/tsp(ts_rate)[3]


  if(is.null(fit)){
    if(lambda_bc){
      lambda = forecast::BoxCox.lambda(ts_rate)
    } else {
      lambda = NULL
    }
    fits = list(arima = forecast::Arima(y = ts_rate, order = c(0, 1, 1), seasonal = c(3, 0, 0), lambda = lambda),
               stlm = forecast::stlm(y = ts_rate, s.window = 13, method = "ets", lambda = lambda))

    if(includeNets){
      fits = append(fits, list(nnetar = forecast::nnetar(y = ts_rate, p = 1, P = 3, size = 3, lambda = lambda)))
    }



  }

  sims = simulate.hybrid(fits, nahead = t, N = N, bootErrs = bootErrs)

  # sim1 = forecast::simulate.Arima(object = fit, nsim = t / delta, future = T)
  # date1 = time(sim1)[1]
  # year1 = round(date1, 0)
  # month1 = (date1 %% 1) / delta
  # date1 = Sys.Date()
  # lubridate::year(date1) = year1
  # lubridate::month(date1) = month1 + 1
  # lubridate::day(date1) = 1
  #
  # dates = lubridate::add_with_rollback(e1 = date1, months(0:(length(sim1)-1)))
  #
  # lubridate::day(dates) = lubridate::days_in_month(dates)
  #
  #   # lubridate::ceiling_date(as.Date(zoo::index(xts::as.xts(sim1))),
  #   #                               unit = "months") - 1
  # sims = sapply(X = 1:n, function(i) {fcast = forecast::simulate.Arima(object = fit, nsim = t / delta, future = T);
  #                                     return(ifelse(fcast>=0, fcast, 0))})
  # sims = data.frame(sims, row.names = dates)

  #actual = as_ts(na.omit(get_rate(pef, rate = rate, clean = F)))

  list(Actual = ts_rate, Simulations = sims)
}

