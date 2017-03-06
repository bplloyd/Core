simulateCashRate = function(pef, rate="DistributionRate", n=100, fit = NULL, clean = T, t = 5, lambda_bc = F) {

  xts_rate = na.omit(pef@PeriodData[, rate])

  if(clean){
    xts_rate =  PerformanceAnalytics::clean.boudt(xts_rate)[[1]]
  }

  ts_rate = as_ts(xts_rate)
  delta = 1/tsp(ts_rate)[3]


  if(is.null(fit)){
    if(lambda_bc){
      lambda = forecast::BoxCox.lambda(ts_rate)
    } else {
      lambda = NULL
    }
    fit = forecast::auto.arima(ts_rate,
                               max.p = 5,
                               max.q = 5,
                               max.Q = 5,
                               max.P = 5,
                               max.d = 3,
                               max.D = 3,
                               trace = F,
                               ic = "aicc",
                               stepwise = F, parallel = T, lambda = lambda)
  }
  sim1 = forecast::simulate.Arima(object = fit, nsim = t / delta, future = T)
  dates = lubridate::ceiling_date(as.Date(zoo::index(xts::as.xts(sim1))),
                                  unit = "months") - 1
  sims = sapply(X = 1:n, function(i) forecast::simulate.Arima(object = fit, nsim = t / delta, future = T))
  data.frame(Date = dates, sims)
}

