simulate.hybrid = function(fit_list, nahead=12, N=1000, weights = rep(1/length(fit_list), length(fit_list)), parallel = T){
  classes = sapply(fit_list, class)

  if(N==1){
    sims = lapply(fit_list,
                  function(f){
                    switch(class(f)[1],
                           'ARIMA' = forecast::simulate.Arima(f, nsim = nahead, future = T),
                           'nnetar' = forecast::simulate.nnetar(f, nsim = nahead, future = T),
                           'stlm' = simulate_stlm(f, nahead = nahead, N = 1))
                  })
    sim_tsp = tsp(sims[[1]])
    ts(data = sapply(sims, function(s)s) %*% weights, start = sim_tsp[1], end = sim_tsp[2], frequency = sim_tsp[3])
  } else {
    if(!parallel) {
      sims = lapply(1:N,
                    function(i) lapply(fit_list,
                                       function(f){
                                         switch(class(f)[1],
                                                'ARIMA' = forecast::simulate.Arima(f, nsim = nahead, future = T),
                                                'nnetar' = forecast::simulate.nnetar(f, nsim = nahead, future = T),
                                                'stlm' = simulate_stlm(f, nahead = nahead, N = 1))
                                       }
                    ))
      sim_tsp = tsp(sims[[1]][[1]])
      ts(data = sapply(sims, function(sim) sapply(sim, function(s)s) %*% weights), start = sim_tsp[1], end = sim_tsp[2], frequency = sim_tsp[3])
    } else {
      ncores = parallel::detectCores() - 1
      cl = parallel::makeCluster(ncores)
      parallel::clusterEvalQ(cl, expr = {library(forecast)})
      parallel::clusterExport(cl, varlist = c("classes", "nahead", "fit_list", "simulate_stlm"))
      sims = parallel::parLapply(cl,
                          X = 1:N,
                          fun = function(i) lapply(fit_list,
                                                   function(f){
                                                     switch(class(f)[1],
                                                            'ARIMA' = forecast::simulate.Arima(f, nsim = nahead, future = T),
                                                            'nnetar' = forecast::simulate.nnetar(f, nsim = nahead, future = T),
                                                            'stlm' = simulate_stlm(f, nahead = nahead, N = 1))
                                                   }))
      parallel::stopCluster(cl)
      sim_tsp = tsp(sims[[1]][[1]])
      ts(data = sapply(sims, function(sim) sapply(sim, function(s)s) %*% weights), start = sim_tsp[1], end = sim_tsp[2], frequency = sim_tsp[3])

    }
  }


}




# s.win = 11
# p=P=4
# s=7
#
# fit_net = forecast::nnetar(y=dist_rate, p=p, P=P, size = s)
# fit_stlm =  forecast::stlm(y = dist_rate, s.window = s.win, method = "ets")
# fit_arima = forecast::Arima(y = dist_rate, order = c(0,1,1), seasonal = c(3, 0, 0))
#
# fit_list = list(arima = fit_arima, stlm = fit_stlm, nnet = fit_net)
#
