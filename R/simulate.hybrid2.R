simulate.hybrid2 = function(fit_list, nahead=12, N=1000, weights = rep(1/length(fit_list), length(fit_list)), parallel = T, bootErrs=T){
  classes = sapply(fit_list, class)

  if(N==1){
    sims = lapply(fit_list,
                  function(f){
                    switch(class(f)[1],
                           'ARIMA' = forecast::simulate.Arima(f, nsim = nahead, future = T, bootstrap = bootErrs),
                           'nnetar' = forecast::simulate.nnetar(f, nsim = nahead, future = T, bootstrap = bootErrs),
                           'stlm' = simulate_stlm_rec(f, nahead = nahead, N = 1, bootstrap = bootErrs))
                  })
    sim_tsp = tsp(sims[[1]])
    ts(data = sapply(sims, function(s)s) %*% weights, start = sim_tsp[1], end = sim_tsp[2], frequency = sim_tsp[3])
  } else {
    if(!parallel) {
      sims = lapply(1:N,
                    function(i) lapply(fit_list,
                                       function(f){
                                         switch(class(f)[1],
                                                'ARIMA' = forecast::simulate.Arima(f, nsim = nahead, future = T, bootstrap = bootErrs),
                                                'nnetar' = forecast::simulate.nnetar(f, nsim = nahead, future = T, bootstrap = bootErrs),
                                                'stlm' = simulate_stlm_rec(f, nahead = nahead, N = 1, bootstrap = bootErrs))
                                       }
                    ))
      sim_tsp = tsp(sims[[1]][[1]])
      ts(data = sapply(sims, function(sim) sapply(sim, function(s)s) %*% weights), start = sim_tsp[1], end = sim_tsp[2], frequency = sim_tsp[3])
    } else {

      env = new.env()
      assign("classes", classes, envir = env)
      assign("nahead", nahead, envir = env)
      assign("fit_list", fit_list, envir = env)
      assign("simulate_stlm_rec", simulate_stlm_rec, envir = env)

      ncores = parallel::detectCores() - 1
      cl = parallel::makeCluster(ncores)
      parallel::clusterEvalQ(cl, expr = {library(forecast)})
      parallel::clusterExport(cl, varlist = c("classes", "nahead", "fit_list", "simulate_stlm_rec", "bootErrs"), envir = env)


      sims = parallel::parLapply(cl,
                          X = 1:N,
                          fun = function(i) lapply(fit_list,
                                                   function(f){
                                                     switch(class(f)[1],
                                                            'ARIMA' = forecast::simulate.Arima(f, nsim = nahead, future = T, bootstrap = bootErrs),
                                                            'nnetar' = forecast::simulate.nnetar(f, nsim = nahead, future = T, bootstrap = bootErrs),
                                                            'stlm' = simulate_stlm_rec(f, nahead = nahead, N = 1, bootstrap = bootErrs))
                                                   }))
      parallel::stopCluster(cl)
      sim_tsp = tsp(sims[[1]][[1]])
      res = list(hybrid = ts(data = sapply(sims, function(sim) sapply(sim, function(s)s) %*% weights), start = sim_tsp[1], end = sim_tsp[2], frequency = sim_tsp[3]),
           #individual = lapply(sims, function(sim) sapply(sim, function(s)s)))
           individual = lapply(1:length(sims[[1]]), function(i)ts(data = sapply(sims,
                                                                                function(sim)sim[[i]]),
                                                                  start = sim_tsp[1],
                                                                  end = sim_tsp[2],
                                                                  frequency = sim_tsp[3])))
      names(res$individual) = names(fit_list)
      return(res)



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
