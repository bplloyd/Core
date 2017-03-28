simulate_stlm = function(stlm_mod, nahead=12, N = 1000, recursive = F) {
  sa <- forecast::seasadj(stlm_mod$stl)
  seascomp <- as.vector(tail(stlm_mod$stl$time.series, 12)[,1])
  if((nahead/length(seascomp)) > 1){
    rep = nahead/length(seascomp)
    for(i in 1:(rep - 1)){
      seascomp = c(seascomp, seascomp[1:12])
    }
  }

  # sim <- matrix(0, nrow=nahead, ncol=N)
  # for(i in 1:N)
  #   sim[,i] <- simulate(stlm_mod$model, nsim=nahead) + as.vector(seascomp)
  if(class(stlm_mod$model)[1] == "ets"){
    sim = sapply(1:N, function(i)forecast::simulate.ets(stlm_mod$model, nsim = nahead) + as.vector(seascomp))
    sim_tsp = tsp(forecast::simulate.ets(stlm_mod$model, nsim = nahead))
  } else {
    sim = sapply(1:N, function(i)forecast::simulate.Arima(stlm_mod$model, nsim = nahead) + as.vector(seascomp))
    sim_tsp = tsp(forecast::simulate.Arima(stlm_mod$model, nsim = nahead))
  }
  return(ts(data = sim, start = sim_tsp[1], end = sim_tsp[2], frequency = sim_tsp[3]))
}





