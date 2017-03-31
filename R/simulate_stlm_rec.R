simulate_stlm_rec = function(stlm_mod, nahead = 12, N = 1, recursive = T, bootstrap = T){
  if(class(stlm_mod$model)[1] == "ARIMA"){
    meth = "arima"
  } else if(class(stlm_mod$model)[1] == "ets"){
    meth = "ets"
  }

  if(stlm_mod$stl$inner == 1){
    rb = T
  } else {
    rb = F
  }
  x = ts(sapply(1:N, function(i)stlm_mod$x), start = start(stlm_mod$x), end = end(stlm_mod$x), frequency = frequency(stlm_mod$x))
  sim_start = tsp(x)[2] + 1/frequency(x)
  new_mod = stlm_mod

  for(i in 1:nahead){

    #seascomp_ts <- tail(new_mod$stl$time.series, 12)[,1]
    seascomp <- as.vector(tail(new_mod$stl$time.series, 12)[,1])
    if(meth == "arima"){
      sim = forecast::simulate.Arima(new_mod$model, nsim = 1, bootstrap = bootstrap, future = T) + as.vector(seascomp)[1]
    } else {
     sim = forecast::simulate.ets(stlm_mod$model, nsim = 1, bootstrap = bootstrap, future = T) + as.vector(seascomp)[1]
    }
    x = ts(c(x, sim), start = start(x), end = end(sim), frequency = frequency(x))
    new_mod = forecast::stlm(y = x, s.window = new_mod$stl$win[1], robust = rb, method = meth, lambda = new_mod$lambda)
  }
  sim = window(x = x, start = sim_start, end = tsp(x)[2])
  return(sim)
}





