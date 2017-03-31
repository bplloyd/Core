plot.sim = function(sim, actual, quantiles, ylim = NULL, lwd=2,...){
  #calculate quantile probs from levels
  # probs.upper = 0.5 + levels/200
  # probs.lower = 0.5 - levels/200
  # probs = c(probs.lower, probs.upper)
  # probs = probs[order(probs)]
  levels = 100 - 2*sapply(strsplit(colnames(quantiles)[1:2], "%"), function(s)as.numeric(s[1]))
  levels = levels[order(levels)]
  #get ts time data
  tsp_act = tsp(actual)
  tsp_sim = tsp(sim)


  #determine length of simulation and number of sims
  if(is.null(dim(sim))){
    nr = length(sim)
    nc = 1
  } else {
    nr = nrow(sim)
    nc = ncol(sim)
  }


  #create ts of actual data with times for sims appended as NAs
  ts_actual_alldates = ts(data = c(as.vector(actual), rep(NA, nr)), start = tsp_act[1], frequency = tsp_act[3])

  #get last observation and last time in order to fill the gap
  lastObs = actual[length(actual)]
  lastTime = time(actual)[length(actual)]


  #calculate quantiles and mean of the simulated data
  #sim_quantiles = ts(t(apply(sim, 1, function(r)quantile(r, probs = probs))), start = tsp_sim[1], frequency = tsp_sim[3])

  #sim_mean = ts(rowMeans(sim), start = tsp_sim[1], frequency = tsp_sim[3])


  #append the last observation to the beginning
  sim_quantiles = ts(data = rbind(rep(lastObs, ncol(quantiles)), quantiles),
                     start = lastTime,
                     frequency = tsp_sim[3])

  sim_mean = ts(data = c(lastObs, sim),
                start = lastTime,
                frequency = tsp_sim[3])



  #colors for prediction intervals
  # Using very small confidence levels.
  if(min(levels) < 50){
    shadecols <- rev(colorspace::sequential_hcl(100)[levels])
  }
  # This should happen almost all the time. Colors mapped to levels.
  else {
    shadecols <- rev(colorspace::sequential_hcl(52)[levels-49])
  }

  xxx <- time(sim_quantiles)
  nint <- ncol(sim_quantiles)/2

  if(is.null(ylim)){
    rng = range(sim_quantiles)
    ylim = c(0, round(rng[2], 3) + 0.01)
  }


  #plot actual
  plot(ts_actual_alldates, ylim = ylim,  ...)

  #plot PIs
  for(i in 1:nint) {
    vec = c(sim_quantiles[, i], rev(sim_quantiles[, 2*nint - i + 1]))
    polygon(c(xxx,rev(xxx)), vec,
            col=shadecols[i], border=FALSE)
  }
  #plot mean
  lines(sim_mean, col = "blue", lwd = lwd)
}

# levels = seq(10, 90, 10)
#
#
#
#
# sim_hyb = dist_sim$Simulations$hybrid
# sim_ar = dist_sim$Simulations$individual$arima
# sim_stlm  = dist_sim$Simulations$individual$stlm
# #sim_nets = dist_sim$Simulations$individual$nnetar
#
# act = dist_sim$Actual
#
#
# if(is.null(dim(sim))){
#   nr = length(sim)
#   nc = 1
# } else {
#   nr = nrow(sim)
#   nc = ncol(sim)
# }
#
# #ts_actual_alldates = ts(data = rbind(replicate(expr = as.vector(act), n = nc), replicate(expr = rep(NA, nr), n=nc)), start = tsp_act[1], frequency = tsp_act[3])
# ts_actual_alldates = ts(data = c(as.vector(act), rep(NA, nr)), start = tsp_act[1], frequency = tsp_act[3])
#
# lastObs = act[length(act)]
# lastTime = time(act)[length(act)]
#
# sim_quantiles = ts(t(apply(sim, 1, function(r)quantile(r, probs = probs))), start = tsp_sim[1], frequency = tsp_sim[3])
# sim_mean = ts(rowMeans(sim), start = tsp_sim[1], frequency = tsp_sim[3])
#
# sim_quantiles = ts(data = rbind(rep(lastObs, ncol(sim_quantiles)), sim_quantiles),
#                    start = lastTime,
#                    frequency = tsp_sim[3])
#
# sim_mean = ts(data = c(lastObs,sim_mean),
#                    start = lastTime,
#                    frequency = tsp_sim[3])
#
#
# # Using very small confidence levels.
# if(min(x$level) < 50){
#   shadecols <- rev(colorspace::sequential_hcl(100)[levels])
# }
# # This should happen almost all the time. Colors mapped to levels.
# else {
#   shadecols <- rev(colorspace::sequential_hcl(52)[levels-49])
# }
#
# #xxx <- time(x$upper)
# xxx <- time(sim_quantiles)
#
# #idx <- rev(order(x$level))
# #idx <- rev(order(x$level))
#
# #nint <- length(x$level)
# nint <- length(levels)
#
#
# plot(ts_actual_alldates, ylim = c(0, .04))
# for(i in 1:nint) {
#   vec = c(sim_quantiles[, i], rev(sim_quantiles[, 2*nint - i + 1]))
#   polygon(c(xxx,rev(xxx)), vec,
#           col=shadecols[i], border=FALSE)
# }
#
# # else if(shaded)
# # {
# #
# #   # polygon(c(xxx,rev(xxx)), c(x$lower[,idx[i]],rev(x$upper[,idx[i]])),
# #   #         col=shadecols[i], border=FALSE)
# # }
#
#
#
#
#
#
#
#
#
# # for(i in 1:ncol(sim)){
# #   lines(sim[, i], col = "blue")
# # }
