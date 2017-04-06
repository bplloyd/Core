ggplot.sim = function(sim, actual, quantiles=NULL, levels=NULL, ylim = NULL, lwd=2,...){
  if(!("ts" %in% class(sim))){
    t0_sim = zoo::as.Date(zoo::as.yearmon(row.names(sim)[1]))
    t0_act = zoo::as.Date(zoo::as.yearmon(row.names(actual)[1]))
    sim = ts(data = sim, start = c(lubridate::year(t0_sim), lubridate::month(t0_sim)), frequency = 12)
    actual = ts(data = actual, start = c(lubridate::year(t0_act), lubridate::month(t0_act)), frequency = 12)
  }

  #get ts time data
  tsp_act = tsp(actual)
  tsp_sim = tsp(sim)
  #calculate quantile probs from levels
  if(is.null(quantiles)){
    probs.upper = 0.5 + levels/200
    probs.lower = 0.5 - levels/200
    probs = c(probs.lower, probs.upper)
    #probs = probs[order(probs)]
    quantiles=ts(t(apply(sim, 1, function(r)quantile(r, probs = probs))), start = tsp_sim[1], frequency = tsp_sim[3])
    #sim_mean = rowMeans(sim)
  } else {
    levels = 100 - 2*sapply(strsplit(colnames(quantiles)[1:2], "%"), function(s)as.numeric(s[1]))
    levels = levels[order(levels)]
  }
  #determine length of simulation and number of sims
  if(is.null(dim(sim))){
    nr = length(sim)
    nc = 1
  } else {
    nr = nrow(sim)
    nc = ncol(sim)
  }

  #get last observation and last time in order to fill the gap
  # lastObs = actual[length(actual)]
  # lastTime = time(actual)[length(actual)]
  # #append the last observation to the beginning
  # # sim_quantiles = rbind(rep(lastObs, ncol(quantiles)), quantiles)
  sim_quantiles = ts(data = rbind(rep(lastObs, ncol(quantiles)), quantiles),
                     start = lastTime,
                     frequency = tsp_sim[3])
  # # sim_mean = c(lastObs, rowMeans(sim))
  sim_mean = ts(data = c(lastObs,rowMeans(sim)),
                start = lastTime,
                frequency = tsp_sim[3])

  df_sim = data.frame(date=zoo::as.yearmon(time(sim_mean)), weight = as.vector(sim_mean))
  nint <- length(levels)
  for(i in 1:nint){
    df_sim = cbind(df_sim, as.vector(sim_quantiles[, i]), as.vector(sim_quantiles[, ncol(sim_quantiles)/2 + i]))
    colnames(df_sim)[(ncol(df_sim)-1):ncol(df_sim)] = c(paste0("lower_", levels[i]), paste0("upper_", levels[i]))

  }

  df_act = data.frame(date = zoo::as.yearmon(time(actual)), weight = as.vector(actual))
  df_act = cbind(df_act, replicate(expr = rep(NA_real_, nrow(df_act)), n = ncol(quantiles)))
  colnames(df_act) = colnames(df_sim)

  df = rbind(cbind(df_act, type = rep("actual", nrow(df_act))),  cbind(df_sim, type = rep("simulation", nrow(df_sim))))

  p = ggplot2::ggplot(df, ggplot2::aes(date, weight))

  p = p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_95, ymax = upper_95), alpha = 0.1)
  p = p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_80, ymax = upper_80), alpha = 0.15)
  p = p + ggplot2::geom_line(ggplot2::aes(color=type))
  p



   #create ts of actual data with times for sims appended as NAs
  ts_actual_alldates = ts(data = c(as.vector(actual), rep(NA, nr)), start = tsp_act[1], frequency = tsp_act[3])




  df = data.frame(date=zoo::as.yearmon(time(ts_actual_alldates)), weight = as.vector(ts_actual_alldates))
  df$type = rep(NA_character_, nrow(df))
  df$type[!is.na(df$weight)] = "actual"
  df[is.na(df$weight), "weight"] = rowMeans(sim)
  df[is.na(df$type), "type"] = "simulation"



  #actual_alldates = c(as.vector(actual), rep(NA, nr))





  #calculate quantiles and mean of the simulated data
  #sim_quantiles = ts(t(apply(sim, 1, function(r)quantile(r, probs = probs))), start = tsp_sim[1], frequency = tsp_sim[3])

  #sim_mean = ts(rowMeans(sim), start = tsp_sim[1], frequency = tsp_sim[3])






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


  if(is.null(ylim)){
    rng = range(sim_quantiles)
    ylim = c(0, round(rng[2], 3) + 0.01)
  }

  x_t = time(ts_actual_alldates)
  at = which((x_t %% 1) == 0)



  #plot actual
 # plot(ts_actual_alldates, ylim = ylim, lwd=1, xaxt = "n")
  plot(ts_actual_alldates, ylim = ylim, lwd=1, xaxt = "n", ...)
  #axis(1, at = at, labels = round(x_t[at], 0))
  axis(1, at =  round(x_t[at], 0), labels = round(x_t[at], 0))


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
