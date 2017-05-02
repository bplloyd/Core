accuracy_OOS = function(beta_roll, mgr.data, factor.data){
  if(all(names(beta_roll)[2:ncol(beta_roll)] == names(factor.data))){
    mod_pred = xts::xts(sapply(zoo::index(beta_roll)[2:nrow(beta_roll)],
                          function(m) xts::lag.xts(beta_roll)[m, ] %*% c(1, factor.data[m, ])),
                   order.by = zoo::index(beta_roll)[2:nrow(beta_roll)])

    names(mod_pred) = paste0(names(mgr.data), "_pred")

    mean_roll = xts::xts(PerformanceAnalytics::apply.fromstart(mgr.data, mean), order.by = zoo::index(mgr.data))
    mod_comp = na.omit(cbind(mgr.data, xts::lag.xts(mean_roll), mod_pred))
    names(mod_comp)[2] = "Mean"

    mod_resids = mod_comp[, 3] - mod_comp[, 1]
    mod_meanErr = mod_comp[, 2] - mod_comp[, 1]

    rmse = as.numeric(sqrt(mean(mod_resids^2)))
    mae = as.numeric(mean(abs(mod_resids)))
    me = as.numeric(mean(mod_resids))

    tss = as.numeric(sum(mod_meanErr^2))
    rss = as.numeric(sum(mod_resids^2))

    r2_os = as.numeric(1 - (rss/tss))
    return(list(r2_os = r2_os,  sd = sd(mod_comp[,1]), rmse = rmse, mae = mae, me = me))
  }

}

roll_accuracy_OOS = function(beta_roll, mgr.data, factor.data, burnIn = 12){
  nCore = parallel::detectCores()
  cl = parallel::makeCluster(nCore)
  env = environment()

  assign(x = "beta_roll", value = beta_roll, envir = env)
  assign(x = "mgr.data", value = mgr.data, envir = env)
  assign(x = "factor.data", value = factor.data, envir = env)
  assign(x = "burnIn", value = burnIn, envir = env)
  assign(x ="accuracy_OOS", value = accuracy_OOS, envir = env)
  parallel::clusterExport(cl, varlist = c("beta_roll", "mgr.data", "factor.data", "burnIn", "accuracy_OOS"), envir = env)
  parallel::clusterEvalQ(cl, expr = {library(xts)})



  res = parallel::parSapply(cl = cl,
                      X = zoo::index(beta_roll)[burnIn:nrow(beta_roll)],
                      function(d) unlist(accuracy_OOS(beta_roll[paste0('/', as.Date(d)),],
                                  mgr.data[paste0('/', as.Date(d)),],
                                  factor.data[paste0('/', as.Date(d)),])))

  parallel::stopCluster(cl = cl)
  xts::xts(t(res), order.by = zoo::index(beta_roll)[burnIn:nrow(beta_roll)])
}

