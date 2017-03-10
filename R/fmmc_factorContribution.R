fmmc_factorContribution = function(mod, factors, assets, weights, B = 1000, boot.method = "random", seed=123, alphaSig = 0.05){
  beta = as.data.frame(na.fill(mod$beta, 0))
  row.names(beta) = row.names(mod$beta)
  modsum = summary(mod)
  alpha_p = sapply(modsum$sum.list, function(s) s$coefficients[1,4])
  alpha = as.matrix(sapply(1:length(alpha_p),
                           function(i) ifelse(alpha_p[i] <= alphaSig,
                                              as.numeric(mod$alpha[i,]),0)))
  fmmc_ret = fmmcSemiParam(B = B,
                           factor.ret = factors['2014/',],
                           beta = beta,
                           alpha = alpha,
                           resid.par = as.matrix(mod$resid.sd),
                           boot.method = boot.method, seed = seed)


  if(is.matrix(fmmc_ret$boot.factor.ret)){
    boot_factors = xts(fmmc_ret$boot.factor.ret, order.by = as.Date(as.yearmon(row.names(fmmc_ret$boot.factor.ret)[1])) + 1:nrow(fmmc_ret$boot.factor.ret))
  } else {
    boot_factors = xts(fmmc_ret$boot.factor.ret, order.by = as.Date(as.yearmon(names(fmmc_ret$boot.factor.ret)[1])) + 1:length(fmmc_ret$boot.factor.ret))
  }
  boot_factors = cbind(boot_factors, resid = xts((fmmc_ret$sim.resid %*% weights), order.by = index(boot_factors)))

  factor_weights = c(as.vector(weights %*% na.fill(as.matrix(beta), 0)), 1)
  names(factor_weights) = c(colnames(beta), "residual")
  names(boot_factors) = names(factor_weights)

  factor_risk = risk_decomp(names(factor_weights),
                                 weights = factor_weights,
                                 data = boot_factors)
}
