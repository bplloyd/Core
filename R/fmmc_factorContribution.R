fmmc_factorContribution = function(mod, factors, assets, weights, B = 1000, boot.method = "random", seed=123, alphaSig = 0.05){
  mod = mod_ls_ss
  assets = lp
  B=10000
  boot.method = "random"
  seed = 123
  alphaSig = 0.05



  beta = as.data.frame(na.fill(mod$beta, 0))
  row.names(beta) = row.names(mod$beta)
  beta
  modsum = summary(mod)
  modsum
  alpha_p = sapply(modsum$sum.list, function(s) s$coefficients[1,4])
  alpha = as.matrix(sapply(1:length(alpha_p),
                           function(i) ifelse(alpha_p[i] <= alphaSig,
                                              as.numeric(mod$alpha[i,]),0)))
  alpha
  fmmc_ret = fmmcSemiParam(B = B,
                           factor.ret = factors['1990/',],
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

  beta_decomp = decompose_beta(beta, weights)
  for(j in 1:(ncol(boot_factors)-1)){
    for(i in 1:nrow(beta_decomp$decomp)){
      if((i == 1) && (j == 1)){
        #boot_factor_decomp = beta_decomp$decomp[i, j] * boot_factors[, j]
        boot_factor_decomp = boot_factors[, j]
      } else {
        #boot_factor_decomp = cbind(boot_factor_decomp, beta_decomp$decomp[i, j] * boot_factors[, j])
        boot_factor_decomp = cbind(boot_factor_decomp, boot_factors[, j])
      }
    }
  }
  for(j in 1:ncol(fmmc_ret$sim.resid)){
    boot_factor_decomp = cbind(boot_factor_decomp, fmmc_ret$sim.resid[, j])
  }

  colnames(boot_factor_decomp) = names(beta_decomp$flattened)
  risk_decomp(names(beta_decomp$flattened),
              weights = beta_decomp$flattened,
              data =  boot_factor_decomp)


  df = as.data.frame(matrix(unlist(strsplit(row.names(factor_risk_decomposed), "_"))[1:120], ncol = 2, byrow = T))
  colnames(df) = c("Factor", "LP")
  df = cbind(df, factor_risk_decomposed$CCTR[1:60],factor_risk_decomposed$`CCTR(%)`[1:60])

  names(df)[3:4] = c("CCTR", "RiskWeight")


  library(ggplot2)

  mypal = c("red", "blue", "orange", "purple", "grey", "black", "yellow", "green", "darkblue", "darkred", "darkgrey")
  #ggplot(df, aes(x = Factor, y = CCTR, fill = LP)) + geom_bar(stat="identity")+ scale_colour_gradientn(colours = "blue") + theme_minimal()
  ggplot(df, aes(x = Factor, y = RiskWeight)) +
    geom_bar(stat="identity") +
    #scale_fill_brewer(palette=mypal) +
    #scale_fill_manual(values = c("red", "blue", "orange", "purple", "lightblue", "black", "pink", "green", "darkblue", "darkred", "gray", "darkgreen")) +
    theme_minimal()
    ggtitle("Risk Weight Decomposition")
}
