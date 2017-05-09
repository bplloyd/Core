makeResids = function(mod_list){
  # mgrs = names(mod_list)
  # coefs = lapply(mod_list, function(m)m$asset.fit[[1]]$coefficients)
  # alpha = sapply(coefs, function(v)v[1])
  # resid.sd = sapply(mod_list, function(m)m$resid.sd)
  resids = lapply(mod_list, function(m)m$asset.fit[[1]]$residuals)
  resids_xts = xts(resids[[1]], order.by = as.yearmon(names(resids[[1]])))
  for(j in 2:length(resids)){
    resids_xts = cbind(resids_xts, xts(resids[[j]], order.by = as.yearmon(names(resids[[j]]))))
  }
  names(resids_xts) = names(resids)
  resids_xts
  # names(alpha) = names(resid.sd) = mgrs
  # factor_names = unique(unlist(lapply(coefs, function(v)names(v)[2:length(v)])))
  # beta = matrix(data = 0, nrow = length(mod_list), ncol = length(factor_names))
  # colnames(beta) = factor_names
  # row.names(beta) = mgrs
  # for(n in mgrs){
  #   mgr_beta = coefs[[n]][2:length(coefs[[n]])]
  #   beta[n, names(mgr_beta)] = mgr_beta
  # }
  # return(list(alpha=matrix(alpha,
  #                          ncol = 1,
  #                          dimnames = list(names(alpha),
  #                                          NULL)
  # ),
  # beta=beta,
  # resid.sd = matrix(resid.sd,
  #                   ncol = 1,
  #                   dimnames = list(names(resid.sd),
  #                                   NULL)
  # ),
  # resids = resids_xts))
}
