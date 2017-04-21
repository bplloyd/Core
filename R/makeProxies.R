makeProxies = function(R, factors, beta, resid.par = NULL, resids = NULL, alpha=NULL, bootResids = T, numResamples = 100){
  proxys = backfilled = vector(mode = "list", length = ncol(R))
  names(proxys) = names(backfilled) = names(R)

  for(n in names(R)){
    needsProxy = index(R)[is.na(R[, n])]
    proxyBeta = beta[n,beta[n,] != 0, drop = F]
    proxy = xts(factors[, colnames(proxyBeta)] %*% t(proxyBeta[,,drop=F]), order.by = index(factors))

    if(!is.null(alpha)){
      proxy = proxy + alpha[n, ]
    }

    for(i in 1:numResamples){
      if(bootResids & !is.null(resids)){
        proxy_samp = proxy + resids[[n]][sample(length(resids[[n]]), size = nrow(proxy), replace = T)]
      } else {
        #proxy = proxy + rnorm(n = nrow(proxy), mean = alpha[n,], sd = resid.par[n, ])
        proxy_samp = proxy + rnorm(n = nrow(proxy), mean = 0, sd = resid.par[n, ])
      }
      if(i ==1){
        proxy_res = proxy_samp
      } else {
        proxy_res = cbind(proxy_res, proxy_samp)
      }
    }

    proxys[[n]] = xts(rowMeans(proxy_res), order.by = index(proxy_res))
    backfilled[[n]] = rbind(proxy[needsProxy, ], na.omit(R[, n]))
    names(proxys[[n]]) = names(backfilled[[n]]) = n
  }

  proxy_comps = proxys
  for(n in names(R)){
    proxy_comps[[n]] = na.omit(cbind(R[, n], proxy_comps[[n]]))
    names(proxy_comps[[n]]) = c(n, paste0(n, "-proxy"))
  }

  bf_xts = backfilled[[1]]
  p_xts = proxys[[1]]
  if(ncol(R) > 1){
    for(i in 2:ncol(R)){
      bf_xts = cbind(bf_xts, backfilled[[i]])
      p_xts = cbind(p_xts, proxys[[i]])
    }
  }

  list(R = R, backfilled = bf_xts, proxys = p_xts, proxy_comps = proxy_comps)
  #sim.fund.ret[, i] = alpha[i, 1] + (boot.factor.ret %*% t(beta[i, , drop = FALSE])) + sim.resid[, i]

}
