cashFlowSimulation = function( I0, L0, D0, R0, C0, sig_S, sig_F, mu_S, mu_F, dist_rate=NULL, dd_rate=NULL, rho=0.5, lambda=0.2, v=0.05, n=100, delta=1/12, t=5) {
  if(is.matrix(dist_rate)) {
    t = nrow(dist_rate) * delta
    n = ncol(dist_rate)
  }
  noises = c(S = "S", F = "F")
  cormat = matrix(c(1, rho, rho, 1), nrow=2)



  delta_mu = mu_S - mu_F
  delta_var_S = sig_S^2 - sig_S * sig_F * rho
  delta_var_F = sig_F^2 - sig_S * sig_F * rho
  var_cor = delta_var_F + delta_var_S

  P0 = I0 + L0

  wL0 = L0 / P0
  wI0 = I0 / P0
  wC0 = C0 / P0

########################
  n=300
  t=2
  dist_rate = simulateCashRate(pef=coreActive, n=n, fit = fit_dist, t = t)
  dd_rate = simulateCashRate(pef=coreActive, n=n, fit = fit_dd, t=t)


  W = lapply(1:n, function(i) mvtnorm::rmvnorm(n = (t / delta), mean = c(0, 0), sigma = cormat))

  Z = lapply(noises, function(z) matrix(data = NA, nrow = t / delta, ncol = n))

  for(i in 1:length(W)) {
    Z$S[, i] = W[[i]][, 1]
    Z$F[, i] = W[[i]][, 2]
  }


  wL = matrix(data = 0, nrow = nrow(Z$S) + 1, ncol = ncol(Z$S))
  wC = wL

  wL[1,] = rep(wL0, ncol(wL))
  wC[1,] = rep(wC0, ncol(wC))

  for(i in 2:nrow(wC)) {
    wC[i, ] = wC[i-1, ] + (v - wC[i-1, ] * (dist_rate[i-1, ] + wL[i-1, ] * mu_S + (1 - wL[i-1, ]) * mu_F)) * delta -
              wC[i-1, ] * sqrt(delta) * (wL[i-1, ] * sig_S * Z$S[i-1, ] + (1 - wL[i-1, ]) * sig_F * Z$F[i-1, ])

    wL[i, ] = wL[i-1, ] + (dist_rate[i-1, ] * (1 - wL[i-1, ]) - dd_rate[i-1, ] * wC[i-1, ] + wL[i-1, ] * (1 - wL[i-1, ]) * (delta_mu - wL[i-1, ] * delta_var_S + (1 - wL[i-1, ]) * delta_var_F)) * delta -
              wL[i-1, ] * (1 - wL[i-1, ]) * (sig_F * Z$F[i-1, ] - sig_S * Z$S[i-1, ])


  }
  wI = 1-wL

 plot(x=1:nrow(wI), y = wI[,1], type = "l", ylim = c(min(wI), max(wI)))
 for(i in 2:ncol(wI)) {
   lines(x=1:nrow(wI), y = wI[, i])
 }

 ########################

}
