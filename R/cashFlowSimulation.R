cashFlowSimulation = function( I0, L0, D0, R0, C0, sig_S, sig_F, mu_S, mu_F, dist_rate=NULL, dd_rate=NULL, rho=0.5, lambda=0.2, v=0.01, n=100, delta=1/12, t=5) {
  if(is.matrix(dist_rate)) {
    t = nrow(dist_rate) * delta
    n = ncol(dist_rate)
  }
  dists = get_series(pef, "Distributions_Total")
  calls =  get_series(pef, "Calls_Total_Gross")

  I0 = as.numeric(allocation_dollars[nrow(allocation_dollars), "ILLIQUID"])/1000000
  L0 = as.numeric(allocation_dollars[nrow(allocation_dollars), "LIQUID"])/1000000
  C0 = as.numeric(pef@PeriodData[nrow(pef@PeriodData), "Undrawn"])
  D0 = as.numeric(cumsum(calls)[nrow(calls),])
  R0 = as.numeric(cumsum(dists)[nrow(dists),])

  sig_S = 0.04
  mu_S = 0.01
  sig_F = 0.05
  mu_F = 0.01
  rho = 0.5
  lambda = 0.2
  v = 0.01
  n = 1000
  delta=1/12
  t=5

  dist_rate = simulateCashRate(pef, n=n, t=t, lambda_bc = T)
  dist_sims = as.matrix((1/delta) *  dist_rate$Simulations)

  dd_rate = simulateCashRate(coreActive, rate = "DrawdownRate", n=n, t=t, lambda_bc = F)
  dd_sims = as.matrix((1/delta) * dd_rate$Simulations)

  date0 = as.Date(row.names(dist_rate$Actual)[nrow(dist_rate$Actual)])
  month0 = lubridate::month(date0)

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
  set.seed(123)
  W = lapply(1:n, function(i) mvtnorm::rmvnorm(n = (t / delta), mean = c(0, 0), sigma = cormat))

  Z = lapply(noises, function(z) matrix(data = NA, nrow = t / delta, ncol = n))

  for(i in 1:length(W)) {
    Z$S[, i] = W[[i]][, 1]
    Z$F[, i] = W[[i]][, 2]
  }


  L = matrix(data = 0, nrow = nrow(Z$S) + 1, ncol = ncol(Z$S))
  row.names(L) = c(row.names(dist_rate$Actual)[nrow(dist_rate$Actual)], row.names(dist_sims))
  redemptions = which(lubridate::month(as.Date(row.names(L))) %in% c(5, 8, 11, 2))
  # wL = matrix(data = 0, nrow = nrow(Z$S) + 1, ncol = ncol(Z$S))
  # row.names(wL) = c(row.names(dist_rate$Actual)[nrow(dist_rate$Actual)], row.names(dist_sims))
  # redemptions = which(lubridate::month(as.Date(row.names(wL))) %in% c(5, 8, 11, 2))

  I = L
  C = L
  P = L
  wI = L
  wL = L
  wC = L
  #wC = wL

  L[1, ] = rep(L0, ncol(L))
  I[1, ] = rep(I0, ncol(I))
  C[1,] = rep(C0, ncol(C))
  P[1,] = rep(P0, ncol(P))
  wI[1,] = rep(wI0, ncol(wI))
  wL[1,] = rep(wL0, ncol(wL))
  wC[1,] = rep(wC0, ncol(wC))
  # wL[1,] = rep(wL0, ncol(wL))
  # wC[1,] = rep(wC0, ncol(wC))



  for(i in 2:nrow(C)) {
    C[i, ] = C[i-1, ] + (P[i-1, ] * v - dd_sims[i-1, ] * C[i-1, ]) * delta

    L[i, ] = L[i-1, ] + L[i-1, ] * (mu_S * delta + sig_S * sqrt(delta) * Z$S[i-1, ]) -
      dd_sims[i-1, ] * C[i-1, ] * delta + dist_sims[i-1, ] * I[i-1, ] * delta

    if(i %in% redemptions){
      L[i, ] = L[i, ] - lambda * P[i - 2, ] * delta
    }

    I[i, ] = I[i-1, ] + I[i-1, ] * ((mu_F - dist_sims[i-1, ]) * delta + sig_F * sqrt(delta) * Z$F[i-1, ]) +
      dd_sims[i-1, ] * C[i-1, ] * delta

    P[i, ] = L[i, ] + I[i, ]
    wL[i, ] = L[i, ] / P[i, ]
    wI[i, ] = I[i, ] / P[i, ]
    wC[i, ] = C[i, ] / P[i, ]

  }

  # wI_std = wI
  # wL_std = wL
  #
  # for(i in 2:nrow(wC)) {
  #   wC[i, ] = wC[i-1, ] + (v - wC[i-1, ] * (dist_sims[i-1, ] + wL[i-1, ] * mu_S + (1 - wL[i-1, ]) * mu_F)) * delta -
  #             wC[i-1, ] * sqrt(delta) * (wL[i-1, ] * sig_S * Z$S[i-1, ] + (1 - wL[i-1, ]) * sig_F * Z$F[i-1, ])
  #
  #   wL[i, ] = wL[i-1, ] + (dist_sims[i-1, ] * (1 - wL[i-1, ]) - dd_sims[i-1, ] * wC[i-1, ] + wL[i-1, ] * (1 - wL[i-1, ]) * (delta_mu - wL[i-1, ] * delta_var_S + (1 - wL[i-1, ]) * delta_var_F)) * delta -
  #             wL[i-1, ] * (1 - wL[i-1, ]) * (sig_F * Z$F[i-1, ] - sig_S * Z$S[i-1, ]) * sqrt(delta)
  #
  #
  #   # if(i %in% redemptions){
  #   #   wL[i, ] = wL[i, ] - 0.05 * wL[i - 2, ]
  #   #
  #   # }
  #
  # }
  # wI = 1-wL

  # row.names(wL) = c(row.names(dist_rate$Actual)[nrow(dist_rate$Actual)], row.names(dist_sims))
  #



  matplot(wI, type = "l")
  matplot(P, type = "l")
 ########################

}
