cashFlowSimulation = function(I0, L0, D0, R0, C0, sig_S, sig_F, mu_S, mu_F, dist_rate=NULL, dd_rate=NULL, rho=0.5, lambda=0.2, v=0.01, n=100, delta=1/12, t=5) {
  if(is.matrix(dist_rate)) {
    t = nrow(dist_rate) * delta
    n = ncol(dist_rate)
  }
  #pef = coreActive
  #------------------parameter setup for simulation
  sig_S = 0.08
  mu_S = 0.04
  sig_F = 0.1
  mu_F = 0.06
  rho = 0.5
  lambda = 0.2
  v = 0
  n = 1000
  delta=1/12
  t=5
  delta_mu = mu_S - mu_F
  delta_var_S = sig_S^2 - sig_S * sig_F * rho
  delta_var_F = sig_F^2 - sig_S * sig_F * rho
  var_cor = delta_var_F + delta_var_S
  noises = c(S = "S", F = "F")
  cormat = matrix(c(1, rho, rho, 1), nrow=2)

  hist_weights = as.matrix(allocation_weights[, c("ILLIQUID", "LIQUID")])
  row.names(hist_weights) = as.character(zoo::as.yearmon(allocation_weights$Date))

  #------------------historical parameters
  dists = get_series(coreFund, "Distributions_Total", clean  = F)
  calls =  get_series(coreFund, "Calls_Total_Gross", clean = F)

  I0 = as.vector((allocation_dollars[(nrow(allocation_dollars) - 1):nrow(allocation_dollars), "ILLIQUID"]/1000000)[, 1])
  #Im1 = as.numeric(allocation_dollars[nrow(allocation_dollars) - 1, "ILLIQUID"])/1000000

  L0 = as.vector((allocation_dollars[(nrow(allocation_dollars) - 1):nrow(allocation_dollars), "LIQUID"]/1000000)[, 1])
 # Lm1 =

  C0 = as.vector(coreActive@PeriodData[(nrow(coreActive@PeriodData) - 1):nrow(coreActive@PeriodData), "Undrawn"])
  #Cm1 = as.numeric(coreFund@PeriodData[nrow(coreFund@PeriodData) - 1, "Undrawn"])

  D0 = as.vector(cumsum(calls)[(nrow(calls)-1):nrow(calls),])
  #Dm1 = as.numeric(cumsum(calls)[nrow(calls) - 1,])

  R0 = as.vector(cumsum(dists)[(nrow(dists)-1):nrow(dists),])
  #Rm1 = as.numeric(cumsum(dists)[nrow(dists) - 1,])



  #Pm1 = Im1 + Lm1
  P0 = I0 + L0

  #wLm1= Lm1 / Pm1
  wL0 = L0 / P0

  #wIm1 = Im1 / Pm1
  wI0 = I0 / P0

  #wCm1 = Cm1 / Pm1
  wC0 = C0 / P0

  date0 = allocation_dollars$Date[nrow(allocation_dollars)]
  month0 = lubridate::month(date0)


  #-------------------data, fit, and simulations for distribution rate
  dist_rate = as_ts(get_series(coreActive, "DistributionRate"))
  lam = forecast::BoxCox.lambda(dist_rate)
  fit_list = list(arima = forecast::Arima(y = dist_rate, order = c(0, 1, 1), seasonal = c(3, 0, 0), lambda = lam),
                  stlm = forecast::stlm(y = dist_rate, s.window = 11, method = "ets", etsmodel = "AAN", lambda = lam),
                  net = forecast::nnetar(y = dist_rate, p = 1, P = 3, lambda = lam))
  fit_sim = simulate.hybrid(fit_list, nahead = t*12, N = n)

  dist_sims = as.matrix((1/delta) *  fit_sim$hybrid$simulations)



  #-------------------data, fit, and simulations for drawdown rate
  dd_rate = get_series(coreActive, "DrawdownRate")
  dd_rate = as_ts(dd_rate)
  fit_dd = forecast::ets(y = window(dd_rate, start = 2006)
                         #lambda = forecast::BoxCox.lambda(window(dd_rate, start= 2006))
                         )
  dd_sims = (1/delta)*sapply(1:n,
                   function(i) forecast::simulate.ets(object = fit_dd, nsim = t*12, future = T, bootstrap = T))

########################
  set.seed(123)
  W = lapply(1:n, function(i) mvtnorm::rmvnorm(n = (t / delta), mean = c(0, 0), sigma = cormat))

  Z = lapply(noises, function(z) matrix(data = NA, nrow = t / delta, ncol = n))

  for(i in 1:length(W)) {
    Z$S[, i] = W[[i]][, 1]
    Z$F[, i] = W[[i]][, 2]
  }



  #L = liquid allocation
  L = matrix(data = 0, nrow = nrow(Z$S) + 2, ncol = ncol(Z$S))
  row.names(L) = as.character(c(zoo::as.yearmon(allocation_dollars$Date[(nrow(allocation_dollars)-1):nrow(allocation_dollars)]),
                                zoo::as.yearmon(time(dist_sims))))
  redemptions = which(lubridate::month(zoo::as.Date(zoo::as.yearmon(row.names(L)))) %in% c(5, 8, 11, 2))
  # wL = matrix(data = 0, nrow = nrow(Z$S) + 1, ncol = ncol(Z$S))
  # row.names(wL) = c(row.names(dist_rate$Actual)[nrow(dist_rate$Actual)], row.names(dist_sims))
  # redemptions = which(lubridate::month(as.Date(row.names(wL))) %in% c(5, 8, 11, 2))

  simulations = array(data = NA_real_,
                      dim = c(nrow(Z$S) + 2, ncol(Z$S), 6),
                      dimnames = list(NULL, NULL, c("I", "L", "C", "P", "D", "R")))

  #I = liquid allocation
  #C = undrawn commitment
  #P = total fund nav
  #L = illiquid weight
  #L = liquid weight
  #C = undrawn commitment weight
  #D = drawdowns
  #R = distributions

  simulations[1:2, , "L"] = replicate(expr = L0, n = ncol(simulations))
  simulations[1:2, , "I"] = replicate(expr = I0, n = ncol(simulations))
  simulations[1:2, , "C"] = replicate(expr = C0, n = ncol(simulations))
  simulations[1:2, , "P"] = replicate(expr = P0, n = ncol(simulations))
  simulations[1:2, , "D"] = replicate(expr = D0, n = ncol(simulations))
  simulations[1:2, , "R"] = replicate(expr = R0, n = ncol(simulations))


  for(h in 1:nrow(dd_sims)) {
    i = h + 2

    #Liquid return
    R_L = (mu_S * delta + sig_S * sqrt(delta) * Z$S[h, ])

    #Illiquid return
    R_I = (mu_F * delta + sig_F * sqrt(delta) * Z$F[h, ])

    #Distributions
    simulations[i, , "R"]  =  simulations[i-1, , "R"]  +  delta * dist_sims[h, ] * simulations[i-1, , "I"]

    #Drawdowns
    simulations[i, , "D"]  =  simulations[i-1, , "D"]  +  delta * dd_sims[h, ] * simulations[i-1, , "C"]

    #Undrawn Capital Commitment
    simulations[i, , "C"]  =  simulations[i-1, , "C"]  +  simulations[i-1, , "P"] * v * delta  -  (simulations[i, , "D"] - simulations[i-1, , "D"])

    #Liquid Allocation
    simulations[i, , "L"]  =  simulations[i-1, , "L"] * (1 + R_L)  -  (simulations[i, , "D"] - simulations[i-1, , "D"])  +  (simulations[i, , "R"] - simulations[i-1, , "R"])

    #Subtract redemptions from liquid allocation
    if(i %in% redemptions){
      simulations[i, , "L"]  =  simulations[i, , "L"]  -  0.25 * lambda * simulations[i-2, , "P"]
    }

    #Illiquid Allocation
    simulations[i, , "I"]  =  simulations[i-1, , "I"] * (1 + R_I)  -   (simulations[i, , "R"] - simulations[i-1, , "R"])  +  (simulations[i, , "D"] - simulations[i-1, , "D"])

    #Total fund NAV
    simulations[i, , "P"]  =  simulations[i, , "L"]  +  simulations[i, , "I"]


    #simulations[i, , "C"] = simulations[i-1, , "C"] + (simulations[i-1, , "P"] * v  - dd_sims[h, ] * simulations[i-1, , "C"]) * delta
    # simulations[i, , "L"] = simulations[i-1, , "L"] + simulations[i-1, , "L"] * (mu_S * delta + sig_S * sqrt(delta) * Z$S[h, ]) -
    #                     dd_sims[h, ] * simulations[i-1, , "C"] * delta +
    #                     dist_sims[h, ] * I[i-1, ] * delta
    # simulations[i, , "L"] = simulations[i-1, , "L"] + simulations[i-1, , "L"] * R_L -
    #                     dd_sims[h, ] * simulations[i-1, , "C"] * delta +
    #                     dist_sims[h, ] * I[i-1, ] * delta
    # I[i, ] = I[i-1, ] + I[i-1, ] * (R_I - dist_sims[h, ] * delta) +
    #   dd_sims[h, ] * simulations[i-1, , "C"] * delta

    # I[i, ] = I[i-1, ] + I[i-1, ] * ((mu_F - dist_sims[h, ]) * delta + sig_F * sqrt(delta) * Z$F[h, ]) +
    #   dd_sims[h, ] * simulations[i-1, , "C"] * delta
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



  matplot(simulations[, , "I"]/simulations[, , "P"], type = "l", main = "Illiquid Weight")
  matplot(simulations[, , "L"]/simulations[, , "P"], type = "l", main = "Liquid Weight")
  matplot(simulations[, , "C"]/simulations[, , "P"], type = "l", main = "Undrawn Committed Capital Weight")

  matplot(simulations[, , "I"], type = "l", main = "Illiquid NAV")
  matplot(simulations[, , "L"], type = "l", main = "Liquid NAV")
  matplot(simulations[, , "C"], type = "l", main = "Undrawn Commitment")
  matplot(simulations[, , "P"], type = "l", main = "Total Fund Nav")




 ########################

}
