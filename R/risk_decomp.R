risk_decomp = function (tickers, weights = rep(1, length(tickers)), data)
{
  MCTR = PortRisk::mctr(tickers, weights, start(data), end(data), data)
  #Weight = weights/sum(weights)
  CCTR = weights * MCTR
  sigma = sum(CCTR)
  CCTR_percent = 100 * CCTR/sigma
  Volatility = PortRisk::volatility(tickers, start(data), end(data), data)
  output = cbind(weights, MCTR, CCTR, CCTR_percent, Volatility)
  Portfolio = c(1, NA, sigma, 100, sigma)
  output = data.frame(rbind(output, Portfolio))

  colnames(output) = c("Weight", "MCTR", "CCTR", "CCTR(%)",
                         "Volatility")
  rownames(output)[nrow(output)] = "Portfolio"
  return(output)
}
