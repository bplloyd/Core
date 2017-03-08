get_rate = function(pef, rate = "DistributionRate", clean = T){
  subset(pef@PeriodData, rep(T, nrow(pef@PeriodData)), select = c(rate))
}
