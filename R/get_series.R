get_series = function(pef, nm = "DistributionRate", clean = T, na.rm = T){
  res = subset(pef@PeriodData, rep(T, nrow(pef@PeriodData)), select = c(nm))
  if(na.rm){
    res = na.omit(res)
  }
  if(clean){
    res = PerformanceAnalytics::clean.boudt(res)[[1]]
  }
  return(as.data.frame(res))
}
