get_rate = function(pef, rate = "DistributionRate", clean = T){
  df_rate = na.omit(subset(pef@PeriodData, rep(T, nrow(pef@PeriodData)), select = c(rate)))
  if(clean){
    #rn = row.names(df_rate)
    df_rate = as.data.frame(PerformanceAnalytics::clean.boudt(df_rate)[[1]])
  }
  return(df_rate)
}
