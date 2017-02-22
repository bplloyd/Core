ggCashFlows = function(pef, freq = "m", scaledBy=NULL, return_object = F){
  df = cull_ggplot_data(pef@PeriodData[,c("Commitment", "Calls_Total_Net", "Calls_Total_Gross", "Distributions_Total", "CashFlows_Net", "FMV")]
                        #, melt_cols = c("Commitment", "Calls_Total_Net", "Calls_Total_Gross", "Distributions_Total", "FMV"),
                        ,cumulativeCols = c("Commitment", "Calls_Total_Net", "Calls_Total_Gross", "Distributions_Total", "CashFlows_Net")
                        , scaledBy = scaledBy)


  df[df$variable %in% c("Commitment", "Calls_Total_Gross", "Calls_Total_Net"), "value"] = -df[df$variable %in% c("Commitment", "Calls_Total_Gross", "Calls_Total_Net"),"value"]


  if(!return_object)
    ggplot2::ggplot(data = df, ggplot2::aes(x=date, y=value, color = variable)) + ggplot2::geom_line()
  else
    return(ggplot2::ggplot(data = df, ggplot2::aes(x=date, y=value, color = variable)) + ggplot2::geom_line())

}
