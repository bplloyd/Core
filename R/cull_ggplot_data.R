cull_ggplot_data = function(xts_data, cols = names(xts_data), cumulativeCols = NULL, ...){
  # if(!is.null(na.action))
  #   xts_data = na.action(xts_data)

  if(!is.null(cumulativeCols))
    xts_data[,cumulativeCols] = cumsum(na.fill(xts_data[, cumulativeCols], 0))


  df = data.frame(date = zoo::index(xts_data), xts_data[, cols])
  df_melt = data.table::melt(data = df,
                             id.vars = c("date"),
                             measure.vars = cols, ...)
  return(df_melt)
}
