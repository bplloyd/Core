cull_ggplot_data = function(xts_data, melt_cols = names(xts_data), cumulativeCols = NULL, scaledBy = NULL, ...){
  # if(!is.null(na.action))
  #   xts_data = na.action(xts_data)
  if(!lubridate::is.Date(index(xts_data)))
    index(xts_data) = zoo::as.Date(index(xts_data))

  freq = PerformanceAnalytics::Frequency(xts_data)[1]
  if(freq == 12){
    per = "months"
  } else
  if (freq == 4) {
    per = "quarters"
  } else {
    per = "years"
  }

  index(xts_data) = lubridate::ceiling_date(index(xts_data), unit = per) - 1

  if(!is.null(scaledBy)){
    denom = abs(sum(na.omit(xts_data[, scaledBy])))
    xts_data = xts_data/denom
  }

  if(!is.null(cumulativeCols))
    xts_data[,cumulativeCols] = cumsum(na.fill(xts_data[, cumulativeCols], 0))


  df = data.frame(date = zoo::index(xts_data), xts_data)
  df_melt = data.table::melt(data = df,
                             #id.vars = names(df)[which(!(names(df) %in% melt_cols))],
                             measure.vars = melt_cols)
  return(df_melt)
}
