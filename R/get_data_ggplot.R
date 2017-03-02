get_data_ggplot = function(pef, cols, melt_cols = cols, cumulative_cols = NA_character_, scaledBy = NA_character_, underlying = NA_character_, date_filter=NA_character_) {
  if(is.na(underlying)) {
    if(is.na(date_filter)) {
      xts_data = pef@PeriodData[ , cols]
    } else {
      xts_data = pef@PeriodData[date_filter, cols]
    }

    if(!is.na(cumulative_cols)) {
      xts_data[, cumulative_cols] = cumsum(zoo::na.fill(xts_data[, cumulative_cols], 0))
    }

    if(!is.na(scaledBy)) {
      xts_data = xts_data/abs(sum(na.omit(pef@PeriodData[, scaledBy])))
    }


    df = data.frame(date = zoo::index(xts_data), xts_data)
    df_melt = data.table::melt(data = df,
                               #id.vars = names(df)[which(!(names(df) %in% melt_cols))],
                               measure.vars = melt_cols,
                               variable.name = "cashflow")
  } else if(tolower(substr(underlying,1,1)) == "v"){
    df_list = lapply(pef@UnderlyingVintages$Underlying,
                     function(v) get_data_ggplot(v, cols, melt_cols=cols, cumulative_cols, scaledBy, underlying = NA_character_,date_filter))
    names(df_list) = names(pef@UnderlyingVintages$Underlying)

    for(i in 1:length(df_list)) {
      v = names(df_list)[[i]]
      if(i == 1) {
        df_melt = cbind(df_list[[i]], vintage = rep(v, nrow(df_list[[i]])))
      } else {
        df_melt = rbind(df_melt, cbind(df_list[[i]], vintage = rep(v, nrow(df_list[[i]]))))
      }
    }
  }
  return(df_melt)
}
