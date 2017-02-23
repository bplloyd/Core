plot_tsCV_comparison = function(result_list, errFunc, funcArgs = NULL, label="MAE", main = "Forecast Validation Comparison", legend_spot = "bottomright", ylim = NULL, ...){

  err_stats = lapply(result_list,
                     function(r) apply(r[,,"error"],
                                       MARGIN = 2,
                                       FUN = function(c)do.call(what = errFunc,
                                                                args = append(list(err=c),funcArgs))
                                       )
                     )

  h=length(err_stats[[1]])
  if(is.null(ylim)) {
    ymax = max(unlist(err_stats))
    ymin = min(unlist(err_stats))
    ylim = c(ymin, ymax)
  }
  for(i in 1:length(err_stats)) {
    if(i == 1) {
      plot(1:h, err_stats[[i]],  type="l", col=i, xlab="horizon", ylab=label, lwd = 2, main = main, ylim = ylim, ...)
    } else {
      lines(1:h, err_stats[[i]], col = i, lwd = 2)
    }
  }
  legend(legend_spot,legend=names(err_stats),col=1:length(err_stats),lwd = 2)
}

plot_tsCV_rolling_error = function(result_array, err_func = function(r)mean(abs(r), na.rm = T), label = "MAE", main = "MAE Over Time", ...) {
  if(is.array(result_array)) {
    inc_rows = which(apply(result_array[,,"error"], 1, function(r)return(all(!is.na(r)))))
    #plot(inc_rows, apply(result_array[inc_rows,,"error"], 1, function(r) err_func(r)), type = "l", main = main)
    result = rep(NA, dim(result_array)[1])
    result[inc_rows] = apply(result_array[,,"error"], 1, function(r) err_func(r))
    plot(1:dim(result_array)[1], result, type = "l", main = main, ...)
  } else if(is.list(result_array)) {
    for(i in 1:length(result_array)) {
      inc_rows = which(apply(result_array[[i]][,,"error"], 1, function(r)return(all(!is.na(r)))))
      #plot(inc_rows, apply(result_array[inc_rows,,"error"], 1, function(r) err_func(r)), type = "l", main = main)
      result = rep(NA, dim(result_array[[i]])[1])
      result[inc_rows] = apply(result_array[[i]][,,"error"], 1, function(r) err_func(r))
      if(i == 1){
        plot(1:length(result), result, type = "l", main = main, col = i, ...)
      } else {
        lines(1:length(result), result, col = i)
      }
    }
  }
}
