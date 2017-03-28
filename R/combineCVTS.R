combineCVTS = function(cvts_list, weights = rep(1/length(cvts_list), length(cvts_list))){
  classes = sapply(cvts_list, function(cv)class(cv$models[[1]])[1])
  net_pos = grep("nnetar", classes)
  # if(length(net_pos) == 0){
  forecasts = lapply(names(cvts_list[[1]]$forecasts), function(d)list(mean = sapply(cvts_list, function(cv) cv$forecasts[[d]]$mean),
                                                                      upper = lapply(cvts_list, function(cv) cv$forecasts[[d]]$upper),
                                                                      lower = lapply(cvts_list, function(cv)cv$forecasts[[d]]$lower)))


  # } else {
  #   forecasts = lapply(names(cvts_list[[1]]$forecasts), function(d)list(mean = sapply(cvts_list, function(cv) cv$forecasts[[d]]$mean),
  #                                                                            upper = sapply(cvts_list[-net_pos], function(cv) cv$forecasts[[d]]$upper),
  #                                                                            lower = sapply(cvts_list[-net_pos], function(cv)cv$forecasts[[d]]$lower)))
  # }
  names(forecasts) = names(cvts_list[[1]]$forecasts)


  for(n in names(forecasts)){
    lower = vector(mode = "list", length = ncol(forecasts[[n]]$lower[[1]]))
    names(lower) = colnames(forecasts[[n]]$lower[[1]])
    upper = vector(mode = "list", length = ncol(forecasts[[n]]$upper[[1]]))
    names(upper) = colnames(forecasts[[n]]$upper[[1]])
    for(q in names(lower)){
      if(length(net_pos)==0){
        lower[[q]] =  matrix(data = NA, nrow = nrow(forecasts[[n]]$lower[[1]]), ncol = length(forecasts[[n]]$lower))
        colnames(lower[[q]]) = names(forecasts[[n]]$lower)
        upper[[q]] =  matrix(data = NA, nrow = nrow(forecasts[[n]]$upper[[1]]), ncol = length(forecasts[[n]]$upper))
        colnames(upper[[q]]) = names(forecasts[[n]]$upper)
        for(j in 1:length(forecasts[[n]]$lower)){
          if(!is.null(forecasts[[n]]$lower[[j]])){
            lower[[q]][, j] = forecasts[[n]]$lower[[j]][, q]
            upper[[q]][, j] = forecasts[[n]]$upper[[j]][, q]
          }
        }
      } else {
        lower[[q]] =  matrix(data = NA, nrow = nrow(forecasts[[n]]$lower[[1]]), ncol = length(forecasts[[n]]$lower[-net_pos]))
        colnames(lower[[q]]) = names(forecasts[[n]]$lower[-net_pos])
        upper[[q]] =  matrix(data = NA, nrow = nrow(forecasts[[n]]$upper[[1]]), ncol = length(forecasts[[n]]$upper[-net_pos]))
        colnames(upper[[q]]) = names(forecasts[[n]]$upper[-net_pos])
        for(j in 1:length(forecasts[[n]]$lower[-net_pos])){
          if(!is.null(forecasts[[n]]$lower[-net_pos][[j]])){
            lower[[q]][, j] = forecasts[[n]]$lower[-net_pos][[j]][, q]
            upper[[q]][, j] = forecasts[[n]]$upper[-net_pos][[j]][, q]
          }
        }
      }
    }
    forecasts[[n]]$upper = upper
    forecasts[[n]]$lower = lower
  }

    # forecasts = lapply(forecasts, function(f) list(mean = cbind(f$mean, f$mean %*% weights),
    #                                    upper = cbind(f$upper, f$upper %*% weights),
    #                                    lower = cbind(f$lower, f$lower %*% weights)))

  forecasts = lapply(names(cvts_list[[1]]$forecasts), function(f)
  {
    upper = matrix(data = NA, nrow = nrow(forecasts[[f]]$upper[[1]]), ncol = length(forecasts[[f]]$upper))
    colnames(upper) = names(forecasts[[f]]$upper)

    lower = matrix(data = NA, nrow = nrow(forecasts[[f]]$lower[[1]]), ncol = length(forecasts[[f]]$lower))
    colnames(lower) = names(forecasts[[f]]$lower)

    for(q in colnames(upper)){
      if(length(net_pos)>0){
        upper[, q] = forecasts[[f]]$upper[[q]] %*% (weights[-net_pos]/sum(weights[-net_pos]))
        lower[, q] = forecasts[[f]]$lower[[q]] %*% (weights[-net_pos]/sum(weights[-net_pos]))
      } else {
        upper[, q] = forecasts[[f]]$upper[[q]] %*% weights
        lower[, q] = forecasts[[f]]$lower[[q]] %*% weights
      }
    }
    upper = ts(data = upper,
               start = tsp(cvts_list[[1]]$forecasts[[f]]$mean)[1],
               end = tsp(cvts_list[[1]]$forecasts[[f]]$mean)[2],
               frequency = tsp(cvts_list[[1]]$forecasts[[f]]$mean)[3])
    lower = ts(data = lower,
               start = tsp(cvts_list[[1]]$forecasts[[f]]$mean)[1],
               end = tsp(cvts_list[[1]]$forecasts[[f]]$mean)[2],
               frequency = tsp(cvts_list[[1]]$forecasts[[f]]$mean)[3])
    list(mean = ts(data = forecasts[[f]]$mean %*% weights,
                   start = tsp(cvts_list[[1]]$forecasts[[f]]$mean)[1],
                   end = tsp(cvts_list[[1]]$forecasts[[f]]$mean)[2],
                   frequency = tsp(cvts_list[[1]]$forecasts[[f]]$mean)[3]),
         upper = upper,
         lower = lower)


  })

  names(forecasts) = names(cvts_list[[1]]$forecasts)

  for(f in names(forecasts)){
    #colnames(forecasts[[f]]$mean)[ncol(forecasts[[f]]$mean)] = colnames(forecasts[[f]]$upper)[ncol(forecasts[[f]]$upper)] = colnames(forecasts[[f]]$lower)[ncol(forecasts[[f]]$lower)] = "hybrid"
    forecasts[[f]]$x = cvts_list[[1]]$forecasts[[f]]$x
    forecasts[[f]]$level = cvts_list[[1]]$forecasts[[f]]$level
    forecasts[[f]]$series = cvts_list[[1]]$forecasts[[f]]$series
    class(forecasts[[f]]) = "forecast"
  }
  residuals = cvts_list[[1]]$residuals * weights[1]
  for(i in 2:length(cvts_list)){
    residuals = residuals + cvts_list[[i]]$residuals * weights[i]
  }
  return(list(forecasts = forecasts, residuals = residuals))
}
