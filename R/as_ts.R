as_ts = function(data, start=NULL, end=NULL, freq=NULL){
  if(is.null(freq)){
    freq = switch(xts::periodicity(data)$scale,
                  'monthly' = 12,
                  'quarterly' = 4,
                  'yearly' = 1)
  }
  if(is.null(start)){
    start = c(lubridate::year(zoo::index(data)[1]), lubridate::month(zoo::index(data)[1]))
  }
  if(is.null(end)){
    end = c(lubridate::year(zoo::index(data)[nrow(data)]), lubridate::month(zoo::index(data)[nrow(data)]))
  }
  return(ts(data = as.vector(data),
            start = start,
            end = end,
            frequency = freq))
}

