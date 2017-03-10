# RETURNS TIME SLICES FOR ROLLING LISTS
createTimeSlices2 = function(data, initialWindow = 126, fixedWindow = TRUE, forward = F, on = "days", skip=0){
  require(xts)
  y = 1:nrow(data)
  stops <- (seq(along = y))[initialWindow:length(y)]
  if (fixedWindow){
    starts <- stops - initialWindow + 1
  }
  else{
    starts <- rep(1, length(stops))
  }
  slices = lapply(mapply(seq, starts, stops, SIMPLIFY = FALSE), function(x)return(index(data)[x]))
  if(!forward)
    names(slices) = index(data[initialWindow:nrow(data),])
  if(forward)
    names(slices) = index(data[1:(nrow(data)-initialWindow+1)])
  thin <- function(x, skip = 2){
    n <- length(x)
    x[seq(1, n, by = skip)]
  }

  epoints = as.character.Date(index(data)[endpoints(data, on = on)])
  slices = slices[which(names(slices) %in% epoints)]
  if (skip > 0){
    slices <- thin(slices, skip = skip + 1)
  }
  return(slices)
}