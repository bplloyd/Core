# getSpReturns = function(inds, startDate = NULL, endDate = NULL){
#   require(xts)
#   require(PerformanceAnalytics)
#   if(is.null(startDate))
#       startDate = start(inds)
#   if(is.null(endDate))
#       endDate = end(inds)
#   sp = inds[,"SPTR"]
#   sp = na.omit(sp)
#   sp=CalculateReturns(sp)
#   return(na.omit(sp[paste(startDate,endDate,sep = "/"),]))
# }

getSpIndex = function(){
  # require(xts)
  # require(PerformanceAnalytics)
  # require(quantmod)
  quantmod::getSymbols('^SP500TR', from = "1980-01-01")
  sp = quantmod::Ad(SP500TR)
  names(sp)[1] = "SPTR"
  return(sp)
}
