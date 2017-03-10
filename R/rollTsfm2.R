rollTsfm2 = function(assets, factors, fit.method = "LS", variable.selection = "none", window = 126, on = "days", fixedWindow = T){
  require(factorAnalytics)
  require(fPortfolio)
  require(xts)
  
  assets = assets[which(rowSums(is.na(assets)) < ncol(assets)),]
  merged = merge.xts(assets, na.fill(factors[index(assets),], fill = 0))
  slices = createTimeSlices2(merged, initialWindow = window, fixedWindow = fixedWindow, on = on)
  
  return(lapply(slices,
                        FUN = function(x)return(fitTsfm(asset.names = names(assets)[which(colSums(is.na(assets[x,]))==0)], 
                                                        factor.names = names(factors),
                                                        data = merged[x,],
                                                        fit.method = fit.method,
                                                        variable.selection = variable.selection
                                                        )
                                                )
                        )
        )
}