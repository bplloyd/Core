extractRolledRiskContribution = function(Fm.VaR.roll, name, measure){
  require(xts)
  result = matrix(NA, nrow = length(Fm.VaR.roll), ncol = ncol(Fm.VaR.roll[[1]][paste(measure)][[1]])) 
  colnames(result) = colnames(Fm.VaR.roll[[1]][paste(measure)][[1]])
  for (i in 1:length(Fm.VaR.roll)){
    
    if(name %in% row.names(Fm.VaR.roll[[i]][paste(measure)][[1]])){
      result[i,] = Fm.VaR.roll[[i]][paste(measure)][[1]][c(name),]
    }
  }
  return(xts(result, order.by = as.Date.character(names(Fm.VaR.roll))))
}