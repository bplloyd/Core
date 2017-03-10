extractSingleBetaroll = function(Fm.roll, name){
  require(xts)
  result = matrix(NA, nrow = length(Fm.roll), ncol = ncol(coef(Fm.roll[[1]])))
  colnames(result) = colnames(coef(Fm.roll[[1]]))
  for (i in 1:length(Fm.roll)){
    if(name %in% row.names(coef(Fm.roll[[i]]))){
      result[i,] = t(as.vector(coef(Fm.roll[[i]])[c(name),])[1,])
    }
  }
  row.names(result) = names(Fm.roll)
  return(result)
}
name = "CITADEL"
