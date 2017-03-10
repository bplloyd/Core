me = function(err, lastN=NULL, na.rm=T) {
  if(!is.null(lastN)) {
    if(na.rm) {
      err = na.omit(err)
    }
    err = err[(length(err) - lastN + 1):length(err)]
  }
  return(mean(err, na.rm = T))
}
