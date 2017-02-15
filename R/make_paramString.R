make_paramString = function(id=NA, strategy = NA, vintage = NA, active = NA){
  if((!is.na(strategy)) && (nchar(strategy) <= 3)){
    strategy = abbToStrategy(strategy)
  }

  if(!is.na(id)){
    paramString = paste0("@holding_ID = ", id)
  } else {
    paramString = NULL
    if(!is.na(strategy))
      paramString = paste0("@strategy = '", strategy, "'")
    if(!is.na(vintage))
      paramString = paste(paramString, paste0("@vintage = ", vintage), sep = ifelse(is.null(paramString), "", ", "))
    if(!is.na(active))
      paramString = paste(paramString, paste0("@active = ", active), sep = ifelse(is.null(paramString), "", ", "))
  }
  return(paramString)
}
