#' @include executeSP.R
#' @include abbToStrategy.R

get_privateHoldings = function(id=NULL, strategy=NULL, vintage=NULL, active = NULL)
{
  if((!is.null(strategy)) && (nchar(strategy) <= 3)){
    strategy = abbToStrategy(strategy)
  }

  if(!is.null(id))
    paramString = paste0("@holding_ID = ", id)
  else
  {
    paramString = NULL
    if(!is.null(strategy))
      paramString = paste0("@strategy = '", strategy, "'")
    if(!is.null(vintage))
      paramString = paste(paramString, paste0("@vintage = ", vintage), sep = ifelse(is.null(paramString), "", ", "))
    if(!is.null(active))
      paramString = paste(paramString, paste0("@active = ", active), sep = ifelse(is.null(paramString), "", ", "))
  }
  procString = "usp_get_PrivateHoldings"
  executeSP(procString, paramString, schema = "Core")
  # if(nrow(res) > 0){
  #   return(res)
  # } else {
  #   return(NULL)
  # }
}

