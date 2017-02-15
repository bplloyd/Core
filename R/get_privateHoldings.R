#' @include executeSP.R
#' @include abbToStrategy.R
#' @include make_paramString.R

get_privateHoldings = function(id=NA, strategy=NA, vintage=NA, active = NA)
{
  paramString = make_paramString(id, strategy, vintage, active)
  procString = "usp_get_PrivateHoldings"
  executeSP(procString, paramString, schema = "Core")
}

