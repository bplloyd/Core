#' @include executeSP.R
#' @include abbToStrategy.R


get_privateValuations = function(id=NULL, strategy=NULL, vintage=NULL, active = NULL, freq = 'm', multiplier = 1)
{
  #require(lubridate)

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
  procString = "usp_get_Valuations"
  val = executeSP(procString, paramString, schema = "Core")
  val$Month = as.Date.factor(val$Month)
  val$Month = lubridate::rollback(lubridate::add_with_rollback(val$Month, months(1)))

  if(nrow(val)>0)
  {
    val = tidyquant::as_xts_(val, "Month")
    names(val) = c("FMV")

    if(freq == "q")
      val = val[xts::endpoints(val, on = "quarters")]
    else if(freq == "y")
      val = val[xts::endpoints(val, on = "years")]
    # else
    #   val = val[zoo::index(val)]

    zoo::index(val) = lubridate::ceiling_date(zoo::index(val), "months")-1

    return(val[zoo::index(val) >= start(val[val != 0])]/multiplier)
  }
  else
    return(NULL)


}

