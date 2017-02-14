#' @include executeSP.R
#' @include abbToStrategy.R

get_privateCommitments = function(id=NULL, strategy=NULL, vintage=NULL, active = NULL, freq = 'd', multiplier = 1)
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
  procString = "usp_get_Commitments"
  commits = executeSP(procString, paramString, schema = "Core")
  commits$Effective_Date = as.Date.factor(commits$Effective_Date)
  names(commits)[names(commits) == "Effective_Date"] = "Date"
  #commits[, 2:ncol(commits)] = -1*commits[, 2:ncol(commits)]

  if(nrow(commits)>0)
  {
    commits = tidyquant::as_xts_(commits, "Date")
    names(commits) = "Commitment"
    if(freq != "d"){
    zoo::index(commits) = lubridate::ceiling_date(zoo::index(commits), "months") - 1
      commits = xts::apply.monthly(commits, sum)
      fillDates = start(commits) %m+% months(0:(lubridate::interval(start(commits), end(commits))/months(1)))
      temp_commits =  xts::xts(rep(0, length(fillDates)), order.by = fillDates)
      temp_commits[zoo::index(commits)] = commits

      if(freq == 'm'){
        commits = temp_commits
      }
      if(freq == 'q'){
        commits = xts::apply.quarterly(temp_commits, sum)
        zoo::index(commits) = lubridate::ceiling_date(zoo::index(commits), "quarters") - 1
      }
      if(freq=='y'){
        commits = xts::apply.yearly(temp_commits, sum)
        zoo::index(commits) = lubridate::ceiling_date(zoo::index(commits), "years") - 1
      }
    }
    return(commits/multiplier)
  } else {
    return(NULL)
  }

}

