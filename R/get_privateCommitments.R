#' @include executeSP.R
#' @include abbToStrategy.R
#' @include make_paramString.R


get_privateCommitments = function(id=NA, strategy=NA, vintage=NA, active = NA, freq = 'd', multiplier = 1)
{
  paramString = make_paramString(id, strategy, vintage, active)
  procString = "usp_get_Commitments"
  commits = executeSP(procString, paramString, schema = "Core")
  commits$Effective_Date = as.Date.factor(commits$Effective_Date)
  names(commits)[names(commits) == "Effective_Date"] = "Date"

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

