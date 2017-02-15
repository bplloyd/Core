#' @include executeSP.R
#' @include abbToStrategy.R
#' @include make_paramString.R

get_privateCashFlows = function(id=NA, strategy=NA, vintage=NA, active = NA, freq = 'm', distIsPositive = T, multiplier = 1){
  paramString = make_paramString(id, strategy, vintage, active)
  procString = "usp_get_CashFlows"
  cf = executeSP(procString, paramString, schema = "Core")
  cf$Effective_Date = as.Date.factor(cf$Effective_Date)
  names(cf)[names(cf) == "Effective_Date"] = "Date"

  cf = dplyr::tbl_df(cf)

  if(nrow(cf)>0){

    cf =  dplyr::summarise(dplyr::group_by(cf, Date, Transaction_Description), Amount = sum(Amount))
    cf = data.table::dcast(cf, Date ~  Transaction_Description, fun.aggregate = sum, value.var = "Amount")
    cf_names = names(cf)[2:ncol(cf)]
    cf = tidyquant::as_xts_(cf, date_col = "Date")
    names(cf) = cf_names
    callCols = grep("CapitalCall", names(cf))
    distCols = grep("Distribution", names(cf))
    feeCol = grep("FeesAndExpenses", names(cf))

    if(length(callCols) > 0)
      calls = xts::xts(apply(cf[, callCols], 1, sum), order.by = zoo::index(cf))
    else
      calls = xts::xts(rep(0, nrow(cf)), order.by = zoo::index(cf))

    if(length(distCols) > 0)
      dists = xts::xts(apply(cf[, distCols], 1, sum), order.by = zoo::index(cf))
    else
      dists = xts::xts(rep(0, nrow(cf)), order.by = zoo::index(cf))

    if(length(feeCol) > 0)
      fees = cf[, feeCol]
    else
      fees = xts::xts(rep(0, nrow(cf)), order.by = zoo::index(cf))


    cf$Calls_Total_Net = calls
    cf$Calls_Total_Gross = calls - fees
    cf$Distributions_Total = dists
    cf$CashFlows_Net = calls - dists
    cf$CashFlows_Gross = calls - dists - fees

    if(distIsPositive)
      cf[, c("CashFlows_Net", "CashFlows_Gross")] = -1*cf[, c("CashFlows_Net", "CashFlows_Gross")]

    if(freq != "d"){
      zoo::index(cf) = lubridate::ceiling_date(zoo::index(cf), "months") - 1
      epts = xts::endpoints(cf, "months")
      num_months = switch(freq, 'm' = 1, 'q' = 3, 'y' = 12)
      if(length(epts)>2){
        cf = xts::xts(apply(cf, 2, function(c)xts::period.sum(c, epts)), order.by = zoo::index(cf)[epts])
        #fillDates = lubridate::ceiling_date(start(cf) %m+% months(0:(lubridate::interval(start(cf), end(cf))/months(1))), "months") - 1

        fillDates = lubridate::ceiling_date(lubridate::add_with_rollback(e1 = start(cf), months(0:(lubridate::interval(start(cf), end(cf))/months(1)))), unit = "m")-1
        temp_cf = xts::xts(replicate(ncol(cf), rep(0, length(fillDates))), order.by = fillDates)
        names(temp_cf) = names(cf)
        temp_cf[zoo::index(cf),] = cf
      }
      else{
        temp_cf = cf
      }

      if(freq != "m"){
        per = switch(freq,
                     "q" = "quarters",
                     "y" = "years"
        )
        epts = xts::endpoints(temp_cf, per)
        if(length(epts)>2)
          cf = xts::xts(apply(temp_cf, 2, function(c) xts::period.sum(c, epts)), order.by = zoo::index(temp_cf)[epts])

        zoo::index(cf) = lubridate::ceiling_date(zoo::index(cf), per) - 1
      }
      else{
        cf = temp_cf
      }
    }
    return(cf/multiplier)
  } else {
    return(NULL)
  }

}
