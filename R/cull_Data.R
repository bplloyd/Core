cull_data = function(pef, freq)
{
  if(!is.null(pef@CashFlows)){
    data = cbind(pef@Commitments, pef@CashFlows, pef@FMV)

    freq = tolower(freq)

    if(freq != "d")
    {
      per = switch(freq, 'm' = "months", 'q' = "quarters", 'y' = "years")
      num_months = switch(freq, 'm' = 1, 'q' = 3, 'y' = 12)
      zoo::index(data) = lubridate::ceiling_date(zoo::index(data), per) - 1
      epts = xts::endpoints(data, per)

      if(length(epts)>2)
      {
        cf = xts::xts(apply(zoo::na.fill(data[,names(pef@CashFlows)],0), 2, function(c)xts::period.sum(c, epts)), order.by = zoo::index(data)[epts])
        commits = xts::period.sum(zoo::na.fill(data[,names(pef@Commitments)],0), epts)
        names(commits) = names(pef@Commitments)
        fmv = data[epts, names(pef@FMV)]
        data = cbind(commits, cf, fmv)


        fillDates = lubridate::ceiling_date(lubridate::add_with_rollback(e1 = start(data), months(0:(lubridate::interval(start(data), end(data))/months(num_months)))), unit = "m") - 1
        #fillDates = lubridate::ceiling_date(start(data) %m+% months(0:(lubridate::interval(start(data), end(data))/months(num_months))), per) - 1

        if(length(fillDates) != nrow(data))
        {
          temp_data = xts::xts(replicate(ncol(data), rep(NA_real_, length(fillDates))), order.by = fillDates)
          names(temp_data) = names(data)
          temp_data[zoo::index(data),] = data


          data = temp_data
        }
      }
    }
    data[, c(names(pef@Commitments), names(pef@CashFlows))] = zoo::na.fill(data[, c(names(pef@Commitments), names(pef@CashFlows))],0)
    data[, names(pef@FMV)] = zoo::na.locf(data[, names(pef@FMV)])

    data$Undrawn = cumsum(data$Commitment - data$Calls_Total_Gross)
    data$DrawdownRate = calc_drawdownRate(calls = data[, "Calls_Total_Gross"],
                                          commits = data[, "Commitment"])
    data$DistributionRate = calc_distributionRate(dists = data[, "Distributions_Total"],
                                                  fmv = data[, "FMV"])

    return(data)
  } else {
    return(NULL)
  }
}
