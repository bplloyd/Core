
#' @include calc_drawdownRate.R
#' @include calc_distributionRate.R
#' @include get_privateHoldings.R
#' @include get_privateCommitments.R
#' @include get_privateCashFlows.R
#' @include get_privateValuations.R
#' @include calc_privatePerformance.R
#' @include cull_Data.R
#' @include calc_PME.R

setOldClass(c("xts"))

#----------------------PrivateFund class definition---------------------------------------------------------
setClass("PrivateFund",
         representation = list(Name = "character",
                                Stats = "list",
                                CashFlows = "xts",
                                FMV = "xts",
                                Commitments = "xts",
                                PeriodData = "xts",
                                Holdings = "data.frame",
                                UnderlyingVintages = "list",
                                UnderlyingStrategies = "list",
                                UnderlyingFunds = "list",
                                InitialCommitment = "numeric",
                                TotalCommitment = "numeric",
                                Vintage =  "numeric",
                                Strategy = "character",
                                Active = "integer",
                                ID = "numeric",
                                Freq = "character",
                                Multiplier = "numeric"
                                ))



#----------------------PrivateFund init function---------------------------------------------------------
setMethod("initialize",
          "PrivateFund",
          function(.Object,
                   name = NA_character_,
                   id = NA_integer_,
                   strategy=NA_character_,
                   vintage=NA_integer_,
                   initialCommitment = NA_real_,
                   totalCommitment = NA_real_,
                   commitments = NULL,
                   cashFlows=NULL,
                   fmv = NULL,
                   periodData = NULL,
                   holdings = NULL,
                   underlyingVintages = list(),
                   underlyingStrategies = list(),
                   underlyingFunds = list(),
                   freq = "d",
                   dataFreq = "m",
                   multiplier = 1e+06,
                   active = NA_integer_,
                   publicBM = NULL,
                   loadVintages = F,
                   loadStrategies = F,
                   loadFunds = F
                   ){

            if(is.na(id))
            {
              .Object@Vintage = vintage
              .Object@Strategy = strategy
              .Object@ID = id
              .Object@Active = active

              id = NULL
              if(is.na(active)){
                active = NULL
              }

              if(is.na(vintage)){
                vintage = NULL
              }
              if(is.na(strategy)){
                strategy = NULL
              }
              if(is.na(name)){
                if(is.null(vintage) & is.null(strategy))
                  name = "Core"
                else if(!is.null(vintage) & is.null(strategy))
                  name = paste0("Core - ", vintage, " Vintage")
                else if(is.null(vintage) & !is.null(strategy))
                  name = paste0("Core - ", strategy)
                else
                  name = paste0("Core - ", vintage, " ", strategy)
              }
              if(!is.null(active)){
                if(active == 1)
                  name = paste0(name, " (Active Funds Only")
                else
                  name = paste0(name, " (Inactive Funds Only")
              }
            }
            else
            {
              .Object@ID = id

              vintage = NULL
              strategy = NULL
              active = NULL
              holdings = get_privateHoldings(id = id, strategy = strategy, vintage = vintage, active = active)


              .Object@Vintage = as.integer(holdings$Vintage)
              .Object@Strategy = as.character(holdings$Strategy)
              .Object@Active = as.integer(holdings$Active)
              .Object@InitialCommitment = as.numeric(holdings$Initial_Commitment_USD)/multiplier


              if(is.na(name))
                name = as.character(holdings$Holding_Name[1])


            }
            if(is.null(holdings))
            {
              holdings = get_privateHoldings(id = id, strategy = strategy, vintage = vintage, active = active)
            }

            if(is.null(commitments))
            {
              commitments = get_privateCommitments(id = id, strategy = strategy, vintage = vintage, active = active, freq = freq, multiplier = multiplier)
            }

            if(is.na(totalCommitment))
            {
              if(!is.null(commitments))
                .Object@TotalCommitment = sum(commitments, na.rm = T)
              else
                .Object@TotalCommitment = NA_real_
            }

            if(is.na(initialCommitment))
            {
              if(!is.null(holdings))
                .Object@InitialCommitment = sum(holdings$Initial_Commitment_USD, na.rm = T)/multiplier
              else
                .Object@InitialCommitment = NA_real_
            }

            if(is.null(cashFlows))
            {
              cashFlows = get_privateCashFlows(id = id, strategy = strategy, vintage = vintage, active = active, freq = freq, distIsPositive = T, multiplier = multiplier)
            }

            if(is.null(fmv))
            {
              fmv = get_privateValuations(id = id, strategy = strategy, vintage = vintage, active = active, freq = freq, multiplier = multiplier)
            }
            if(is.null(publicBM))
              publicBM = loadIndices("SPTR")

            pme = calc_PME(cashFlows[, "CashFlows_Net"], fmv = fmv, bm = publicBM)
            commit = sum(commitments)
            called_net = sum(cashFlows[, "Calls_Total_Net"])
            called_gross = sum(cashFlows[, "Calls_Total_Gross"])
            dists = sum(cashFlows[, "Distributions_Total"])
            fmv_end = fmv[nrow(fmv)]

            cf_stats = data.frame(Committed = commit,
                                  Called_w_Fees = called_net,
                                  Called_wo_Fees = called_gross,
                                  Distributed = dists,
                                  FMV = fmv_end)


            colnames(cf_stats)[2:3] = c("Called_w_Fees", "Called_wo_Fees")

            stats = list(Performance = try(calc_privatePerformance(cashFlows, fmv)),
                         PME_Performance = try(calc_privatePerformance(cashFlows, pme)),
                         CashFlows = try(cf_stats))

            .Object@Commitments = commitments
            .Object@Holdings = holdings
            .Object@CashFlows = cashFlows
            .Object@FMV = fmv
            .Object@Freq = freq
            .Object@Multiplier = multiplier
            .Object@Name = name
            .Object@UnderlyingFunds = underlyingFunds
            .Object@UnderlyingVintages = underlyingVintages
            .Object@UnderlyingStrategies = underlyingStrategies
            .Object@Stats = stats
            .Object@PeriodData = cull_Data(.Object, dataFreq)

            if(loadVintages)
              .Object@UnderlyingVintages = loadUnderlying(.Object, mode = "v")
            if(loadStrategies)
              .Object@UnderlyingStrategies = loadUnderlying(.Object, mode = "s")
            if(loadFunds)
              .Object@UnderlyingFunds = loadUnderlying(.Object, mode = "f")

            .Object

          })
#----------------------PrivateFund get_Data definition---------------------------------------------------------
# setGeneric(name = "get_Data", function(pef, freq) standardGeneric("get_Data"))
#
# setMethod(f = "get_Data",
#           c("PrivateFund", "character"),
#           function(pef, freq)
#           {
#             data = cbind(pef@Commitments, pef@CashFlows, pef@FMV)
#
#             freq = tolower(freq)
#
#             if(freq != "d")
#             {
#               per = switch(freq, 'm' = "months", 'q' = "quarters", 'y' = "years")
#               num_months = switch(freq, 'm' = 1, 'q' = 3, 'y' = 12)
#               zoo::index(data) = lubridate::ceiling_date(zoo::index(data), per) - 1
#               epts = xts::endpoints(data, per)
#
#               if(length(epts)>2)
#               {
#                 cf = xts::xts(apply(zoo::na.fill(data[,names(pef@CashFlows)],0), 2, function(c)xts::period.sum(c, epts)), order.by = zoo::index(data)[epts])
#                 commits = xts::period.sum(zoo::na.fill(data[,names(pef@Commitments)],0), epts)
#                 names(commits) = names(pef@Commitments)
#                 fmv = data[epts, names(pef@FMV)]
#                 data = cbind(commits, cf, fmv)
#
#                 fillDates = lubridate::ceiling_date(start(data) %m+% lubridate::months(0:(lubridate::interval(start(data), end(data))/lubridate::months(num_months))), per) - 1
#
#                 if(length(fillDates) != nrow(data))
#                 {
#                   temp_data = xts::xts(replicate(ncol(data), rep(NA_real_, length(fillDates))), order.by = fillDates)
#                   names(temp_data) = names(data)
#                   temp_data[zoo::index(data),] = data
#
#
#                   data = temp_data
#                 }
#               }
#             }
#             data[, c(names(pef@Commitments), names(pef@CashFlows))] = zoo::na.fill(data[, c(names(pef@Commitments), names(pef@CashFlows))],0)
#             data[, names(pef@FMV)] = xts::na.locf(data[, names(pef@FMV)])
#
#             data$Undrawn = cumsum(data$Commitment - data$Calls_Total_Gross)
#             data$DrawdownRate = calc_drawdownRate(calls = data[, "Calls_Total_Gross"],
#                                                   commits = data[, "Commitment"])
#             data$DistributionRate = calc_distributionRate(dists = data[, "Distributions_Total"],
#                                                           fmv = data[, "FMV"])
#
#             return(data)
#           }
# )

#----------------------PrivateFund constructor definition---------------------------------------------------------
PrivateFund = function(name = NA_character_,
                        id = NA_integer_,
                        strategy=NA_character_,
                        vintage=NA_integer_,
                        initialCommitment = NA_real_,
                        totalCommitment = NA_real_,
                        commitments = NULL,
                        cashFlows=NULL,
                        fmv = NULL,
                        periodData = NULL,
                        holdings = NULL,
                        underlyingFunds = list(),
                        underlyingVintages = list(),
                        underlyingStrategies = list(),
                        freq = "d",
                        dataFreq = "m",
                        multiplier = 1e+06,
                        active = NA_integer_,
                        loadVintages = F,
                        loadStrategies = F,
                        loadFunds = F)
{
  new("PrivateFund",
      name = name,
      id = id,
      strategy = strategy,
      vintage = vintage,
      initialCommitment = initialCommitment,
      totalCommitment = totalCommitment,
      commitments = commitments,
      cashFlows = cashFlows,
      fmv = fmv,
      periodData = periodData,
      holdings = holdings,
      underlyingFunds = underlyingFunds,
      underlyingStrategies = underlyingStrategies,
      underlyingVintages = underlyingVintages,
      freq = freq,
      dataFreq = dataFreq,
      multiplier = multiplier,
      active = as.integer(active),
      loadVintages = loadVintages,
      loadStrategies = loadStrategies,
      loadFunds = loadFunds
      )
}




#----------------------PrivateFund privateIRR definition---------------------------------------------------------
# setMethod(f = "privateIRR",
#           c("PrivateFund", "Period"),
#           function(pef, per)
#           {
#             cf_net = pef@CashFlows$CashFlows_Net
#             cf_gross = pef@CashFlows$CashFlows_Gross
#             fmv = pef@FMV
#
#             perf_start = max(end(cf_net), end(cf_gross), end(fmv)) %m-% per
#             cf_net = cf_net[which(zoo::index(cf_net) >= perf_start)]
#             cf_gross = cf_gross[which(zoo::index(cf_gross) >= perf_start)]
#             fmv = fmv[which(zoo::index(fmv)>=perf_start)]
#
#             cf_net = rbind(-fmv[1], cf_net, fmv[nrow(fmv)])
#             cf_gross = rbind(-fmv[1], cf_gross, fmv[nrow(fmv)])
#
#
#             return(UnderlyingFunds)
#           }
# )
#----------------------PrivateFund loadUnderlying definition---------------------------------------------------------
setGeneric(name = "loadUnderlying", function(pef, mode) standardGeneric("loadUnderlying"))
setMethod(f = "loadUnderlying",
        c("PrivateFund", "character"),

        function(pef, mode)
        {
          mode = tolower(strtrim(mode, 1))
          freq = tolower(pef@Freq)
          if(mode == "f"){
            ids = pef@Holdings$Holding_ID
            names(ids) = as.character(pef@Holdings$Holding_Name)
            underlying = lapply(ids, function(id)try(PrivateFund(id = id, freq = freq)))

          } else if (mode == "v"){
            vintages = sort(unique(pef@Holdings$Vintage))
            names(vintages) = vintages
            underlying = lapply(vintages, function(v)try(PrivateFund(vintage = v, freq = freq)))
          } else if (mode == "s"){
            vintages = sort(unique(pef@Holdings$Vintage))
            names(vintages) = vintages
            strategies = sort(unique(as.character.factor(pef@Holdings$Strategy)))
            names(strategies) = strategies
            underlying = lapply(vintages,
                                function(v) lapply(strategies,
                                                   function(s) try(PrivateFund(vintage = v, strategy = s, freq = freq))))
          }
          return(underlying)
        }
)
#----------------------PrivateFund compareHoldings definition---------------------------------------------------------
setGeneric(name = "compareHoldings", function(pef) standardGeneric("compareHoldings"))
setMethod(f = "compareHoldings",
          c("PrivateFund"),

          function(pef)
          {
            ids = pef@Holdings$Holding_ID
            if(length(ids)>1)
            {
              if(length(pef@UnderlyingFunds) == 0){
                names(ids) = as.character(pef@Holdings$Holding_Name)
                freq = tolower(pef@Freq)
                UnderlyingFunds = lapply(ids, function(id)new("PrivateFund", ID = id, Freq = freq))
              }
              else{
                UnderlyingFunds = pef@UnderlyingFunds
              }

              comparisons = vector(mode = "list", length = 2)
              names(comparisons) = c("Performance", "CashFlows")

              comparisons$Performance = vector(mode = "list", length = ncol(pef@Stats$Performance))
              names(comparisons$Performance) = colnames(pef@Stats$Performance)
              comparisons$CashFlows = vector(mode = "list", length = ncol(pef@Stats$CashFlows))
              names(comparisons$CashFlows) = colnames(pef@Stats$CashFlows)


              for(comp in names(comparisons$Performance))
              {
                comparisons$Performance[[comp]] = vector(mode = "list", length = 2)
                names(comparisons$Performance[[comp]]) = c("Total", "Underlying")
                comparisons$Performance[[comp]]$Total = pef@Stats$Performance["NetFees", comp]

                for(fund in UnderlyingFunds)
                {
                  df = data.frame(fund@Stats$Performance["NetFees", comp], row.names = fund@Name)
                  colnames(df) = comp
                  if(is.null(comparisons$Performance[[comp]]$Underlying))
                    comparisons$Performance[[comp]]$Underlying = df
                  else
                    comparisons$Performance[[comp]]$Underlying = rbind(comparisons$Performance[[comp]]$Underlying, df)

                }
                ord = order(comparisons$Performance[[comp]]$Underlying, decreasing = T)
                comparisons$Performance[[comp]]$Underlying = data.frame(comparisons$Performance[[comp]]$Underlying[ord, comp],
                                                                        row.names = row.names(comparisons$Performance[[comp]]$Underlying)[ord])
                colnames(comparisons$Performance[[comp]]$Underlying) = comp
              }

              for(comp in names(comparisons$CashFlows))
              {
                comparisons$CashFlows[[comp]] = vector(mode = "list", length = 2)
                names(comparisons$CashFlows[[comp]]) = c("Total", "Underlying")
                comparisons$CashFlows[[comp]]$Total = pef@Stats$CashFlows[, comp]

                for(fund in UnderlyingFunds)
                {
                  df = data.frame(fund@Stats$CashFlows[ , comp], row.names = fund@Name)
                  colnames(df) = comp
                  if(is.null(comparisons$CashFlows[[comp]]$Underlying))
                    comparisons$CashFlows[[comp]]$Underlying = df
                  else
                    comparisons$CashFlows[[comp]]$Underlying = rbind(comparisons$CashFlows[[comp]]$Underlying, df)

                }
                ord = order(comparisons$CashFlows[[comp]]$Underlying, decreasing = T)
                comparisons$CashFlows[[comp]]$Underlying = data.frame(comparisons$CashFlows[[comp]]$Underlying[ord, comp],
                                                                        row.names = row.names(comparisons$CashFlows[[comp]]$Underlying)[ord])
                colnames(comparisons$CashFlows[[comp]]$Underlying) = comp
              }


              #
              # if(tolower(substr(comparison, 1, 1)) == "i")
              # {
              #   comp = "IRR"
              #   df = data.frame(IRR = pef@Performance$`2016-12-31`["NetFees", comp], row.names = c("Total"))
              #
              # }
              # else
              # {
              #   comp = "RollingMultiple"
              #   df = data.frame(IRR = pef@Performance[[1]]["NetFees", comp], row.names = c("Total"))
              # }
              # for(fund in UnderlyingFunds){
              #   newdf = data.frame(fund@Performance[[1]]["NetFees", comp], row.names = c(fund@Name))
              #   colnames(newdf) = comp
              #   rownames(newdf) = fund@Name
              #   df = rbind(df, newdf)
              # }


              return(comparisons)
            }
            # return(df)

          }
)

#----------------------PrivateFund print definition---------------------------------------------------------
setMethod("print",
          signature(c(x="PrivateFund")),
          function(x)
            {
              pef = x
              #perf = pef@Stats$Performance
              # commit = sum(pef@Commitments)
              # called_net = sum(pef@CashFlows[, "Calls_Total_Net"])
              # called_gross = sum(pef@CashFlows[, "Calls_Total_Gross"])
              # dists = sum(pef@CashFlows[, "Distributions_Total"])

              # cf_stats = data.frame(Committed = commit,
              #                       Called_Net = called_net,
              #                       Called_Gross = called_gross,
              #                       Distributed = dists)



              cat('\nCash Flow Stats-------------------------------------------\n')
              #print(cf_stats)
              print(pef@Stats$CashFlows)
              cat('\n\n')
              cat('Performance Stats-------------------------------------------\n')
              #print(perf)
              print(pef@Stats$Performance)
            })




#----------------------PrivateFund modelDistributions definition---------------------------------------------------------
# setMethod(f = "modelDistributions",
#           c("PrivateFund"),
#
#           function(pef)
#           {
#             require(yuima)
#             dist_rate = data[, "DistributionRate"]
#             fmv = data[zoo::index(dist_rate), "FMV"]
#             dist_start = zoo::index(fmv[fmv!=0])[2]
#             dist_rate = zoo::na.fill(dist_rate[zoo::index(dist_rate) >= dist_start], 0)
#
#             param.init_shift = list(a = 1.1, b = -2, sigma = 1)
#
#             low.par = list(a = 0, b = -1e+09, sigma = 0)
#             upp.par = list(a = 1e+09, b = 0, sigma = 100)
#             prior <-
#               list(
#                 a=list(measure.type="code",df="dnorm(z,0,1)"),
#                 b=list(measure.type="code",df="dnorm(z,0,1)"),
#                 sigma=list(measure.type="code",df="dnorm(z,0,1)")
#               )
#
#             mod = setModel(drift = "(a + b*x)", diffusion = "sigma*sqrt(x)", solve.variable = "x")
#           }
# )
