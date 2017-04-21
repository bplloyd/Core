loadFactorData = function(){
  factor_data = readRDS("G:/PORTFOLIO MANGEMENT/Bryan Lloyd/2016 Projects/Data/factor_data.rds")
  #fama = getFamaFactors()

  for(i in 1:length(factor_data$data)){
    ticker.i = names(factor_data[[1]])[i]
    name.i = factor_data$names[i,]
    df = factor_data$data[[i]]

    if(nrow(df)>0){
      xts.i = xts(df[,2], order.by = df[,1])
      xts.i = xts.i[endpoints(xts.i),]
      index(xts.i) = as.yearmon(index(xts.i))
      names(xts.i) = name.i
      xts.i = na.omit(CalculateReturns(xts.i))



      if((i==1)){
        factors_xts = xts.i
        tickers = ticker.i
      } else {
        factors_xts = cbind(factors_xts, xts.i)
        tickers = c(tickers, ticker.i)
      }
    }

  }

  keepCols = colSums(is.na(factors_xts['201612/',])) != nrow(factors_xts['201612/',])
  factors_xts = factors_xts[, keepCols]
  tickers = tickers[keepCols]
  lookup = cbind(tickers, names=names(factors_xts))


  #SMALL MINUS BIG US
  factors_xts = cbind(factors_xts,
                      factors_xts[,tickers=="RU20INTR Index"] - factors_xts[,tickers=="RU10INTR Index"])
  names(factors_xts)[ncol(factors_xts)] = "SMB_US"

  #SMALL MINUS BIG WORLD
  factors_xts = cbind(factors_xts,
                      factors_xts[,tickers=="M2WOSC Index"] - factors_xts[,tickers=="M2WOLC Index"])
  names(factors_xts)[ncol(factors_xts)] = "SMB_WORLD"


  #VALUE MINUS GROWTH US
  factors_xts = cbind(factors_xts,
                      factors_xts[,tickers=="RU30VATR Index"] - factors_xts[,tickers=="RU30GRTR Index"])
  names(factors_xts)[ncol(factors_xts)] = "VMG_US"

  #VALUE MINUS GROWTH WORLD
  factors_xts = cbind(factors_xts,
                      factors_xts[,tickers=="M2WO000V Index"] - factors_xts[,tickers=="M2WO000G Index"])
  names(factors_xts)[ncol(factors_xts)] = "VMG_WORLD"

  #HY MINUS TRES US
  factors_xts = cbind(factors_xts,
                      factors_xts[,tickers=="H0A0 Index"] - factors_xts[,tickers=="G0Q0 Index"])
  names(factors_xts)[ncol(factors_xts)] = "HY_M_TRES_US"

  #HY MINUS TRES GLOBAL
  factors_xts = cbind(factors_xts,
                      factors_xts[,tickers=="HW00 Index"] - factors_xts[,tickers=="W0G1 Index"])
  names(factors_xts)[ncol(factors_xts)] = "HY_M_TRES_WORLD"

  factors_xts[, c("MSCI.WORLD.GR",
                            "SMB_US",
                            "SMB_WORLD",
                            "VMG_US",
                            "VMG_WORLD",
                            "HY_M_TRES_US",
                            "HY_M_TRES_WORLD",
                            "US.Treasury",
                            "Global.Government",
                            "MSCI.AC.ASIA.PACIFIC.GR",
                            "MSCI.India.USD",
                            "Nifty.50",
                            "TOPIX.100.TR",
                            "Deutsche.Bank.Equity.Risk.Adju",
                            "Deutsche.Bank.Equity.Momentum",
                            "Deutsche.Bank.Long.Short.Momen.1",
                            "Credit.Suisse.Cross.Sectional",
                            "Credit.Suisse.Leveraged.Loan.T",
                            "S.P.SUPERCOM.O.G.EXP.IDX",
                            "S.P.SUPERCOM.OIL.GAS.IDX",
                            "BarclayHedge.US.Managed.Future",
                            "BarclayHedge.CTA.Index",
                            "MSCI.WORLD.IT.GR",
                            "MSCI.WORLD.HC.GR",
                            "MSCI.WORLD.MAT.GR",
                            "MSCI.WORLD.FIN.GR",
                            "MSCI.WORLD.IND.GR",
                            "MSCI.WORLD.ENGY.GR",
                            "MSCI.WORLD.CD.GR",
                            "MSCI.WORLD.CS.GR",
                            "MSCI.WORLD.UTIL.GR",
                            "MSCI.WORLD.TC.S.GR",
                            "MSCI.World.Metals.Min.Gr")]
}
