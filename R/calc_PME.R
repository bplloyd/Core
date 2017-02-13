calc_PME = function(cf, fmv, bm, distIsPositive = T){
  data = cbind(cf, fmv, na.omit(bm))
  names(data) = c("cf", "fmv", "bm")
  data[, "fmv"] = zoo::na.locf(data[, "fmv"])
  data[, "bm"] = zoo::na.locf(data[, "bm"])
  data[, "cf"] = zoo::na.fill(data[, "cf"], 0)
  data = data[(zoo::index(data) >= start(cf)) & (zoo::index(data) <= end(cf)), ]

  if(distIsPositive){
    data[zoo::index(cf[1, ]), "cf"] =  data[zoo::index(cf[1, ]), "cf"] - zoo::na.fill(data[zoo::index(cf[1, ]), "fmv"], 0)
    pme = -data[, "bm"] * cumsum(data[, "cf"] / data[, "bm"])
  } else {
    data[zoo::index(cf[1, ]), "cf"] =  data[zoo::index(cf[1, ]), "cf"] + zoo::na.fill(data[zoo::index(cf[1, ]), "fmv"], 0)
    pme = data[, "bm"] * cumsum(data[, "cf"] / data[, "bm"])
  }
  names(pme)[1] = paste0(names(bm), "_PME")
  return(pme)
}

