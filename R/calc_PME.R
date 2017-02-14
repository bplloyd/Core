calc_PME = function(cf, fmv, bm, distIsPositive = T){
  if(is.null(fmv)){
    fmv = xts::xts(rep(0, nrow(cf)), order.by = zoo::index(cf))
  }

  if(!is.null(cf)){
    data = cbind(cf, fmv, na.omit(bm))
    names(data) = c("cf", "fmv", "bm")
    data[, "fmv"] = zoo::na.locf(data[, "fmv"])
    data[, "bm"] = zoo::na.locf(data[, "bm"])
    data[, "cf"] = zoo::na.fill(data[, "cf"], 0)
    data = data[(zoo::index(data) >= min(start(cf), start(na.omit(fmv)))) & (zoo::index(data) <= max(end(cf), end(fmv))), ]

    if(distIsPositive){
      data[zoo::index(cf[1, ]), "cf"] =  data[zoo::index(cf[1, ]), "cf"] - zoo::na.fill(data[zoo::index(cf[1, ]), "fmv"], 0)
      pme = -data[, "bm"] * cumsum(data[, "cf"] / data[, "bm"])
    } else {
      data[zoo::index(cf[1, ]), "cf"] =  data[zoo::index(cf[1, ]), "cf"] + zoo::na.fill(data[zoo::index(cf[1, ]), "fmv"], 0)
      pme = data[, "bm"] * cumsum(data[, "cf"] / data[, "bm"])
    }
    names(pme)[1] = paste0(names(bm), "_PME")
  } else {
    pme = NULL
  }
  return(pme)
}

