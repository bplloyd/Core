getFamaFactors = function(code = "FACTORS_M"){
  # require(Quandl)
  # require(xts)
  Quandl::Quandl(code = paste0("KFRENCH/", code), type = "xts")
}
