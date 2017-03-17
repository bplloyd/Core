loadMLFI = function(codes = c("AAATRI", "AATRI", "ATRI", "BBBTRI", "BBTRI", "BTRI" ,  "CCCTRI", "IGEM",  "TRI", "HYOAS", "EMCBI")){
  #require(Quandl)
  #source("Functions/Quandl.key.R")
  #Quandl.api_key(quandl.key)
  # path =  file.path('C:/Users/blloyd.HF/Documents/GitHub/R-risk-mgmt/QuandlData', 'dataset_mlfi.csv')
  # #path =  file.path('QuandlData', 'dataset_mlfi.csv')
  # read.table(file = path, header = T, sep = ",") -> mlfi.codes
  #names = mlfi.codes$dataset_code
  mlfi.codes = paste("ML",codes, sep = '/')
  return(Quandl::Quandl(code = mlfi.codes, type="xts"))
}
