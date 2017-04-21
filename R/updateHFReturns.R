updateHFReturns = function(){
  HF_RETURNS <- read_excel("C:/Users/blloyd.HF/Dropbox/CF_Model/Core/HF_RETURNS.xlsx",
                sheet = "HF Returns", col_types = c("date",
                "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric",
                "numeric", "numeric", "numeric",
                "numeric"))

  HF_RETURNS = subset(HF_RETURNS, !is.na(HF_RETURNS$Date))
  hf = xts::xts(HF_RETURNS[, 2:ncol(HF_RETURNS)], order.by = zoo::as.yearmon(HF_RETURNS$Date))
  hf = hf[apply(hf, 1, function(r)!all(is.na(r))),]
  saveRDS(hf, "hf_xts.rds")
}
