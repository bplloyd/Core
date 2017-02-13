loadIndices = function(idxNames = "SPTR"){
  # require(RODBC)
  # require(data.table)
  # require(xts)
  # require(PerformanceAnalytics)
  idxFilter = "("
  for (n in idxNames){
    if (idxFilter == "("){

      idxFilter = paste0(idxFilter, "'", n, "'")
    } else {

      idxFilter = paste0(idxFilter, ", '", n, "'")
    }
  }
  idxFilter = paste0(idxFilter, ")")
  cn = RODBC::odbcDriverConnect("driver={SQL Server}; server=HAT-SQL-01; database=Hatteras_Sandbox_Tools; trusted_connection=true")
  sql = paste0("SELECT CAST(v.DateReported AS date) 'DateReported', v.Ticker, v.PX_LAST FROM v_Index_Px_Daily AS v WHERE v.Ticker IN ", idxFilter)
  indices = data.table::dcast(data.table::data.table(RODBC::sqlQuery(cn, sql)), formula = DateReported ~ Ticker, fun.aggregate = mean)
  indices$DateReported = as.Date.factor(indices$DateReported)
  RODBC::odbcClose(cn)
  return(data.table::as.xts.data.table(indices)[, idxNames])
}
