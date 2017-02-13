executeSP <- function(procname, paramstring, db = "Hatteras_Sandbox_Tools", schema = "dbo"){
  database = paste("database", db, sep = "=")
  ch = paste("driver={SQL Server}", "server=HAT-SQL-01",database, "trusted_connection=true", sep = "; ")
  ch = RODBC::odbcDriverConnect(ch)
  procString = paste(db, schema, procname, sep = ".")
  query <- paste("exec", procString, paramstring, sep = " ")
  res = RODBC::sqlQuery(ch, query, believeNRows = FALSE);
  #return(query)
  RODBC::odbcClose(ch)
  return(res)
}

