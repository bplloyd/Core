rank_cvts = function(cv_diag, errmeas = "RMSE", nahead = "mean_12", topN = NULL, allCols = F) {

  if(!is.null(topN)){
    res = as.data.frame(cv_diag[[errmeas]][order(cv_diag[[errmeas]][, nahead]),][1:topN, ])
  } else {
    res = as.data.frame(cv_diag[[errmeas]][order(cv_diag[[errmeas]][, nahead]), ])
  }
  if(!allCols){
    res = data.frame(res[, nahead], row.names = row.names(res))
    colnames(res) = nahead
  }
  return(res)
  # else
  #   cols = 1:ncol(cv_diag[[errmeas]])
}
