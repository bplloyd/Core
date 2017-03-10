decompose_beta = function(beta, weights){
  beta_decomp = cbind(as.data.frame(lapply(beta, function(f) f * weights)), data.frame(residual = weights))
  beta_port = colSums(beta_decomp)

  beta_flat = unlist(beta_decomp)
  for(j in 1:ncol(beta_decomp)){
    for(i in 1:nrow(beta_decomp)){
      names(beta_flat)[nrow(beta_decomp)*(j - 1) + i] = paste(colnames(beta_decomp)[j], row.names(beta_decomp)[i], sep = "_")
    }
  }
  list(port_beta = beta_port, decomp = beta_decomp, flattened = beta_flat)
}
