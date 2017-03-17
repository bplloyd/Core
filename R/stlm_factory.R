stlm_factory = function(boxCox = T, s.window = "periodic", robust=F, method = "ets", modelfunction = NULL, model = NULL, etsmodel = "ZZN", biasadj = F, xreg = NULL, allow.multiplicative.trend = F, ... ){
  if(boxCox){
    return(function(x)forecast::stlm(x,
                          s.window = s.window,
                          robust = robust,
                          method = method,
                          modelfunction = modelfunction,
                          model = model,
                          etsmodel = etsmodel,
                          biasadj = biasadj,
                          xreg = xreg,
                          allow.multiplicative.trend = allow.multiplicative.trend,
                          lambda = forecast::BoxCox.lambda(x),
                          ...))
  } else {
    return(function(x) forecast::stlm(x,
                          s.window = s.window,
                          robust = robust,
                          method = method,
                          modelfunction = modelfunction,
                          model = model,
                          etsmodel = etsmodel,
                          biasadj = biasadj,
                          xreg = xreg,
                          allow.multiplicative.trend = allow.multiplicative.trend,
                          lambda = NULL,
                          ...))
  }
}


arima_factory = function(order = NULL, seasonal = NULL, boxCox = T, xreg = NULL, include.mean = T, include.drift = F, biasadj = F) {
  if(is.null(order) & is.null(seasonal)){
    if(boxCox) {
      return(function(x) forecast::auto.arima(x,
                                              max.P = 5,
                                              max.Q = 5,
                                              max.d = 5,
                                              max.D = 5,
                                              lambda = forecast::BoxCox.lambda(x),
                                              biasadj = biasadj
                                              ))
    } else {
      return(function(x) forecast::auto.arima(x,
                                              max.P = 5,
                                              max.Q = 5,
                                              max.d = 5,
                                              max.D = 5,
                                              biasadj = biasadj
      ))
    }
  } else {
    if(boxCox) {
      return(function(x) forecast::Arima(x,
                                         order = order,
                                         seasonal = seasonal,
                                         include.mean = include.mean,
                                         include.drift = include.drift,
                                         lambda = forecast::BoxCox.lambda(x),
                                          biasadj = biasadj
      ))
    } else {
      return(function(x) forecast::Arima(x,
                                         order = order,
                                         seasonal = seasonal,
                                         include.mean = include.mean,
                                         include.drift = include.drift,
                                         biasadj = biasadj
      ))
    }
  }
}

nnetar_factory = function(boxCox = T, p=NULL, P=1, size=NULL, repeats = 20) {
  if(boxCox) {
    if(is.null(p)) {
      if(is.null(size)){
        return(function(x) forecast::nnetar(x, P=P, repeats = repeats, lambda = forecast::BoxCox.lambda(x)))
      } else {
        return(function(x) forecast::nnetar(x, P=P, size = size, repeats = repeats, lambda = forecast::BoxCox.lambda(x)))
      }
    } else {
      if(is.null(size)){
        return(function(x) forecast::nnetar(x, p = p, P=P, repeats = repeats, lambda = forecast::BoxCox.lambda(x)))
      } else {
        return(function(x) forecast::nnetar(x, p = p, P=P, size = size, repeats = repeats, lambda = forecast::BoxCox.lambda(x)))
      }
    }
  } else {
    if(is.null(p)) {
      if(is.null(size)){
        return(function(x) forecast::nnetar(x, P=P, repeats = repeats))
      } else {
        return(function(x) forecast::nnetar(x, P=P, size = size, repeats = repeats))
      }
    } else {
      if(is.null(size)){
        return(function(x) forecast::nnetar(x, p = p, P=P, repeats = repeats))
      } else {
        return(function(x) forecast::nnetar(x, p = p, P=P, size = size, repeats = repeats))
      }
    }
  }
}
