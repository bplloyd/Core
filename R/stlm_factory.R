stlm_factory = function(boxCox = T, s.window = "periodic", robust=F, method = "ets", modelfunction = NULL, model = NULL, etsmodel = "ZZN", biasadj = F, allow.multiplicative.trend = F, ... ){
  if(boxCox){
    return(function(x, xreg=NULL)forecast::stlm(x,
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
    return(function(x, xreg=NULL) forecast::stlm(x,
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
ets_factory = function(boxCox = T, model = "ZZZ", biasadj = F, damped = NULL, allow.multiplicative.trend = F, ... ){
  if(boxCox){
    return(function(x, xreg=NULL)forecast::ets(y = x,
                                                model = model,
                                                lambda = forecast::BoxCox.lambda(x),
                                                biasadj = biasadj,
                                                allow.multiplicative.trend = allow.multiplicative.trend,
                                               damped = damped,
                                                ...))
  } else {
    return(function(x, xreg=NULL)forecast::ets(y = x,
                                               model = model,
                                               lambda = NULL,
                                               biasadj = biasadj,
                                               allow.multiplicative.trend = allow.multiplicative.trend,
                                               damped = damped,
                                               ...))
  }
}

arima_factory = function(order = NULL, seasonal = NULL, boxCox = T, include.mean = T, include.drift = F, biasadj = F) {
  if(is.null(order) & is.null(seasonal)){
    if(boxCox) {
      return(function(x, xreg=NULL) forecast::auto.arima(x,
                                              max.P = 5,
                                              max.Q = 5,
                                              max.d = 5,
                                              max.D = 5,
                                              lambda = forecast::BoxCox.lambda(x),
                                              biasadj = biasadj,
                                              xreg = xreg
                                              ))
    } else {
      return(function(x, xreg=NULL) forecast::auto.arima(x,
                                              max.P = 5,
                                              max.Q = 5,
                                              max.d = 5,
                                              max.D = 5,
                                              biasadj = biasadj,
                                              xreg = xreg
      ))
    }
  } else {
    if(boxCox) {
      return(function(x, xreg=NULL) forecast::Arima(x,
                                         order = order,
                                         seasonal = seasonal,
                                         include.mean = include.mean,
                                         include.drift = include.drift,
                                         lambda = forecast::BoxCox.lambda(x),
                                          biasadj = biasadj,
                                         xreg = xreg
      ))
    } else {
      return(function(x, xreg=NULL) forecast::Arima(x,
                                         order = order,
                                         seasonal = seasonal,
                                         include.mean = include.mean,
                                         include.drift = include.drift,
                                         biasadj = biasadj,
                                         xreg = xreg
      ))
    }
  }
}

nnetar_factory = function(boxCox = T, p=NULL, P=1, size=NULL, repeats = 20) {
  if(boxCox) {
    if(is.null(p)) {
      if(is.null(size)){
        return(function(x, xreg=NULL) forecast::nnetar(x, P=P, repeats = repeats, lambda = forecast::BoxCox.lambda(x), xreg = xreg))
      } else {
        return(function(x, xreg=NULL) forecast::nnetar(x, P=P, size = size, repeats = repeats, lambda = forecast::BoxCox.lambda(x), xreg = xreg))
      }
    } else {
      if(is.null(size)){
        return(function(x, xreg=NULL) forecast::nnetar(x, p = p, P=P, repeats = repeats, lambda = forecast::BoxCox.lambda(x), xreg = xreg))
      } else {
        return(function(x, xreg=NULL) forecast::nnetar(x, p = p, P=P, size = size, repeats = repeats, lambda = forecast::BoxCox.lambda(x), xreg = xreg))
      }
    }
  } else {
    if(is.null(p)) {
      if(is.null(size)){
        return(function(x, xreg=NULL) forecast::nnetar(x, P=P, repeats = repeats, xreg = xreg))
      } else {
        return(function(x, xreg=NULL) forecast::nnetar(x, P=P, size = size, repeats = repeats, xreg = xreg))
      }
    } else {
      if(is.null(size)){
        return(function(x, xreg=NULL) forecast::nnetar(x, p = p, P=P, repeats = repeats, xreg = xreg))
      } else {
        return(function(x, xreg=NULL) forecast::nnetar(x, p = p, P=P, size = size, repeats = repeats, xreg = xreg))
      }
    }
  }
}

hybrid_factory = function(models = "ans", boxCox = T, s.args = NULL, a.args = NULL, e.args = NULL, n.args = NULL,  t.args = NULL, parallel = T) {
  if(boxCox) {
    return(function(x) forecastHybrid::hybridModel(x,
                                                   models = models,
                                                   a.args = a.args,
                                                   s.args = s.args,
                                                   e.args = e.args,
                                                   n.args = n.args,
                                                   t.args = t.args,
                                                   lambda = forecast::BoxCox.lambda(x),
                                                   parallel = parallel

    ))
  } else {
    return(function(x)forecastHybrid::hybridModel(x,
                                                  models = models,
                                                  a.args = a.args,
                                                  s.args = s.args,
                                                  e.args = e.args,
                                                  n.args = n.args,
                                                  t.args = t.args,
                                                  parallel = parallel
    ))
  }
}
