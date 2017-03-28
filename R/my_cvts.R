my_cvts = function (x, x_reg = NULL, FUN = NULL, FCFUN = NULL, rolling = T, windowSize = 84,
          maxHorizon = 12, horizonAverage = FALSE, saveModels = ifelse(length(x) >
                                                                        500, FALSE, TRUE), saveForecasts = ifelse(length(x) >
                                                                                                                    500, FALSE, TRUE), verbose = TRUE)
  {
  if (is.null(FUN)) {
    FUN <- function(x) {
      return(x)
    }
  }
  if (is.null(FCFUN)) {
    FCFUN <- forecast::forecast
  }
  f = frequency(x)
  tspx <- tsp(x)
  if (is.null(tspx)) {
    x <- ts(x, frequency = f)
  }
  if (any(sapply(c(x, windowSize, maxHorizon), FUN = function(x) !is.numeric(x)))) {
    stop("The arguments x, windowSize, and maxHorizon must all be numeric.")
  }
  if (any(c(windowSize, maxHorizon) < 1L)) {
    stop("The arguments windowSize, and maxHorizon must be positive integers.")
  }
  if (any(c(windowSize, maxHorizon)%%1L != 0)) {
    stop("The arguments windowSize, and maxHorizon must be positive integers.")
  }
  if (windowSize + 2 * maxHorizon > length(x)) {
    stop("The time series must be longer than windowSize + 2 * maxHorizon.")
  }
  results <- matrix(NA, nrow = ifelse(rolling, length(x) - windowSize, as.integer((length(x) - windowSize)/maxHorizon)),
                    ncol = maxHorizon)
  forecasts <- fits <- vector("list", nrow(results))
  startWindow <- 1
  endWindow <- windowSize
  stsp <- tsp(x)[1]
  for (i in 1:nrow(results)) {
    if (verbose) {
      cat("Fitting fold", i, "of", nrow(results), "\n")
    }
    if (rolling) {
      etsp <- stsp + (i + windowSize - 2)/frequency(x)
      y <- window(x, start = stsp, end = etsp)

      fstsp <- stsp + (i + windowSize - 1)/frequency(x)
      fetsp <- fstsp + (maxHorizon - 1)/frequency(x)
      if(!is.null(x_reg)){
        x_reg_train = matrix(x_reg[1:(windowSize + i - 1), ], ncol = ncol(x_reg))
        x_reg_test = matrix(x_reg[(windowSize + i):min(windowSize + i + maxHorizon - 1, nrow(x_reg)),], ncol = ncol(x_reg))
        colnames(x_reg_train) =  colnames(x_reg_test) = colnames(x_reg)
      }
    }
    else {
      etsp <- stsp + (windowSize - 1)/frequency(x) + maxHorizon *
        (i - 1)/frequency(x)
      y <- window(x, end = etsp)
      fstsp <- tsp(y)[2] + 1/frequency(x)
      fetsp <- stsp + (windowSize - 1)/frequency(x) +
        maxHorizon * i/frequency(x)
    }
    ynext <- window(x, start = fstsp, end = min(fetsp, tsp(x)[2]))
    if(is.null(x_reg)){
      mod <- do.call(FUN, list(y))
      fc <- do.call(FCFUN, list(mod, h = maxHorizon))
    } else {
      mod <- do.call(FUN, list(y, xreg = x_reg_train))
      fc <- do.call(FCFUN, list(mod, h = min(maxHorizon, nrow(x_reg_test)), xreg = x_reg_test))
    }

    if (saveModels) {
      fits[[i]] <- mod
    }
    if (saveForecasts) {
      forecasts[[i]] <- fc
    }
    if(length(ynext) == length(fc$mean)){
      if(length(ynext) > 1){
        results[i, 1:length(fc$mean)] <- ynext - fc$mean
      } else {
        results[i, 1:length(fc$mean)] <- ynext[1] - fc$mean[1]
      }

    } else {
      numnext = length(ynext)
      results[i, 1:numnext] = ynext - fc$mean[1:numnext]
    }
    #row.names(results)[i] =  etsp
  }
  row.names(results) = round(as.vector(time(subset(x, start = windowSize, end = length(x) - 1))), 3)
  if (horizonAverage) {
    results <- as.matrix(rowMeans(results), ncol = 1)
  }
  if (!saveModels) {
    fits <- NULL
  } else {
    names(fits) = round(as.vector(time(subset(x, start = windowSize, end = length(x) - 1))), 3)
  }
  if (!saveForecasts) {
    forecasts <- NULL
  } else {
    names(forecasts) = round(as.vector(time(subset(x, start = windowSize, end = length(x) - 1))), 3)
  }
  result <- list(forecasts = forecasts, models = fits, residuals = results)
  #class(result) <- "cvts"
  return(result)
}
