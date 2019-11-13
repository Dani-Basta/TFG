#' Predicts next value of the time series using k-nearest neighbors algorithm.
#'
#' @param y A time series.
#' @param k Number of neighbors.
#' @param d Length of each of the 'elements'.
#' @param v Variable to be predicted if given multivariate time series.
#' @param distance_metric Type of metric to evaluate the distance between points. Many metrics are supported: euclidean, manhattan,
#' dynamic time warping, camberra and others. For more information about the supported metrics check the values that 'method'
#' argument of function parDist (from parallelDist package) can take as this is the function used to calculate the distances.
#' Link to the package info: https://cran.r-project.org/web/packages/parallelDist
#' Some of the values that this argument can take are "euclidean", "manhattan", "dtw", "camberra", "chord".
#' @param weight Type of weight to be used at the time of calculating the predicted value with a weighted mean.
#' Three supported: proximity, same, linear.
#' \describe{
#'   \item{proximity}{the weight assigned to each neighbor is proportional to its distance}
#'   \item{same}{all neighbors are assigned with the same weight}
#'   \item{linear}{nearest neighbor is assigned with weight k, second closest neighbor with weight k-1, and so on until the
#'                least nearest neighbor which is assigned with a weight of 1.}
#' }
#' @param threads Number of threads to be used when parallelizing distances calculation, default is number of cores detected - 1 or
#' 1 if there is only one core.
#' @return The predicted value.
#' @examples
#' knn_next(AirPassengers, 5, 2)
#' knn_next(LakeHuron, 3, 6)
knn_next <- function(y, k, d, v = 1, distance_metric = "euclidean", weight = "proximity", threads = 1) {
  require(parallelDist)
  require(parallel)
  
  # Default number of threads to be used
  if (is.null(threads)) {
    cores <- parallel::detectCores( logical = FALSE )
    threads <- ifelse(cores == 1, cores, cores - 1)
  }
  
  forec <- list()
  class(forec) <- "forecast"
  forec$method <- "k-Nearest Neighbors for unknown observations"
  
  if (class(y) == "kNN") {
    forec$model <- y
    
    k <- y$opt_k
    d <- y$opt_d
    distance_metric <- y$distance
    weight <- y$weight
    threads <- threads
    
    y <- y$x
  }
  else {
    model <- list()
    class(model) <- "kNN"
    model$method <- "k-Nearest Neighbors"
    model$k <- k
    model$d <- d
    model$distance <- distance_metric
    model$weight <- weight
    forec$model <- model
  }
  
  if ( any( is.na(y) ) ) {
    stop("There are NAs values in the time series")
  }
  
  if ( any( is.nan(y) )) {
    stop("There are NaNs values in the time series")
  }
  
  if ( all( weight != c("proximity", "same", "linear") ) ) {
    stop(paste0("Weight metric '", weight, "' unrecognized."))
  }
  
  
  # Initialization of variables to be used
  n <- NROW(y)
  forec$x <- y

  if ( any(class(y) == "ts" ) ) {
    require(tseries)
    
    if ( NCOL(y) < v ) {
      stop(paste0("Index of variable off limits: v = ", v, " but given time series has ", NCOL(y), " variables."))
    }
    
    sta <- time(y)[n]
    freq <- frequency(y)
    resType = "ts"
    
    y <- matrix(sapply(y, as.double), ncol = NCOL(y))
  }
  else if ( any(class(y) == "tbl_ts")) {
    require(tsibble)
    
    if (length(tsibble::measured_vars(y)) < v ) {
      stop(paste0("Index of variable off limits: v = ", v, " but given time series has ", length(measured_vars(y)), " variables."))
    }
    
    resul <- tail( append_row(y), 1 )

    resType = "tsibble"
    
    # y <- matrix(sapply(y$value, as.double), ncol = 1)
    # 
    # y <- matrix(sapply( y[ measured_vars(y)[v] ], as.double), ncol = 1)
    
    y <- matrix(sapply( y[ tsibble::measured_vars(y) ], as.double), ncol = length(tsibble::measures(y) ))
  } 
  else{
    resType = "undef"
    
    if ( NCOL(y) < v ) {
      stop(paste0("Index of variable off limits: v = ", v, " but given time series has ", NCOL(y), " variables."))
    }
    
    y <- matrix(sapply(y, as.double), ncol = NCOL(y))
  }
  
  # y <- matrix(sapply(y, as.numeric), ncol = NCOL(y), byrow = FALSE)
  
  expSmoVal <- 0.5

  # Get 'elements' matrices (one per variable)
  elements_matrices <- plyr::alply(y, 2, function(y_col) knn_elements(matrix(y_col, ncol = 1), d))

  # For each of the elements matrices, obtain the distances between 
  # the most recent 'element' and the rest of the 'elements'.
  # This results in a list of distances vectors
  distances_vectors <- plyr::llply(elements_matrices, function(elements_matrix) parallelDist::parDist(elements_matrix, distance_metric, threads = threads)[1:(n - d)])
  
  # Combine all distances vectors by aggregating them
  distances <- Reduce('+', distances_vectors)

  # Get the indexes of the k nearest 'elements', these are called neighbors
  k_nn <- head((sort.int(distances, index.return = TRUE))$ix, k)
  
  forec$neighbors <- k_nn
  
  if ( weight == "expSmooth" )
      k_nn <- sort.int(k_nn)

  # Calculate the weights for the future computation of the weighted mean
  weights <- switch(weight, 
                    proximity = 1 / (distances[k_nn] + .Machine$double.xmin * 1e150),
                    same = rep.int(1, k),
                    linear = k:1,
                    #expSmooth = expSmoVal ** k_value:1
                    expSmooth = expSmoVal * (1 - expSmoVal) ** (k - 1):0 
                )

  # Calculate the predicted value
  prediction <- weighted.mean(y[n - k_nn + 1, v], weights)
  
  
  
  if ( resType == "ts" )  {
    forec$mean <- tail(ts(c(1, prediction), start = sta, frequency = freq ), 1)
  }
  else if ( resType == "tsibble" ) {
    # resul$value <- prediction
    resul[ measured_vars(resul)[v] ] <- prediction
    forec$mean <- resul
  } 
  else{
    forec$mean <- prediction
  }
  
  forec$lower <- NA
  forec$upper <- NA
  
  forec$residuals <- tail(y[,v], 1) - prediction
  
  forec
  
}
