#' Predicts values of the time series using k-nearest neighbors algorithm. Values corresponding to instants from init + 1 to
#' the last one are predicted. The first value predicted, which corresponds to instant init + 1, is calculated using instants
#' from 1 to instant init; the second value predicted, which corresponds to instant init + 2, is predicted using instants from
#' 1 to instant init + 1; and so on until the last value, which corresponds to instant n (length of the given time series),
#' is predicted using instants from 1 to instant n - 1.
#'
#' @param y A time series.
#' @param k Number of neighbors.
#' @param d Length of each of the 'elements'.
#' @param init Variable that determines the limit of the known past for the first instant predicted.
#' @param v Variable to be predicted if given multivariate time series.
#' @param init Variable that determines the limit of the known past for the first instant predicted.
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
#' @param threads Number of threads to be used when parallelizing, default is number of cores detected - 1 or
#' 1 if there is only one core.
#' @return The predicted value.
#' @examples
#' knn_past(AirPassengers, 5, 2)
#' knn_past(LakeHuron, 3, 6)
knn_past <- function(y, k, d, v = 1, init = NULL, distance_metric = "euclidean", weight = "proximity", threads = 1) {
  require(parallelDist)
  require(parallel)

  if ( any( is.na(y) ) ) {
    stop("There are NAs values in the time series")
  }
  
  if ( any( is.nan(y) )) {
    stop("There are NaNs values in the time series")
  }
  
  if ( all( weight != c("proximity", "same", "linear") ) ) {
    stop(paste0("Weight metric '", weight, "' unrecognized."))
  }
  
  # Default number of threads to be used
  if (is.null(threads)) {
    cores <- parallel::detectCores(logical = FALSE)
    threads <- ifelse(cores == 1, cores, cores - 1)
  }

  # Initialization of variables to be used
  
  n <- NROW(y)
  init <- ifelse(is.null(init), init <- floor(n * 0.7), init)
  
  model <- list()
  class(model) <- "kNN"
  
  model$method <- "k-Nearest Neighbors"
  model$k <- k
  model$d <- d
  model$distance <- distance_metric
  model$weight <- weight
  
  forec <- list()
  class(forec) <- "forecast"
  forec$model <- model
  forec$method <- "k-Nearest Neighbors over known observations"
  
  forec$x <- y

  if ( any(class(y) == "ts" ) ) {
    require(tseries)
    
    if ( NCOL(y) < v ) {
      stop(paste0("Index of variable off limits: v = ", v, " but given time series has ", NCOL(y), " variables."))
    }
    
    sta <- time(y)[init + 1]
    freq <- frequency(y)
    resType = "ts"
    
    y <- matrix(sapply(y, as.double), ncol = NCOL(y))
  }
  else if ( any(class(y) == "tbl_ts")) {
    require(tsibble)
    
    if (length(tsibble::measured_vars(y)) < v ) {
      stop(paste0("Index of variable off limits: v = ", v, " but given time series has ", length(measured_vars(y)), " variables."))
    }

    resul <- tail(y, (n - init ) )
    
    resul[measured_vars(resul)] <- NA
    
    resType = "tsibble"
    # y <- matrix(sapply(y$value, as.double), ncol = 1)
    # 
    # y <- matrix(sapply( y[ tsibble::measured_vars(y)[v] ], as.double), ncol = 1)
    
    y <- matrix(sapply( y[ tsibble::measured_vars(y) ], as.double), ncol = length(measures(y) ))
    
  } 
  else{
    resType = "undef"
    
    if ( NCOL(y) < v ) {
      stop(paste0("Index of variable off limits: v = ", v, " but given time series has ", NCOL(y), " variables."))
    }
    
    y <- matrix(sapply(y, as.double), ncol = NCOL(y))
  }
  
  # y <- matrix(sapply(y, as.double), ncol = NCOL(y))
  
  expSmoVal <- 0.5
  
  predictions <- array(dim = n - init)
  neighbors <- matrix(nrow = k, ncol = n - init)
  
  # Get 'elements' matrices (one per variable)
  elements_matrices <- plyr::alply(y, 2, function(y_col) knn_elements(matrix(y_col, ncol = 1), d))

  # For each of the elements matrices, calculate the distances between 
  # every 'element'. This results in a list of triangular matrices.
  distances_matrices <- plyr::llply(elements_matrices, function(elements_matrix) parallelDist::parDist(elements_matrix, distance_metric, threads = threads))
  
  # Combine all distances matrices by aggregating them
  distances_matrix <- Reduce('+', distances_matrices)
  
  distances_size <- attr(distances_matrix, "Size")

  prediction_index <- length(predictions)
  for (j in 2:(n - init + 1)) {
      # Get column needed from the distances matrix
      initial_index <- distances_size * (j - 1) - j * (j - 1) / 2 + 1
      distances_col <- distances_matrix[initial_index:(initial_index + n - d - j)]

      # Get the indexes of the k nearest 'elements', these are called neighbors
      k_nn <- head((sort.int(distances_col, index.return = TRUE))$ix, k)
      neighbors[,prediction_index] <- k_nn
      
      if ( weight == "expSmooth" )
          k_nn <- sort.int(k_nn)

      # Calculate the weights for the future computation of the weighted mean
      weights <- switch(weight, 
                        proximity = 1 / (distances_col[k_nn] + .Machine$double.xmin * 1e150),
                        same = rep.int(1, k),
                        linear = k:1,
                        #expSmooth = expSmoVal ** k_value:1
                        expSmooth = expSmoVal * (1 - expSmoVal) ** ((k - 1):0) 
                    )

      # Calculate the predicted value
      predictions[prediction_index] <- weighted.mean(y[n - j + 2 - k_nn, v], weights)
      prediction_index <- prediction_index - 1
  }
  if ( resType == "ts")  {
    forec$mean <- ts(predictions, start = sta, frequency = freq )
  }
  else if ( resType == "tsibble" ) {
    resul[ measured_vars(resul)[v] ] <- predictions
    forec$mean <- resul
  } 
  else{
    forec$mean <- predictions
  }
  
  
  forec$lower <- rep(NA, length(predictions))
  forec$upper <- rep(NA, length(predictions))
  
  forec$residuals <- tail(y[,v], length(predictions)) - predictions 
  
  
  forec$neighbors <- neighbors
  forec$init <- init
  
  return(forec)

}
