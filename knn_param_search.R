#' Searches for the optimal values of k and d for a given time series. First, values corresponding to instants from initial + 1 to the last one
#' are predicted. The first value predicted, which corresponds to instant initial + 1, is calculated using instants from 1 to
#' instant initial; the second value predicted, which corresponds to instant initial + 2, is predicted using instants from 1
#' to instant initial + 1; and so on until the last value, which corresponds to instant n (length of the given time series),
#' is predicted using instants from 1 to instant n - 1. Finally, the error is evaluated between the predicted values and
#' the real values of the series.
#' This version of the optimization function uses a parallelized distances calculation function, and the computation of
#' the predicted values is done parallelizing by the number of d's.
#'
#' @param y A time series.
#' @param k Values of k's to be analyzed.
#' @param d Values of d's to be analyzed.
#' @param initial Variable that determines the limit of the known past for the first instant predicted.
#' @param distance Type of metric to evaluate the distance between points. Many metrics are supported: euclidean, manhattan,
#' dynamic time warping, camberra and others. For more information about the supported metrics check the values that 'method'
#' argument of function parDist (from parallelDist package) can take as this is the function used to calculate the distances.
#' Link to the package info: https://cran.r-project.org/web/packages/parallelDist
#' Some of the values that this argument can take are "euclidean", "manhattan", "dtw", "camberra", "chord".
#' @param error_measure Type of metric to evaluate the prediction error.
#' Five metrics supported:
#' \describe{
#'   \item{ME}{Mean Error}
#'   \item{RMSE}{Root Mean Squared Error}
#'   \item{MAE}{Mean Absolute Error}
#'   \item{MPE}{Mean Percentage Error}
#'   \item{MAPE}{Mean Absolute Percentage Error}
#' }
#' @param weight Type of weight to be used at the time of calculating the predicted value with a weighted mean.
#' Three supported: proportional , average, linear.
#' \describe{
#'   \item{proportional}{the weight assigned to each neighbor is inversely proportional to its distance}
#'   \item{average}{all neighbors are assigned with the same weight}
#'   \item{linear}{nearest neighbor is assigned with weight k, second closest neighbor with weight k-1, and so on until the
#'                least nearest neighbor which is assigned with a weight of 1.}
#' }
#' @param v Variable to be predicted if given multivariate time series.
#' @param threads Number of threads to be used when parallelizing, default is 1.
#' @return A matrix of errors, optimal k and d. All tested ks and ks and all the used metrics.
#' @examples
#' knn_param_search(AirPassengers, 1:5, 1:3)
#' knn_param_search(LakeHuron, 1:10, 1:6)
knn_param_search <- function(y, k, d, initial = NULL, distance = "euclidean", error_measure = "MAE", weight = "proportional", v = 1, threads = 1){
  require(parallelDist)
  require(forecast)
  require(foreach)
  require(doParallel)
  require(iterators)

  if ( any( is.na(y) ) ) {
    stop("There are NAs values in the time series")
  }
  
  if ( any( is.nan(y) )) {
    stop("There are NaNs values in the time series")
  }
  
  # Default number of threads to be used
  if (is.null(threads)) {
    cores <- parallel::detectCores(logical = FALSE)
    threads <- ifelse(cores == 1, cores, cores - 1)
  }

  # Choose the appropiate index of the accuracy result, depending on the error_measure
  error_type <- switch(error_measure,
                      ME = 1,
                      RMSE = 2,
                      MAE = 3,
                      MPE = 4,
                      MAPE = 5,
                      0
  )
  
  if ( error_type == 0 ) {
    stop(paste0("Error measure '", error_measure, "' unrecognized."))
  }
  
  if ( all( weight != c("proportional", "average", "linear") ) ) {
    stop(paste0("Weight metric '", weight, "' unrecognized."))
  }

  # Sort k or d vector if they are unsorted
  if (is.unsorted(k)) {
      k <- sort(k)
  }
  if (is.unsorted(d)) {
      d <- sort(d)
  }

  model <- list()
  class(model) <- "kNN"

  model$x <- y
  
  # Initialization of variables to be used
  if ( any(class(y) == "tbl_ts")) {
    require(tsibble)
    if (length(tsibble::measured_vars(y)) < v ) {
      stop(paste0("Index of variable off limits: v = ", v, " but given time series has ", length(measured_vars(y)), " variables."))
    }
    y <- matrix(sapply( y[ measured_vars(y) ], as.double), ncol = length(measures(y) ) )
  }
  else{
    if ( NCOL(y) < v ) {
      stop(paste0("Index of variable off limits: v = ", v, " but given time series has ", NCOL(y), " variables."))
    }
    y <- matrix(sapply(y, as.numeric), ncol = NCOL(y))
  }
  
  n <- NROW(y)
  
  if ( max(d) + max(k) >= n ) {
    stop(paste0("timeseries length must be higher than k+d: ", max(k), "+", max(d), " >= ", n))
  }
  
  ks <- length(k)
  ds <- length(d)
  initial <- ifelse(is.null(initial), floor(n * 0.7), initial)
  real_values <- matrix(y[(initial + 1):n, v])
  
  # This next line is only there to avoid 'No visible binding for global variable' warning
  # in R CMD check due to i variable used in foreach loop
  i <- NULL

  # For each d an 'elements' matrix and a distances matrix is calculated. Then, with the two inner loops
  # all combinations of instants initial to n - 1 and k's values are generated in order to predict values
  # using k-nn algorithm and calculate errors.

  clust <- parallel::makeCluster(threads)
  doParallel::registerDoParallel(cl = clust)

  errors_matrix <- foreach(i = 1:ds, .combine = cbind, .packages = c("forecast", "parallelDist"), .export = "knn_elements") %dopar% {
    predictions <- matrix(nrow = ks, ncol = n - initial)
    errors <- vector(mode = "numeric", ks)

    # Get 'elements' matrices (one per variable)
    distances <- plyr::alply(y, 2, function(y_col) knn_elements(matrix(y_col, ncol = 1), d[i]))
    
    # For each of the elements matrices, calculate the distances between 
    # every 'element'. This results in a list of triangular matrices.
    distances <- plyr::llply(distances, function(distances) parallelDist::parDist(distances, distance, threads = threads))
    
    # Combine all distances matrices by aggregating them
    distances <- Reduce('+', distances)
    distances_size <- attr(distances, "Size")

    for (j in (n - initial + 1):2) {
      # Get column needed from the distances matrix and sort it
      initial_index <- distances_size * (j - 1) - j * (j - 1) / 2 + 1
      distances_col <- distances[ initial_index:(initial_index + n - d[i] - j) ]
      # sorted_dists <- sort.int(distances_col, index.return = TRUE)
      sorted_dists <- which( distances_col <= sort.int(distances_col, partial = max(k))[max(k)], arr.ind = TRUE)
      sorted_dists <- head(sorted_dists[sort.int(distances_col[sorted_dists], index.return = TRUE, decreasing = FALSE)$ix], max(k))

      for (k_index in 1:ks) {
        k_value <- k[k_index]

        # Get the indexes of the k nearest 'elements', these are called neighbors
        k_nn <- head(sorted_dists, k_value)

        # Calculate the weights for the future computation of the weighted mean
        weights <- switch(weight,
                          proportional = 1 / (distances_col[k_nn] + .Machine$double.xmin * 1e150),
                          average = rep.int(1, k_value),
                          linear = k_value:1
                        )

        # Calculate the predicted value
        predictions[k_index, n - initial + 2 - j] <- weighted.mean(y[n - j + 2 - k_nn, v], weights)
      }
    }

    # Calculate error values between the known values and the predicted values, these values
    # correspond to instants initial to n - 1. This is done for the current d and all k's
    for (k_index in 1:ks) {
      errors[k_index] <- forecast::accuracy(ts(predictions[k_index, ]), real_values)[error_type]
    }

    errors
  }

  foreach::registerDoSEQ()
  parallel::stopCluster(clust)

  # Construction of the list to be returned
  index_min_error <- which.min(errors_matrix)
  opt_k <- k[((index_min_error - 1) %% ks) + 1]
  opt_d <- d[ceiling(index_min_error / ks)]
  dimnames(errors_matrix) <- list(k, d)
  
  model$method <- "k-Nearest Neighbors"
  model$opt_k <- opt_k
  model$opt_d <- opt_d
  model$tested_ds <- d
  model$tested_ks <- k
  model$errors <- errors_matrix
  model$initial <- initial
  model$distance <- distance
  model$error <- error_measure
  model$weight <- weight
  model$threads <- threads
  model$call <- deparse(sys.call())
  
  #result
  model
}
