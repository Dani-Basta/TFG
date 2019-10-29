#' Optimizes the values of k and d for a given time series. First, values corresponding to instants from init + 1 to the last one
#' are predicted. The first value predicted, which corresponds to instant init + 1, is calculated using instants from 1 to
#' instant init; the second value predicted, which corresponds to instant init + 2, is predicted using instants from 1
#' to instant init + 1; and so on until the last value, which corresponds to instant n (length of the given time series),
#' is predicted using instants from 1 to instant n - 1. Finally, the error is evaluated between the predicted values and
#' the real values of the series.
#' This version of the optimization function uses a parallelized distances calculation function, and the computation of
#' the predicted values is done parallelizing by the number of d's.
#'
#' @param y A time series.
#' @param k Values of k's to be analyzed.
#' @param d Values of d's to be analyzed.
#' @param v Variable to be predicted if given multivariate time series.
#' @param init Variable that determines the limit of the known past for the first instant predicted.
#' @param distance_metric Type of metric to evaluate the distance between points. Many metrics are supported: euclidean, manhattan,
#' dynamic time warping, camberra and others. For more information about the supported metrics check the values that 'method'
#' argument of function parDist (from parallelDist package) can take as this is the function used to calculate the distances.
#' Link to the package info: https://cran.r-project.org/web/packages/parallelDist
#' Some of the values that this argument can take are "euclidean", "manhattan", "dtw", "camberra", "chord".
#' @param error_metric Type of metric to evaluate the prediction error.
#' Five metrics supported:
#' \describe{
#'   \item{ME}{Mean Error}
#'   \item{RMSE}{Root Mean Squared Error}
#'   \item{MAE}{Mean Absolute Error}
#'   \item{MPE}{Mean Percentage Error}
#'   \item{MAPE}{Mean Absolute Percentage Error}
#' }
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
#' @return A matrix of errors, optimal k and d. All tested ks and ks and all the used metrics.
#' @examples
#' knn_optim_parallel2(AirPassengers, 1:5, 1:3)
#' knn_optim_parallel2(LakeHuron, 1:10, 1:6)mosa
knn_optim_parallel2 <- function(y, k, d, v = 1, init = NULL, distance_metric = "euclidean", error_metric = "MAE", weight = "proximity", threads = NULL){
  require(parallelDist)
  require(forecast)
  require(foreach)
  require(doParallel)
  require(iterators)

  if( any( is.na(y) ) ){
    warning("There are NAs values in the time series", immediate. = TRUE)
  }
  
  if(any( is.nan(y) )){
    warning("There are NaNs values in the time series", immediate. = TRUE)
  }
  
  # Default number of threads to be used
  if (is.null(threads)) {
    cores <- parallel::detectCores()
    threads <- ifelse(cores == 1, cores, cores - 1)
  }

  # Choose the appropiate index of the accuracy result, depending on the error_metric
  error_type <- switch(error_metric,
                      ME = 1,
                      RMSE = 2,
                      MAE = 3,
                      MPE = 4,
                      MAPE = 5,
                      0
  )
  
  if ( error_type == 0 ) {
    error_type <- 3
    warning(paste0("Error metric '", error_metric, "' unrecognized. Using MAE as default"), immediate. = TRUE)
  }
  
  if ( ! any( weight == c("proximity", "same", "linear") ) ) {
    warning(paste0("Weight metric '", weight, "' unrecognized. Using 'proximity' as default"), immediate. = TRUE)
    weight <- "proximity"
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

  
  # Initialization of variables to be used
  if ( any(class(y) == "tbl_ts")) {
    require(tsibble)
    if (length(measured_vars(y)) < v ) {
      stop(paste0("Index of variable off limits: v = ", v, " but given time series has ", length(measured_vars(y)), " variables."))
    }
    y <- matrix(sapply( y[ measured_vars(y) ], as.double), ncol = length(measures(y) ) )
  }
  else{
    y <- matrix(sapply(y, as.numeric), ncol = NCOL(y))
  }
  n <- NROW(y)
  ks <- length(k)
  ds <- length(d)
  init <- ifelse(is.null(init), floor(n * 0.7), init)
  real_values <- matrix(y[(init + 1):n, v])
  
  expSmoVal <- 0.5

  # This next line is only there to avoid 'No visible binding for global variable' warning
  # in R CMD check due to i variable used in foreach loop
  i <- NULL

  # For each d an 'elements' matrix and a distances matrix is calculated. Then, with the two inner loops
  # all combinations of instants init to n - 1 and k's values are generated in order to predict values
  # using k-nn algorithm and calculate errors.

  clust <- makeCluster(threads)
  registerDoParallel(cl = clust)

  errors_matrix <- foreach(i = 1:ds, .combine = cbind, .packages = c("forecast", "parallelDist"), .export = "knn_elements") %dopar% {
    predictions <- matrix(nrow = ks, ncol = n - init)
    errors <- vector(mode = "numeric", ks)

    # Get 'elements' matrix
    elements_matrix <- knn_elements(y, d[i])

    # Calculate distances between every 'element', a 'triangular matrix' is returned
    distances_matrix <- parDist(elements_matrix, method = distance_metric, threads = threads)
    distances_matrix_size <- attr(distances_matrix, "Size")

    for (j in (n - init + 1):2) {
      # Get column needed from the distances matrix and sort it
      initial_index <- distances_matrix_size * (j - 1) - j * (j - 1) / 2 + 1
      distances_col <- distances_matrix[ initial_index:(initial_index + n - d[i] - j) ]
      sorted_distances_col <- sort.int(distances_col, index.return = TRUE)

      for (k_index in 1:ks) {
        k_value <- k[k_index]

        # Get the indexes of the k nearest 'elements', these are called neighbors
        k_nn <- head(sorted_distances_col$ix, k_value)
        
        if ( weight == "expSmooth" ) {
            k_nn <- sort.int(k_nn)
        }

        # Calculate the weights for the future computation of the weighted mean
        weights <- switch(weight,
                          proximity = 1 / (distances_col[k_nn] + .Machine$double.xmin * 1e150),
                          same = rep.int(1, k_value),
                          linear = k_value:1,
                          #expSmooth = expSmoVal ** k_value:1
                          expSmooth = expSmoVal * (1 - expSmoVal) ** (k_value - 1):0 ,
                          rep.int(1, k_value)
                        )

        # Calculate the predicted value
        predictions[k_index, n - init + 2 - j] <- weighted.mean(y[n - j + 2 - k_nn, v], weights)
      }
    }

    # Calculate error values between the known values and the predicted values, these values
    # correspond to instants init to n - 1. This is done for the current d and all k's
    for (k_index in 1:ks) {
      errors[k_index] <- accuracy(ts(predictions[k_index, ]), real_values)[error_type]
    }

    errors
  }

  registerDoSEQ()
  stopCluster(clust)

  # Construction of the list to be returned
  index_min_error <- which.min(errors_matrix)
  opt_k <- k[((index_min_error - 1) %% ks) + 1]
  opt_d <- d[ceiling(index_min_error / ks)]
  dimnames(errors_matrix) <- list(k, d)
  
  #result <- list(errors = errors_matrix, k = opt_k, d = opt_d)
  model$method <- "k-Nearest Neighbors"
  model$opt_k <- opt_k
  model$opt_d <- opt_d
  model$tested_ds <- d
  model$tested_ks <- k
  model$errors <- errors_matrix
  model$init_index <- init
  model$distance <- distance_metric
  model$error <- error_metric
  model$weight <- weight
  model$call <- deparse(sys.call())
  
  #result
  model
}
