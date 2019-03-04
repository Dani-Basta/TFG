#' Past time prediction
#'
#' Predicts values of the time series using k-nearest neighbors algorithm. Values corresponding to instants from init + 1 to
#' the last one are predicted. The first value predicted, which corresponds to instant init + 1, is calculated using instants
#' from 1 to instant init; the second value predicted, which corresponds to instant init + 2, is predicted using instants from
#' 1 to instant init + 1; and so on until the last value, which corresponds to instant n (length of the given time series),
#' is predicted using instants from 1 to instant n - 1.
#'
#' @param y A time series.
#' @param k Number of neighbors.
#' @param d Length of each of the 'elements'.
#' @param v Variable to be predicted if given multivariate time series.
#' @param init Variable that determines the limit of the known past for the first instant predicted.
#' @param distance_metric Type of metric to evaluate the distance between points. Many metrics are supported: euclidean, manhattan,
#' dynamic time warping, canberra and others. For more information about the supported metrics check the values that 'method'
#' argument of function 'parDist' (from 'parallelDist' package) can take as this is the function used to calculate the distances.
#' Link to the package info: \url{https://cran.r-project.org/package=parallelDist}.
#' Some of the values that this argument can take are "euclidean", "manhattan", "dtw", "canberra", "chord".
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
#' @return The predicted values.
#' @examples
#' knn_past(AirPassengers, 5, 2, threads = 2)
#' knn_past(LakeHuron, 3, 6, threads = 2)
#' @export
knn_past <- function(y, k, d, v = 1, init = NULL, distance_metric = "euclidean", weight = "proximity", threads = NULL) {

  # Default number of threads to be used
  if (is.null(threads)) {
    cores <- parallel::detectCores()
    threads <- ifelse(cores == 1, cores, cores - 1)
  }

  # Initialization of variables to be used
  y <- matrix(y, ncol = NCOL(y))
  n <- NROW(y)
  init <- ifelse(is.null(init), init <- floor(n * 0.7), init)
  predictions <- array(dim = n - init)

  # Get 'elements' matrix
  elements_matrix <- knn_elements(y, d)

  # Calculate distances between every 'element', a 'triangular matrix' is returned
  distances_matrix <- parallelDist::parDist(elements_matrix, distance_metric, threads = threads)
  distances_size <- attr(distances_matrix, "Size")

  prediction_index <- length(predictions)
  for (j in 2:(n - init + 1)) {
      # Get column needed from the distances matrix
      initial_index <- distances_size * (j - 1) - j * (j - 1) / 2 + 1
      distances_col <- distances_matrix[initial_index:(initial_index + n - d - j)]

      # Get the indexes of the k nearest 'elements', these are called neighbors
      k_nn <- utils::head((sort.int(distances_col, index.return = TRUE))$ix, k)

      # Calculate the weights for the future computation of the weighted mean
      weights <- switch(weight, 
                        proximity = 1 / (distances_col[k_nn] + .Machine$double.xmin * 1e150),
                        same = rep.int(1, k),
                        linear = k:1)

      # Calculate the predicted value
      predictions[prediction_index] <- stats::weighted.mean(y[n - j + 2 - k_nn, v], weights)
      prediction_index <- prediction_index - 1
  }

  predictions
}
