#' Next value prediction
#'
#' Predicts next value of the time series using k-nearest neighbors algorithm.
#'
#' @param y A time series.
#' @param k Number of neighbors.
#' @param d Length of each of the 'elements'.
#' @param v Variable to be predicted if given multivariate time series.
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
#' @param threads Number of threads to be used when parallelizing distances calculation, default is number of cores detected - 1 or
#' 1 if there is only one core.
#' @return The predicted value.
#' @examples
#' knn_next(AirPassengers, 5, 2, threads = 2)
#' knn_next(LakeHuron, 3, 6, threads = 2)
#' @export
knn_next <- function(y, k, d, v = 1, distance_metric = "euclidean", weight = "proximity", threads = NULL) {
  
  # Default number of threads to be used
  if (is.null(threads)) {
    cores <- parallel::detectCores()
    threads <- ifelse(cores == 1, cores, cores - 1)
  }

  # Initialization of variables to be used
  y <- matrix(y, ncol = NCOL(y))
  n <- NROW(y)

  # Get 'elements' matrix
  elements_matrix <- knn_elements(y, d)

  # Calculate distances between every 'element', a 'triangular matrix' is returned
  # Only the first column is used because it corresponds to the distances
  # between the most recent 'element' and the rest of the 'elements'
  distances <- parallelDist::parDist(elements_matrix, distance_metric, threads = threads)[1:(n - d)]

  # Get the indexes of the k nearest 'elements', these are called neighbors
  k_nn <- utils::head((sort.int(distances, index.return = TRUE))$ix, k)

  # Calculate the weights for the future computation of the weighted mean
  weights <- switch(weight, 
                    proximity = 1 / (distances[k_nn] + .Machine$double.xmin * 1e150),
                    same = rep.int(1, k),
                    linear = k:1)

  # Calculate the predicted value
  prediction <- stats::weighted.mean(y[n - k_nn + 1, v], weights)

  prediction
}
