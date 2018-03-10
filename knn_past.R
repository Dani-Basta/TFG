#' Predicts values of the time series using k-nearest neighbors algorithm. The first predicted value
#' corresponds to instant init + 1 and its determined by the first instants until init, the same is
#' done for instant init + 2 and so on until the last instant of the time series.
#'
#' @param x A time series
#' @param k Number of neighbors
#' @param d Length of each of the elements
#' @param init A value that has to satisfy d <= init < n, values are predicted from this point of the time series to the end
#' @param v Variable to be predicted if given multivariate time series
#' @param distance_metric Type of metric to evaluate the distance between points. Many metrics are supported: euclidean, manhattan,
#' dynamic time warping, camberra and others. For more information about the supported metrics check the values that 'method'
#' argument of function parDist (from parallelDist package) can take as this is the function used to calculate the distances.
#' Link to the package info: https://cran.r-project.org/web/packages/parallelDist
#' Some of the values that this argument can take are "euclidean", "manhattan", "dtw", "camberra", "chord".
#' @param weight Type of weight to use at the time of calculating the predicted value with a weighted mean.
#- Three supported: proximity, same, trend.
#' \describe{
#'   \item{proximity}{the weight assigned to each neighbor is proportional to its distance}
#'   \item{same}{all neighbors are assigned with the same weight}
#'   \item{trend}{nearest neighbor is assigned with weight k, second closest neighbor with weight k-1, and so on until the
#'                least nearest neighbor which is assigned with a weight of 1.}
#' }
#' @param threads Number of threads to be used when parallelizing distances calculation
#' @return The predicted value

knn_past = function(x, k, d, v = 1, init, distance_metric = "euclidean", weight = "proximity", threads = 1) {
    require(parallelDist)
    y <- matrix(x, ncol = NCOL(x))
    n <- NROW(y)
    m <- NCOL(y)
    predictions <- array(dim = n - init)

    # Get elements matrix
    elements_matrix <- knn_elements(y, d)

    # This happens if d=1 and a univariate time series is given, a very unusual case
    # This transformation is needed so that parDist doesn't throw an error
    if (is(elements_matrix, "numeric")) {
      elements_matrix <- matrix(elements_matrix, nrow = length(curr_elems))
    }

    # Calculate distances between every element, a 'triangular matrix' is returned
    distances_matrix <- parDist(elements_matrix, distance_metric, threads = threads)
    distances_size <- attr(distances_matrix, "Size")

    prediction_index <- length(predictions)
    for (j in 2:(n - init + 1)) {

        # Get column needed from the distances matrix
        initial_index <- distances_size * (j - 1) - j * (j - 1) / 2 + 1
        distances_col <- distances_matrix[initial_index:(initial_index + n - d - j)]

        # Get the indexes of the k nearest 'elements'
        k_nn <- head((sort.int(distances_col, index.return = TRUE))$ix, k)

        # Calculate the weights for the future computation of the weighted mean
        weights = switch(weight, proximity = {1/(distances_col[k_nn] + .Machine$double.xmin)},
                         same = {rep.int(1, k)},
                         trend = {k:1})

        # Calculate the predicted value
        predictions[prediction_index] <- weighted.mean(y[n - j + 2 - k_nn, v], weights)
        prediction_index <- prediction_index - 1
    }

    predictions
}
