#' Predict next value of the time series using k-nearest neighbors algorithm
#'
#' @param x A time series
#' @param k Number of neighbors
#' @param d Length of each of the elements
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

knn_next = function(x, k, d, v = 1, distance_metric = "euclidean", weight = "proximity", threads = 1) {
    require(parallelDist)
    y <- matrix(x, ncol = NCOL(x))
    n <- NROW(y)
    m <- NCOL(y)

    # Get 'elements' matrix
    elements_matrix <- knn_elements(y, d)

    # This happens if d=1 and a univariate time series is given, a very unusual case
    # This transformation is needed so that parDist doesn't throw an error
    if (is(elements_matrix, "numeric")) {
      elements_matrix <- matrix(elements_matrix, nrow = n)
    }

    # Calculate distances between every element, a 'triangular matrix' is returned
    # Only the first column is used because it corresponds to the distances
    # between the most recent 'element' and the rest of the 'elements'
    distances <- parDist(elements_matrix, distance_metric, threads = threads)[1:(n - d)]

    # Get the indexes of the k nearest neighbors(elements)
    k_nn <- head((sort.int(distances, index.return = TRUE))$ix, k)

    # Calculate the weights for the future computation of the weighted mean
    weights = switch(weight, proximity = {1 / (distances[k_nn] + .Machine$double.xmin * 1e150)},
                             same = {rep.int(1, k)},
                             trend = {k:1})

    # Calculate the predicted value
    prediction <- weighted.mean(y[n - k_nn + 1, v], weights)

    prediction
}
