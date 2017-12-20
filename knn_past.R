#' Predicts values of the time series using k-nearest neighbors algorithm. The first predicted value 
#' corresponds to instant init + 1 and its determined by the first instants until init, the same is
#' done for instant init + 2 and so on until the last instant of the time series.
#'
#' @param x A time series
#' @param k Number of neighbors
#' @param d Length of each group of elements
#' @param init A value that has to satisfy d <= init < n, values are predicted from this point of the time series to the end
#' @param v Variable to be predicted if given multivariate time series
#' @param metric Type of metric to evaluate the distance between points. Many metrics are supported: euclidean, manhattan, 
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
#' @return The predicted value

knn_past = function(x, k, d, init, v = 1, metric = "euclidean", weight = "proximity") {
    require(parallelDist)
    require(rdist)
    y <- matrix(x, ncol = NCOL(x))
    n <- NROW(y)
    m <- NCOL(y)
    last_elem <- n - d
    predictions <- array(dim = n - init)
    
    # Get elements matrix
    elements_matrix <- knn_elements(y, d, v)
    curr_elems <- elements_matrix[1:last_elem, 1:(d * m)]
    
    # This happens if d=1 and a univariate time series is given, a very unusual case
    if (is(curr_elems, "numeric")) {
      curr_elems <- matrix(curr_elems, nrow = length(curr_elems))
    }
    
    # Calculate distances between every element, a 'triangular matrix' is returned
    raw_distances <- parDist(curr_elems, metric)
    
    # Transform previous 'triangular matrix' in a regular matrix
    distances <- diag(n - d)
    distances[lower.tri(distances, diag = FALSE)] <- raw_distances
    
    prediction_index <- 1
    for (j in (init - d + 1):last_elem) {
        
        # Get row needed from the distances matrix in order to predict instant j+1 asumming that all we
        # know about the time series is instants 1 to j
        distances_rowj <- distances[j, 1:(j - 1)]
        
        # Get the indexes of the k nearest neighbors(elements)
        k_nn <- head((sort.int(distances_rowj, index.return = TRUE))$ix, k)
        
        # Calculate the weights for the future computation of the weighted mean 
        weights = switch(weight, proximity = {1/(distances[k_nn] + .Machine$double.xmin)}, 
                         same = {rep.int(1, k)}, 
                         trend = {k:1})
        
        # Calculate the predicted value
        predictions[prediction_index] <- weighted.mean(elements_matrix[k_nn, m * d + 1], weights)
        prediction_index <- prediction_index + 1
    }
    
    predictions
}
