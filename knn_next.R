#' Predicts next value of the time series using k-nearest neighbors algorithm
#'
#' @param x A time series
#' @param k Number of neighbors
#' @param d Length of the 'elements'
#' @param v Variable to be predicted if given multivariate time series
#' @param metric Type of metric to evaluate the distance between points
#' @param weight Type of weight to use at the time of the prediction. 3 supported: proximity, same, trend 
#' @return The predicted value

knn_next = function(x, k, d, v = 1, metric = "euclidean", weight = "proximity") {
    require(rdist)
    y <- matrix(x, ncol = NCOL(x))
    n <- NROW(y)
    m <- NCOL(y)
    last_elem <- n - d
    
    # Get 'elements' matrix
    elements_matrix <- knn_elements(y, d, v)
    
    # Calculate distances between the last 'element' and each of rest 'elements'
    distances <- cdist(elements_matrix[1:last_elem, 1:(d * m)], matrix(elements_matrix[last_elem + 1, 1:(d * m)], nrow = 1))
    
    # Get the indexes of the k nearest neighbors(elements)
    k_nn <- head((sort.int(distances, index.return = TRUE))$ix, k)
    
    # Calculate the weights for the future computation of the weighted mean
    weights = switch(weight, proximity = {1/(distances[k_nn] + .Machine$double.xmin)}, 
                             same = {rep.int(1, k)}, 
                             trend = {k:1})
  
    # Calculate the predicted value
    prediction <- weighted.mean(elements_matrix[k_nn, m * d + 1], weights)
    
    prediction
}
