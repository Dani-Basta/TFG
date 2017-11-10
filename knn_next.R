#' Predicts next value of the time series using k-nearest neighbors algorithm
#'
#' @param x A time series
#' @param k Number of neighbors
#' @param d Length of the 'neighbourhoods'
#' @param v Variable to be predicted if given multivariate time series
#' @param metric Type of metric to evaluate the distance between points
#' @param weight Type of weight to use at the time of the prediction. 3 supported: proximity, same, trend 
#' @return The predicted value

knn_next = function(x, k, d, v = 1, metric = "euclidean", weight = "proximity") {
    require(rdist)
    y <- matrix(x, ncol = NCOL(x))
    n <- NROW(y)
    m <- NCOL(y)
    roof <- n - d
    
    # Get 'neighbourhoods' matrix
    neighs <- knn_neighs(y, d)
    
    # Calculate distances between the last 'neighbourhood' and each of rest 'neighbourhoods'
    distances <- cdist(neighs[1:roof, 1:(d * m)], matrix(neighs[roof + 1, 1:(d * m)], nrow = 1))
    
    # Get the indexes of the k nearest neighbors
    k_nn <- head((sort.int(distances, index.return = TRUE))$ix, k)
    
    # Calculate the weights for the future computation of the weighted mean 
    # ------------Falta tratamiento correcto del valor delta------------------
    weights = switch(weight, proximity = {1/(distances[k_nn] + 0.00001)}, 
                             same = {rep.int(1, k)}, 
                             trend = {k:1})
  
    # Calculate the predicted value
    prediction <- weighted.mean(neighs[k_nn, m * d + v], weights)
    
    prediction
}
