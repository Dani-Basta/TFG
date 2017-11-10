#' Predicts values of the time series using k-nearest neighbors algorithm
#'
#' @param x A time series
#' @param k Number of neighbors
#' @param d Length of the 'neighbourhoods'
#' @param init A value that has to satisfy d <= init < n, values are predicted from this point of the time series to the end
#' @param v Variable to be predicted if given multivariate time series
#' @param metric Type of metric to evaluate the distance between points
#' @param weight Type of weight to use at the time of the prediction. 3 supported: proximity, same, trend 
#' @return The predicted value

knn_past = function(x, k, d, init, v = 1, metric = "euclidean", weight = "proximity") {
    require(rdist)
    y <- matrix(x, ncol = NCOL(x))
    n <- NROW(y)
    m <- NCOL(y)
    roof <- n - d
    prediction <- array(dim = n - init - d + 1)
    
    # Get 'neighbourhoods' matrix
    neighs <- knn_neighs(y, d)

    # Calculate distances between every 'neighbourhood', a 'triangular matrix' is returned
    raw_distances <- rdist(neighs[, 1:(d * m)])
    
    # Transform previous 'triangular matrix' in a regular matrix
    distances <- diag(n - d + 1)
    distances[lower.tri(distances, diag = FALSE)] <- raw_distances
    
    print(neighs)

    for (j in init:roof) {
        
        # Get row needed from the distances matrix in order to predict instant j+1 asumming that all we
        # know about the time series is instants 1 to j
        distances_rowj <- distances[j, 1:(j - 1)]
        
        # Get the indexes of the k nearest neighbors
        k_nn <- head((sort.int(distances_rowj, index.return = TRUE))$ix, k)
        
        # Calculate the weights for the future computation of the weighted mean 
        # ------------Falta tratamiento correcto del valor delta------------------
        weights = switch(weight, proximity = {1/(distances[k_nn] + 0.00001)}, 
                         same = {rep.int(1, k)}, 
                         trend = {k:1})
        
        # Calculate the predicted value
        prediction[j - init + 1] <- weighted.mean(neighs[k_nn, m * d + v], weights)
    }
    
    ts(prediction)
}
