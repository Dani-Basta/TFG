#' Optimizes the values of K and D for a given time series
#' 
#' @param x A time series
#' @param k Values of Ks to be analyzed
#' @param d Values of Ds to be analyzed
#' @param v Variable to be predicted if given multivariate time series
#' @param distance_metric Type of metric to evaluate the distance between points
#' @param error_metric Type of metric to evaluate the prediction error
#' @param weight Type of weight to use at the time of the prediction. 3 supported: proximity, same, trend
#' @return A matrix of errors, optimal K & D

knn_optim = function(x, k, d, v=1, distance_metric="euclidean", error_metric="MAE", weight="proximity"){
    require(parallelDist)
    
    y <- matrix(x, ncol = NCOL(x))
    n <- NROW(y)
    m <- NCOL(y)
    
    # Calculate all the k and d values to be explored, this depends on the user but default values are also given
    # in case of missing arguments
    if (missing(k)) {
        k <- 3:20
    }
    if (missing(d)) {
        d <- 3:20
    }
    
    ks <- length(k)
    ds <- length(d)
    distances <- vector("list", ds)
    
    #Calculate all distances matrixes
    j <- 1
    for (i in d) {
      # Get 'neighbourhoods' matrix
      neighs <- knn_neighs(y, i)
      
       # Calculate distances between the last 'neighbor' and each of the others 'neighbors'
      elements <- neighs[, 1:(i * m)]
      # This happens if d=1 and a univariate time series is given, a very unusual case
      if (is(elements, "numeric")) {
        elements <- matrix(elements, nrow = length(elements))
      }
      raw_distances <- parDist(elements, distance_metric)
      
      # Transform previous 'triangular matrix' in a regular matrix
      distances_new_element <- diag(n - i + 1)
      distances_new_element[lower.tri(distances_new_element, diag = FALSE)] <- raw_distances
      distances[[j]] <- distances_new_element
      
      j <- j + 1
    }
    
    
    init <- floor(n*0.7)
    errors <- matrix(nrow = ks, ncol = length(d))
    index <- 1
    for (i in d) {
        roof <- n - i
        preds <- matrix(nrow = ks, ncol = roof - init + 1)
        distances_element <- distances[[index]]
        
        for (j in init:roof) {
            
            # For k = j get the indexes of all neighbors ordered by distance
            dist_row <- sort.int(distances_element[j, 1:(j - 1)], index.return = TRUE)
            
            for (h in k) {
              # Get the indexes h nearest neighbors
              k_nn <- head(dist_row$ix, h)
              
              # Calculate the weights for the future computation of the weighted mean
              weights =  switch(weight, proximity = {1/(distances_element[k_nn] + .Machine$double.eps ^ 0.5)},
                                        same = {rep.int(1,h)},
                                        trend = {h:1})
              
              # Calculate the predicted value
              preds[h - k[1] + 1, j - init + 1] <- weighted.mean(neighs[k_nn, m * i + v], weights)
            }
        }
        
        # Calculate error values between the known values and the predicted values, these values go from init to t - 1
        # and for all Ks
        errors[, i - d[1] + 1] <- cdist(preds, matrix(neighs[init:(n - i), m * i + v], nrow = 1), error_metric)
        
        index <- index + 1
    }
    
    
    
    # Construction of the list to be returned
    minErr <- which.min(errors)
    optK <- (minErr %% ks) + 1
    optD <- ceiling(minErr / ks)
    result <- list(errors = errors, k = optK, d = optD)
    
    result
}
