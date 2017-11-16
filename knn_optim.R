#' Optimizes the values of K and D for a given time series
#' 
#' @param x A time series
#' @param k Values of Ks to be analyzed
#' @param kmin Minimum value of K to be analyzed
#' @param kmax Maximum value of K to be analyzed
#' @param d Values of Ds to be analyzed
#' @param dmin Minimum value of D to be analyzed
#' @param dmax Maximum value of K to be analyzed
#' @param v Variable to be predicted if given multivariate time series
#' @param metric Type of metric to evaluate the distance between points
#' @param weight Type of weight to use at the time of the prediction. 3 supported: proximity, same, trend
#' @return A matrix of errors, optimal K & D

knn_optim = function(x, k, kmin, kmax, d, dmin, dmax, v=1, metric="euclidean", weight="proximity"){
    require(rdist)
    
    y <- matrix(x, ncol = NCOL(x))
    n <- NROW(y)
    m <- NCOL(y)
    
    # Calculate all the k and d values to be explored, this depends on the user but default values are also given
    # in case of missing arguments
    if (missing(k)) {
        if (missing(kmin) || missing(kmax)) {
            k <- 3:20
        } else {
            k <- kmin:kmax
        }
    }
    if (missing(d)) {
        if (missing(dmin) || missing(dmax)) {
            d <- 3:20
        } else {
            d <- dmin:dmax
        }
    }
    
    ks <- length(k)
    init <- floor(n*0.7)
    errors <- matrix(nrow = ks, ncol = length(d))
    
    for (i in d) {
        roof <- n - i
        
        # Get 'neighbourhoods' matrix
        neighs <- knn_neighs(y, i)
        
        # Calculate distances between every 'neighbourhood', a 'triangular matrix' is returned
        raw_distances <- rdist(neighs[, 1:(i * m)])
        distances <- diag(n - i + 1)
        
        # Transform previous 'triangular matrix' in a regular matrix
        distances[lower.tri(distances, diag = FALSE)] <- raw_distances
        
        preds <- matrix(nrow = ks, ncol = roof - init + 1)
        
        for (j in init:roof) {
            
            # For k = j get the indexes of all neighbors ordered by distance
            dist_row <- sort.int(distances[j, 1:(j - 1)], index.return = TRUE)
            
            for (h in k) {
              # Get the indexes h nearest neighbors
              k_nn <- head(dist_row$ix, h)
              
              # Calculate the weights for the future computation of the weighted mean 
              # ------------Falta tratamiento correcto del valor delta------------------
              weights =  switch(weight, proximity = {1/(distances[k_nn] + 0.0001)},
                                        same = {rep.int(1,h)},
                                        trend = {h:1})
              
              # Calculate the predicted value
              preds[h - k[1] + 1, j - init + 1] <- weighted.mean(neighs[k_nn, m * i + v], weights)
            }
        }
        
        # Calculate error values between the known values and the predicted values, these values go from init to t - 1
        # and for all Ks
        errors[, i - d[1] + 1] <- cdist(preds, matrix(neighs[init:(n - i), m * i + v], nrow = 1), metric)
    }
    
    # Construction of the list to be returned
    minErr <- which.min(errors)
    optK <- (minErr %% ks) + 1
    optD <- ceiling(minErr / ks)
    result <- list(errors = errors, k = optK, d = optD)
    
    result
}