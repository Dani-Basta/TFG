#' Optimizes the values of K and D for a given Time Serie
#' 
#' @param x Time serie to analyze
#' @param k
#' @param kmin
#' @param kmax
#' @param d
#' @param dmin
#' @param dmax
#' @param metric
#' @param v
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
    
    ##
    ds <- length(d)
    ks <- length(k)
    init <- floor(n*0.7)
    errors <- matrix(nrow = ks, ncol = ds)
    
    for (i in d) {
        roof <- n - i
        
        neighs <- knn_neighs(y, i)
        prediction <- array(dim = n - init - i + 1)
        
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
        
        # Ya tenemos todas las predicciones para todas las Ks de las filas desde init a t-1
        errors[, i - d[1] + 1] <- cdist(preds, matrix(neighs[init:(n - i), m * i + v], nrow = 1), metric)
    }
    
    # Construction of the list to be returned
    minErr <- which.min(errors)
    optK <- (minErr %% ks)
    optD <- ceiling(minErr / ks)
    result <- list(errors = errors, k = optK, d = optD)
    
    result
}