#' Optimizes the values of K and D for a given time series
#' 
#' @param x A time series
#' @param k Values of Ks to be analyzed
#' @param d Values of Ds to be analyzed
#' @param v Variable to be predicted if given multivariate time series
#' @param metric Type of metric to evaluate the distance between points
#' @param weight Type of weight to use at the time of the prediction. 3 supported: proximity, same, trend
#' @return A matrix of errors, optimal K & D

knn_optim = function(x, k, kmin, kmax, d, dmin, dmax, v=1){
    require(knn_past)
    require(rdist)
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
    
    init <- floor(n*0.7)
                  
    errors <- matrix(nrow = k, ncol = d)
    pred <- matrix(nrow = tail(k, 1), ncol = n - init)
    for (i in d) {
        r <- 1
        for (j in k) {
            pred[r,] <- knn_past(x, j, i, init, v)
            r <- r + 1
        }
        errors[,i] <- cdist(pred, x[ (init + 1:n) , v]) 
    }
    minErr <- which.min(errors)
    optK <- (minErr %% k) + 1
    optD <- ceiling( minErr / k )
    result <- list(errors = errors, k = optK, d = optD)
    result
}
