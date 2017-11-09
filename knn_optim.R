#' Optimizes the values of K and D for a given Time Serie
#' 
#' @param x Time serie to analyze
#' @param k
#' @param kmin
#' @param kmax
#' @param d
#' @param dmin
#' @param dmax
#' @param v
#' @return A matrix of errors, optimal K & D

knn_optim = function(x, k, kmin, kmax, d, dmin, dmax, v=1){
    require(knn_past)
    require(rdist)
    
    n <- NROW(y)
    m <- NCOL(y)
    
    if (missing(k)) {
        if (missing(kmin) || missing(kmax)) {
            k = 3:20
        } else {
            k = kmin:kmax
        }
    }
    if (missing(d)) {
        if (missing(dmin) || missing(dmax)) {
            d = 3:20
        } else {
            d = dmin:dmax
        }
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