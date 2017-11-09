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
    require(knn_past)
    require(rdist)
    
    y <- matrix(x, ncol = NCOL(x))
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
    
    ks <- NCOL(k)
    
    #####
    init <- floor(n*0.7)
                  
    errors <- matrix(nrow = ks, ncol = NCOL(d))
    pred <- matrix(nrow = ks, ncol = n - init)
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
    
    #####
    
    init <- floor(n*0.7)
    
    for (i in d) {
        roof <- n - i
        
        neighs <- knn_neighs(y, i)
        prediction <- array(dim = n - init)
        raw_distances <- rdist(neighs[, 1:(i * m)])
        distances <- diag(n - 1)
        distances[lower.tri(distances, diag = FALSE)] <- raw_distances
        
        preds <- matrix(nrow = ks, ncol = roof)
        
        for (j in init:roof) {
            #distances_rowj <- distances[j, 1:(j - 1)]
            dist_row <- sort.int(distances[j, 1:(j - 1)], index.return = TRUE)
            for (h in k) {
                #k_nn <- head((sort.int(distances_rowj, index.return = TRUE))$ix, h)
                k_nn <- head(dist_row$ix, h)
                weights =  switch(weight,
                                  proximity = {
                                      # Falta mirar casos de 1/0
                                      1/distances[k_nn]
                                  },
                                  same = {
                                      rep.int(1,h)
                                  },
                                  trend = {
                                      h:1
                                  }
                )
                preds[h,j] <- weighted.mean(neighs[k_nn, h*i + v], weights)
            }
        }
        # Ya tenemos todas las predicciones para todas las Ks de las filas desde init a t-1
        errors[,i] <- cdist(preds, neighs[ init:(n - 1) , i*m + v], metric) 
    }
    
    minErr <- which.min(errors)
    optK <- (minErr %% ks) + 1
    optD <- ceiling( minErr / ks )
    result <- list(errors = errors, k = optK, d = optD)
    result
}