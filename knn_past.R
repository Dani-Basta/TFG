#' Predict next value of the time series using k-nearest neighbors algorithm
#'
#' @param y A time series
#' @param k Number of neighbors
#' @param d Length of the "neighbourhoods"
#' @param v Variable to be predicted if given multivariate time series
#' @return The predicted value

knn_past = function(y, k, d, init, v=1, metric="euclidean"){
  require(rdist)
  n <- NROW(y)
  m <- NCOL(y)
  roof <- n - d
  
  neighs <- knn_neighs(y, d)
  
  prediction <- array(dim = n - init)
  
  distances <- rdist(neighs[, 1:(d * m)], ...)
  
  for (j in init:roof) {
    
    # Generamos los indices que nos interesan para acceder a la fila correspondiente de la matriz triangular inferior de distancias
    sums <- 0
    for (i in 2:j) {
      sums[i] <- sums[i - 1] - (i - 1)
    }
    
    indexes <- sums + (0:(j - 1)) * roof + j
    
    dists <- distances[indexes]
    
    # Buscamos las k distancias mas pequenas y luego los indices correspondientes
    k_nn <- head(which( dists %in% head(sort.int(dists, k)), k))
    #print(k_nn)
    if (any(class(y) == "mts")) {
      prediction[j - init] <- mean(y[(k_nn + d), v])
    } else {
      prediction[j - init] <- mean(y[(k_nn + d)])
    }
  }
  
  ts(prediction)
  
}