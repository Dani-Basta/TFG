#' Predict next value of the time series using k-nearest neighbors algorithm
#'
#' @param x A time series
#' @param k Number of neighbors
#' @param d Length of the "neighbourhoods"
#' @param v Variable to be predicted if given multivariate time series
#' @return The predicted value

knn_past = function(x, k, d, init, v=1, metric="euclidean"){
  require(rdist)
  y <- matrix(x, ncol = NCOL(x))
  n <- NROW(y)
  m <- NCOL(y)
  roof <- n - d
  
  # Obtenemos la matriz de vecindarios
  neighs <- knn_neighs(y, d)
  
  # Reservamos array para los valores a predecir
  prediction <- array(dim = n - init)
  
  # Calculamos las distancias (todos contra todos)
  raw_distances <- rdist(neighs[, 1:(d * m)])
  
  # Transformamos la 'matriz triangular' a una matriz normal triangular inferior
  distances <- diag(n - 1)
  distances[lower.tri(distances, diag=TRUE)] <- raw_distances
  
  for (j in init:roof) {
    
    # Obtenemos la fila de distancias que nos interesa para predicir el instante j+1
    distances_rowj <- distances[j, 1:j]
    
    # Buscamos las k distancias mas pequenas y luego los indices correspondientes
    k_nn <- head(which( distances_rowj %in% head(sort.int(distances_rowj, k)), k))
    
    prediction[j - init + 1] <- mean(y[(k_nn + d), v])
  }
  
  ts(prediction)
}