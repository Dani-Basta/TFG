#' Predict next value of the time series using k-nearest neighbors algorithm
#'
#' @param x A time series
#' @param k Number of neighbors
#' @param d Length of the "neighbourhoods"
#' @param v Variable to be predicted if given multivariate time series
#' @param metric Type of metric to evaluate the distance between points
#' @return The predicted value

knn_next = function(x, k, d, v=1, metric="euclidean"){
  require(rdist)
  y <- matrix(x, ncol = NCOL(x))
  n <- NROW(y)
  m <- NCOL(y)
  roof <- n - d
  
  # Obtenemos la matriz de vecindarios
  neighs <- knn_neighs(y, d)

  # Calculamos las distancias omitiendo las columnas que tiene los valores reales
  distances <- cdist(neighs[1:roof, 1:(d * m)], matrix(neighs[roof + 1, 1:(d * m)], nrow = 1))
  
  # Buscamos las k distancias mas pequenas y luego los indices correspondientes
  k_nn <- head( which( distances %in% head(sort.int(distances), k) ), k )
  
  prediction <- weighted.mean(y[(k_nn + d), v], k:1)
  
  ts(prediction)
}