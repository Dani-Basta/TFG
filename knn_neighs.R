#' Create a matrix to be used for calculating distances and errors
#'
#' @param y A time series
#' @param d Length of the "neighbourhoods"
#' @return A matrix to be used for calculating distances and errors

knn_neighs = function(y, d) {
  n <- NROW(y)
  m <- NCOL(y)
  roof <- n - d
  
  if (any(class(y) == "mts")) {
    # Pre-reservando la matriz
    neighs <- matrix(nrow = roof + 1, ncol = d * m + m)
    k <- 1
    for (i in 1:m) {
      for (j in 1:d) {
        neighs[, k] <- y[(j:(j + roof)), i]
        k <- k + 1
      }
    }
    # Rellenado de las columnas de valores reales
    for (i in 1:m) {
      neighs[1:roof, d * m + i] <- y[(d + 1):n, i]
    }
  } else {
    # Pre-reservando la matriz
    neighs = matrix(nrow = roof + 1, ncol = d + 1)
    for (i in 1:d) {
      neighs[,i] <- y[ i:(i + roof)]
    }
    # Rellenado de la columna de valor real
    neighs[1:roof, d + 1] <- y[(d + 1):n]
  }
  
  neighs
}