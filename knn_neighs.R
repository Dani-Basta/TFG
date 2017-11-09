#' Create a matrix to be used for calculating distances and errors
#'
#' @param y A matrix
#' @param d Length of the "neighbourhoods"
#' @return A matrix to be used for calculating distances and errors

knn_neighs = function(y, d) {
  n <- NROW(y)
  m <- NCOL(y)
  roof <- n - d

  # Pre-reservando la matriz
  neighs <- matrix(nrow = roof + 1, ncol = d * m + m)
  col <- 1
  for (i in 1:m) {
    for (j in 1:d) {
      neighs[, col] <- y[(j:(j + roof)), i]
      col <- col + 1
    }
  }
  
  # Rellenado de las columnas de valores reales
  for (i in 1:m) {
    neighs[1:roof, d * m + i] <- y[(d + 1):n, i]
  }
  
  neighs
}
