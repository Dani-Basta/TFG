#' Create a matrix to be used for calculating distances
#'
#' @param y A matrix
#' @param d Length of each of the elements
#' @return A matrix to be used for calculating distances

knn_elements2 = function(y, d) {
  n <- NROW(y)
  m <- NCOL(y)
  last_elem <- n - d
  
  # Fill matrix in a way in which every row has an 'element'
  elements_matrix <- matrix(nrow = last_elem + 1, ncol = d * m)
  col <- 1
  for (i in 1:m) {
    for (j in 1:d) {
      elements_matrix[, col] <- rev(y[(j:(j + last_elem)), i])
      col <- col + 1
    }
  }
  
  elements_matrix
}
