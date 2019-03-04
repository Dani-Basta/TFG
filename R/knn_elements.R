#' 'Elements' matrix computation
#'
#' Creates a matrix to be used for calculating distances. The most
#' recent 'element' is put in the first row of the matrix, the
#' second most recent 'element' in the second row and so on. Therefore,
#' the oldest 'element' is put in the last row.
#'
#' @param y A matrix.
#' @param d Length of each of the 'elements'.
#' @return A matrix to be used for calculating distances.
knn_elements <- function(y, d) {
  n <- NROW(y)
  m <- NCOL(y)
  last_elem <- n - d

  # Fill matrix as described above, it is done vertically for efficiency reasons
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
