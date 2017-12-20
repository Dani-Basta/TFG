#' Creates a matrix to be used for calculating distances and errors
#'
#' @param y A matrix
#' @param d Length of the 'elements'
#' @return A matrix to be used for calculating distances and errors

knn_elements = function(y, d, v) {
    n <- NROW(y)
    m <- NCOL(y)
    
    # Fill matrix in a way in which every row has a 'element'
    elements_matrix <- matrix(nrow = n, ncol = d * m + 1)
    col <- 1
    for (i in 1:m) {
        for (j in 1:d) {
            elements_matrix[, col] <- c(y[(j:n), i], rep(NA, j - 1))
            col <- col + 1
        }
    }

    elements_matrix[1:(n - d), d * m + 1] <- y[(d + 1):n, v]
    
    elements_matrix[1:(n - 1),]
}
