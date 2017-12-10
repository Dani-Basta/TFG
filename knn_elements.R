#' Creates a matrix to be used for calculating distances and errors
#'
#' @param y A matrix
#' @param d Length of the 'elements'
#' @return A matrix to be used for calculating distances and errors

knn_elements = function(y, d, v) {
    n <- NROW(y)
    m <- NCOL(y)
    last_elem <- n - d
    
    # Fill matrix in a way in which every row has a 'element'
    elements_matrix <- matrix(nrow = last_elem + 1, ncol = d * m + m)
    col <- 1
    for (i in 1:m) {
        for (j in 1:d) {
            elements_matrix[, col] <- y[(j:(j + last_elem)), i]
            col <- col + 1
        }
    }
    
    
    # INVALIDATED since the next value depends of the used d
        # Fill the last column of the matrix assigning to each element the instant value 
        # next to most recent instant value of the 'element'
        #for (i in 1:m) {
        #    elements_matrix[1:last_elem, d * m + i] <- y[(d + 1):n, i]
        #}
    
    
    elements_matrix
}
