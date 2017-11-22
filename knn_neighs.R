#' Creates a matrix to be used for calculating distances and errors
#'
#' @param y A matrix
#' @param d Length of the 'neighbourhood'
#' @return A matrix to be used for calculating distances and errors

knn_neighs = function(y, d) {
    n <- NROW(y)
    m <- NCOL(y)
    roof <- n - d
    
    # Fill matrix in a way in which every row has a 'neighbor'
    neighs <- matrix(nrow = roof + 1, ncol = d * m + m)
    col <- 1
    for (i in 1:m) {
        for (j in 1:d) {
            neighs[, col] <- y[(j:(j + roof)), i]
            col <- col + 1
        }
    }
    
    
    # INVALIDATED since the next value depends of the used d
        # Fill the last column of the matrix assigning to each neighbor the instant value 
        # next to most recent instant value of the 'neighbor'
        #for (i in 1:m) {
        #    neighs[1:roof, d * m + i] <- y[(d + 1):n, i]
        #}
    
    
    neighs
}
