knn_distances = function(x, d, distance_metric="euclidean", threads = 3, file){
    require(parallelDist)
    
    # Calculate all d values to be explored. If a number is given, it creates a vector from 1 to k.
    # Otherwise it will just make sure that the vector is ordered
    if (length(d) == 1) {
        d <- 1:d
    } else if (is.unsorted(d)) {
        d <- sort(d)
    }
    
    # Initialization of variables to be used
    y <- matrix(x, ncol = NCOL(x))
    n <- NROW(y)
    
    # In order to paralelise we calculate the distances matrix just once for each d, as the distance variates 
    # with the number of values that characterise each element.
    
    #Calculate all distances matrixes
    j <- 1
    for (i in d) {
        # Get elements matrix
        elements_matrix <- knn_elements(y, i)
        
        # Calculate distances between the last 'element' and each of the others 'elements'
        # This happens if d=1 and a univariate time series is given, a very unusual case
        if (is(elements_matrix, "numeric")) {
            elements_matrix <- matrix(elements_matrix, nrow = length(curr_elems))
        }
        raw_distances <- parDist(elements_matrix, distance_metric, threads = threads)
        
        # Transform previous 'triangular matrix' in a regular matrix
        distances_new_element <- diag(n - i + 1)
        distances_new_element[lower.tri(distances_new_element, diag = FALSE)] <- raw_distances
        
        # Save distances matrix in file
        saveRDS(distances_new_element, paste0(file, i))
        
        j <- j + 1
    }
    
}