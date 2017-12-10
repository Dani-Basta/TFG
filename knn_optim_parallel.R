#' Optimizes the values of K and D for a given time series
#' 
#' @param x A time series
#' @param k Values of Ks to be analyzed
#' @param d Values of Ds to be analyzed
#' @param v Variable to be predicted if given multivariate time series
#' @param distance_metric Type of metric to evaluate the distance between points
#' @param error_metric Type of metric to evaluate the prediction error
#' @param weight Type of weight to use at the time of the prediction. 3 supported: proximity, same, trend
#' @return A matrix of errors, optimal K & D

knn_optim = function(x, k, d, v=1, distance_metric="euclidean", error_metric="MAE", weight="proximity"){
    require(parallelDist)
    require(forecast)
    require(foreach)
    
    y <- matrix(x, ncol = NCOL(x))
    n <- NROW(y)
    m <- NCOL(y)
    
    # Calculate all the k and d values to be explored. If a number is given, it creates a vector from 1 to k.
    # Otherwise it will just make sure that the vector is ordered
    if (length(k) == 1) {
        k <- 1:k
    } else if (is.unsorted(k)) {
        k <- sort(k)
    }
    if (length(d) == 1) {
        d <- 1:d
    } else if (is.unsorted(d)) {
        k <- sort(d)
    }
    
    ks <- length(k)
    ds <- length(d)
    distances <- vector("list", ds)
    
    # In order to paralelise we calculate the distances matrix just once for each d, as the distance variates 
    # with the number of values that characterise each element.
    
    #Calculate all distances matrixes
    j <- 1
    for (i in d) {
        # Get elements matrix
        elements_matrix <- knn_elements(y, i)
        
        # Calculate distances between the last 'element' and each of the others 'elements'
        curr_elems <- elements_matrix[, 1:(i * m)]
        # This happens if d=1 and a univariate time series is given, a very unusual case
        if (is(curr_elems, "numeric")) {
            curr_elems <- matrix(curr_elems, nrow = length(curr_elems))
        }
        raw_distances <- parDist(curr_elems, distance_metric)
        
        # Transform previous 'triangular matrix' in a regular matrix
        distances_new_element <- diag(n - i + 1)
        distances_new_element[lower.tri(distances_new_element, diag = FALSE)] <- raw_distances
        distances[[j]] <- distances_new_element
        
        j <- j + 1
    }

    # In case the d vector is not in order we find the maximum posible d
    elements_matrix <- knn_elements(y, max(d))
    
    # Choose the appropiate index of the accuracy result, depending on the error_metric
    error_type = switch(error_metric,
                        ME = {1},
                        RMSE = {2},
                        MAE = {3},
                        MPE = {4},
                        MAPE = {5}
    )
    
    init <- floor(n*0.7)
    
    # Once we have all distance matrixes we proceed to evaluate in parallel with a different combination
    # of d and the last row to evaluate.
    # For each of the combinations we order all the neighbors(elements) by proximity and evaluate with 
    # all the posible values for k, taking each time the k-Nearest ones, to make k predictions.
    # Finally when we have all the predictions we calculate the error for each prediction and store them
    # in the variable of the foreach loop.

    ## TODO: eliminar las combinaciones the fila y d que no son viables (falta de futuro)
    indexes = 0:((n-init+1)*ds -1)

    errors <- foreach(i = indexes, )  %dopar% {
    	# Obtain the actual row
    	act_row <- floor((i)/ds) + init

    	# Obtain the index of the actual d and it's value
    	d_index <- (i %% ds) + 1
    	act_d <- d[d_index]

    	# Obtain the distances matrix for the actual d
    	distances_element <- distances[[d_index]]

    	# For k = act_row get the indexes of all neighbors(elements) ordered by distance
    	dist_row <- sort.int(distances_element[act_row, 1:(act_row - 1)], index.return = TRUE)

    	preds <- vector(ks)

    	for (k_index in 1:ks) {
    		h <- k[k_index]
    		# Get the indexes h nearest neighbors(elements)
    		k_nn <- head(dist_row$ix, h)
              
            # Calculate the weights for the future computation of the weighted mean
            weights <- switch(weight, 
            					proximity = {1/(distances_element[k_nn] + .Machine$double.xmin)},
            					same = {rep.int(1,h)},
            					trend = {h:1}
            				)
            # Calculate the predicted value 
            ## TODO: falta acceder a los valores de los vecinos cercanos
            preds[k_index] <- weighted.mean(elements_matrix[k_nn, ????? ], weights)
    	}

    	# Once you have the predictions for all the Ks, calculate the error
    	## TODO: falta acceder a los valores de los vecinos cercanos
    	error <- accuracy(f= preds, x= rep(elements_matrix[act_row, ????? ], ks) )[error_type]
    	error
    }
    
    
    # Construction of the list to be returned
    minErr <- which.min(errors)
    optK <- k[(minErr %% ks) + 1]
    optD <- d[ceiling(minErr / ks)]
    result <- list(errors = errors, k = optK, d = optD)
    
    result
}
