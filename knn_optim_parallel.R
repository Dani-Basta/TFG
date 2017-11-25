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
    
    # Calculate all the k and d values to be explored, this depends on the user but default values are also given
    # in case of missing arguments
    if (missing(k)) {
        k <- 3:20
    }
    if (missing(d)) {
        d <- 3:20
    }
    
    ks <- length(k)
    ds <- length(d)
    distances <- vector("list", ds)
    
    #Calculate all distances matrixes
    j <- 1
    for (i in d) {
        # Get 'neighbourhoods' matrix
        neighs <- knn_neighs(y, i)
        
        # Calculate distances between the last 'neighbor' and each of the others 'neighbors'
        elements <- neighs[, 1:(i * m)]
        # This happens if d=1 and a univariate time series is given, a very unusual case
        if (is(elements, "numeric")) {
            elements <- matrix(elements, nrow = length(elements))
        }
        raw_distances <- parDist(elements, distance_metric)
        
        # Transform previous 'triangular matrix' in a regular matrix
        distances_new_element <- diag(n - i + 1)
        distances_new_element[lower.tri(distances_new_element, diag = FALSE)] <- raw_distances
        distances[[j]] <- distances_new_element
        
        j <- j + 1
    }

    # In case the d vector is not in order we find the maximum posible d
    neighs <- knn_neighs(y, max(d))
    
    # Choose the appropiate index of the accuracy result, depending on the error_metric
    error_type = switch(error_metric,
                        ME = {1},
                        RMSE = {2},
                        MAE = {3},
                        MPE = {4},
                        MAPE = {5}
    )

    ##
    ## Bucles sin paralelizar
    ##
    
    init <- floor(n*0.7)
    errors <- matrix(nrow = ks, ncol = length(d))
    index <- 1
    for (i in d) {
        roof <- n - i
        preds <- matrix(nrow = ks, ncol = roof - init + 1)
        distances_element <- distances[[index]]
        
        for (j in init:roof) {
            
            # For k = j get the indexes of all neighbors ordered by distance
            dist_row <- sort.int(distances_element[j, 1:(j - 1)], index.return = TRUE)
            
            for (h in k) {
                # Get the indexes h nearest neighbors
                k_nn <- head(dist_row$ix, h)
                
                # Calculate the weights for the future computation of the weighted mean
                weights = switch(weight, 
                                 proximity = {1/(distances_element[k_nn] + .Machine$double.eps ^ 0.5)},
                                 same = {rep.int(1,h)},
                                 trend = {h:1}
                )
                
                # Calculate the predicted value
                preds[h - k[1] + 1, j - init + 1] <- weighted.mean(neighs[k_nn, ???? ], weights)
                
            }
        }
        
        # Calculate error values between the known values and the predicted values, these values go from init to t - 1
        # and for all Ks
        errors[, i - d[1] + 1] <- cdist(preds, matrix(neighs[init:roof, ???? ], nrow = 1), error_metric)
        
        index <- index + 1
    }

    ##
    ## Bucle paralelizado
    ##

    ## TODO: eliminar las combinaciones the fila y d que no son viables (falta de futuro)
    indexes = 0:((n-init+1)*ds -1)

    errors <- foreach(i = indexes, )  %dopar% {
    	# Obtain the actual row
    	act_row = floor((i)/ds) + init

    	# Obtain the index of the actual d and it's value
    	d_index = (i %% ds) + 1
    	act_d = d[d_index]

    	# Obtain the distances matrix for the actual d
    	distances_element <- distances[[d_index]]

    	# For k = act_row get the indexes of all neighbors ordered by distance
    	dist_row <- sort.int(distances_element[act_row, 1:(act_row - 1)], index.return = TRUE)

    	preds <- vector(ks)

    	for (k_index in 1:ks) {
    		h <- k[k_index]
    		# Get the indexes h nearest neighbors
    		k_nn <- head(dist_row$ix, h)
              
            # Calculate the weights for the future computation of the weighted mean
            weights =  switch(weight, 
            					proximity = {1/(distances_element[k_nn] + .Machine$double.eps ^ 0.5)},
            					same = {rep.int(1,h)},
            					trend = {h:1}
            				)
            # Calculate the predicted value 
            ## TODO: falta acceder a los valores de los vecinos cercanos
            preds[k_index] <- weighted.mean(neighs[k_nn, ????? ], weights)
    	}

    	# Once you have the predictions for all the Ks, calculate the error
    	## TODO: falta acceder a los valores de los vecinos cercanos
    	error <- accuracy(f= preds, x= rep(neighs[act_row, ????? ], ks) )[error_type]
    	error
    }
    
    
    # Construction of the list to be returned
    minErr <- which.min(errors)
    optK <- k[(minErr %% ks) + 1]
    optD <- d[ceiling(minErr / ks)]
    result <- list(errors = errors, k = optK, d = optD)
    
    result
}
