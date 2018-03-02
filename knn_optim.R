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
  
    # Choose the appropiate index of the accuracy result, depending on the error_metric
    error_type = switch(error_metric,
                        ME = {1},
                        RMSE = {2},
                        MAE = {3},
                        MPE = {4},
                        MAPE = {5}
    )
    
    # Sort k or d vector if they are unsorted
    if (is.unsorted(k)) {
      k <- sort(k)
    }
    if (is.unsorted(d)) {
      d <- sort(d)
    }
    
    # Initialization of variables to be used
    y <- matrix(x, ncol = NCOL(x))
    n <- NROW(y)
    m <- NCOL(y)
    ks <- length(k)
    ds <- length(d)
    distances <- vector("list", ds)
    
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
      raw_distances <- parDist(elements_matrix, distance_metric, threads = 1)
      
      # Transform previous 'triangular matrix' in a regular matrix
      distances_new_element <- diag(n - i + 1)
      distances_new_element[lower.tri(distances_new_element, diag = FALSE)] <- raw_distances
      distances[[j]] <- distances_new_element
      
      j <- j + 1
    }
    
    
    init <- floor(n*0.7)
    errors <- matrix(nrow = ks, ncol = ds)
    real_values <- matrix(y[(init + 1):n, v])
    index <- 1
    for (i in d) {
        last_elem <- n - i
        preds <- matrix(nrow = ks, ncol = n - init)
        distances_element <- distances[[index]]
        for (j in (init - i + 1):last_elem) {
            
            # For k = j get the indexes of all elements ordered by distance
            dist_row <- sort.int(distances_element[j, 1:(j - 1)], index.return = TRUE)
            
            for (k_index in 1:ks) {
              k_value <- k[k_index]
              
              # Get the indexes k nearest elements
              k_nn <- head(dist_row$ix, k_value )
              
              # Calculate the weights for the future computation of the weighted mean
              weights =  switch(weight, 
                                proximity = {1/(distances_element[k_nn] + 1)},
                                same = {rep.int(1,k_value )},
                                trend = {k_value :1}
                            )
              
              # Calculate the predicted value
              preds[k_index, j - init + i] <- weighted.mean(y[k_nn + i, v], weights)
            }
        }
        
        # Calculate error values between the known values and the predicted values, these values go from init to t - 1
        # and for all Ks
        for (ind in 1:ks) {
          errors[ind, i - d[1] + 1] <- accuracy(ts(preds[ind, ]), real_values)[error_type]
        }
        
        index <- index + 1
    }
    
    # Construction of the list to be returned
    index_min_error <- which.min(errors)
    optK <- k[((index_min_error - 1) %% ks) + 1]
    optD <- d[ceiling(index_min_error / ks)]
    result <- list(errors = errors, k = optK, d = optD)
    
    result
}
