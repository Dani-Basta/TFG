#' Optimize the values of K and D for a given time series
#' 
#' @param x A time series
#' @param k Values of Ks to be analyzed
#' @param d Values of Ds to be analyzed
#' @param v Variable to be predicted
#' @param error_metric Type of metric to evaluate the prediction error
#' @param weight Type of weight to use at the time of calculating the predicted value with a weighted mean. 
#- Three supported: proximity, same, trend.
#' \describe{
#'   \item{proximity}{the weight assigned to each neighbor is proportional to its distance}
#'   \item{same}{all neighbors are assigned with the same weight}
#'   \item{trend}{nearest neighbor is assigned with weight k, second closest neighbor with weight k-1, and so on until the 
#'                least nearest neighbor which is assigned with a weight of 1.}
#' }
#' @param threads Number of threads to be used when parallelizing
#' @param file Name or id of the files where the distances matrixes are saved
#' @return A matrix of errors, optimal K & D

knn_optim_parallelf = function(x, k, d, v = 1, error_metric = "MAE", weight = "proximity", threads = 3, file){
    require(parallelDist)
    require(forecast)
    require(foreach)
    require(doParallel)
    require(iterators)
    
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
    
    # Once we have all distances matrixes we proceed to evaluate in parallel with a different combination
    # of d and row.
    # For each of the combinations we order all the neighbors(elements) by proximity and evaluate with 
    # all the posible values for k, taking each time the k-Nearest ones, to make k predictions.
    # Finally when we have all the predictions we calculate the error for each prediction and store them
    # in the variable of the foreach loop.
    
    init <- floor(n * 0.7)
    #clust <- makeCluster(parallel::detectCores()-1)
    clust <- makeCluster(threads)
    registerDoParallel(cl = clust)
    
    raw_preds <- foreach(act_row = init:(n - 1)) %:% foreach(act_d = iter(d)) %dopar% {
        # Obtain the distances matrix for the actual d
        # TODO: eliminar las variables para evitar duplicación innecesaria de memoria. De momento se deja así para simplificar cambios
        d_index <- match(act_d, d)
        row_index <- act_row - act_d + 1
        preds <- vector(mode = "numeric", ks)
        
        distances_element <- readRDS(paste0(file, act_d))[row_index, 1:(row_index - 1)]
        
        # For k = act_row get the indexes of all neighbors(elements) ordered by distance
        dist_row <- sort.int(distances_element, index.return = TRUE)
        
        for (k_index in 1:ks) {
            k_value <- k[k_index]
            
            # Get the indexes k nearest neighbors(elements)
            k_nn <- head(dist_row$ix, k_value)
            
            # Calculate the weights for the future computation of the weighted mean
            weights <- switch(weight, 
                              proximity = {1/(distances_element[k_nn] + 1)},
                              same = {rep.int(1,k_value)},
                              trend = {k_value:1})
            
            # Calculate the predicted value 
            preds[k_index] <- weighted.mean(y[k_nn + act_d, v], weights)
        }
        
        list(d_index = d_index, instant_index = act_row - init + 1, preds = preds)
    }
    
    registerDoSEQ()
    stopCluster(clust)
    
    # Change the format of raw_preds to d matrixes of k x (n - init) dimension in order
    preds_list <- vector("list", ds)
    j <- 1
    for (i in d) {
        preds_list[[j]] <- matrix(nrow = ks, ncol = n - init)
        j <- j + 1
    }
    
    for (l_elems in raw_preds) {
        for (elem in l_elems){
            preds_list[[elem$d_index]][, elem$instant_index] <- elem$preds
        }
    }
    
    # Calculate error values between the known values and the predicted values, these values go from init to t - 1
    # and for all Ks
    errors <- matrix(nrow = ks, ncol = ds)
    real_values <- y[(init + 1):n, v]
    for (i in 1:ds) {
        for (ind in 1:ks) {
            errors[ind, i] <- accuracy(ts(preds_list[[i]][ind, ]), matrix(real_values))[error_type]
        }
    }
    
    # Construction of the list to be returned
    index_min_error <- which.min(errors)
    optK <- k[((index_min_error - 1) %% ks) + 1]
    optD <- d[ceiling(index_min_error / ks)]
    result <- list(errors = errors, k = optK, d = optD)
    
    result
}