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
#' @param rows Number of rows per file
#' @param file Name or id of the files where the distances matrixes are saved
#' @return A matrix of errors, optimal K & D

knn_optim_parallelf = function(x, k, d, v = 1, error_metric = "MAE", weight = "proximity", threads = 0, file){
  require(parallelDist)
  require(forecast)
  require(foreach)
  require(doParallel)
  require(iterators)

  threads <- ifelse(threads == 0, parallel::detectCores() - 1, threads)

  # Choose the appropiate index of the accuracy result, depending on the error_metric
  error_type <- switch(error_metric,
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
  clust <- makeCluster(threads)
  registerDoParallel(cl = clust)

    all_predictions <- foreach(i = 1:ds, .combine = cbind) %:% foreach(j = (n - init + 1):2, .combine = cbind) %dopar% {
        predictions <- vector(mode = "numeric", ks)

        # Get column needed from the distances matrix and sort it
        initial_index <- (n - d[i] + 1) * (j - 1) - j * (j - 1) / 2 + 1
        distances_col <- readRDS(paste0(file, "-", d[i]))[initial_index:(initial_index + n - d[i] - j)]
        sorted_distances_col <- sort.int(distances_col, index.return = TRUE)

        for (k_index in 1:ks) {
            k_value <- k[k_index]

            # Get the indexes k nearest neighbors(elements)
            k_nn <- head(sorted_distances_col$ix, k_value)

            # Calculate the weights for the future computation of the weighted mean
            weights <- switch(weight,
                              proximity = {1 / (distances_col[k_nn] + .Machine$double.xmin * 1e150)},
                              same = {rep.int(1, k_value)},
                              trend = {k_value:1})

            # Calculate the predicted value
            predictions[k_index] <- weighted.mean(y[n - j + 2 - k_nn, v], weights)
        }

        predictions
    }

    registerDoSEQ()
    stopCluster(clust)

    # Calculate error values between the known values and the predicted values, these values go from init to t - 1
    # and for all Ks
    errors <- matrix(nrow = ks, ncol = ds)
    real_values <- matrix(y[(init + 1):n, v])
    for (i in 1:ds) {
        initial_index <- (i - 1) * (n - init) + 1
        for (k_index in 1:ks) {
            errors[k_index, i] <- accuracy(ts(all_predictions[k_index, initial_index:(initial_index + n - init - 1)]), real_values)[error_type]
        }
    }

    # Construction of the list to be returned
    index_min_error <- which.min(errors)
    optK <- k[((index_min_error - 1) %% ks) + 1]
    optD <- d[ceiling(index_min_error / ks)]
    result <- list(errors = errors, k = optK, d = optD)

    return(result)
}
