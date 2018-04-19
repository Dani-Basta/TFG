#' Optimize the values of K and D for a given time series
#'
#' @param x A time series
#' @param k Values of Ks to be analyzed
#' @param d Values of Ds to be analyzed
#' @param v Variable to be predicted
#' @param distance_metric Type of metric to evaluate the distance between points. Many metrics are supported: euclidean, manhattan,
#' dynamic time warping, camberra and others. For more information about the supported metrics check the values that 'method'
#' argument of function parDist (from parallelDist package) can take as this is the function used to calculate the distances.
#' Link to the package info: https://cran.r-project.org/web/packages/parallelDist
#' Some of the values that this argument can take are "euclidean", "manhattan", "dtw", "camberra", "chord".
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
#' @return A matrix of errors, optimal K & D

knn_optim_parallel2 = function(x, k, d, v = 1, init = NULL, distance_metric = "euclidean", error_metric = "MAE", weight = "proximity", threads = NULL){
  require(parallelDist)
  require(forecast)
  require(foreach)
  require(doParallel)
  require(iterators)

  threads <- ifelse(is.null(threads), parallel::detectCores() - 1, threads)

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
  init <- ifelse(is.null(init), floor(n * 0.7), init)
  real_values <- matrix(y[(init + 1):n, v])
  distances_matrixes <- vector("list", ds)
  distances_matrixes_sizes <- vector(mode = "numeric", ds)

  # In order to paralelise we calculate the distances matrix just once for each d, as the distance variates
  # with the number of values that characterise each element.

  #Calculate all distances matrixes
  for (i in 1:ds) {
      # Get elements matrix
      elements_matrix <- knn_elements(y, d[i])

      # This happens if d=1 and a univariate time series is given, a very unusual case
      # This transformation is needed so that parDist doesn't throw an error
      if (is(elements_matrix, "numeric")) {
          elements_matrix <- matrix(elements_matrix, nrow = length(curr_elems))
      }

      # Calculate distances between every element, a 'triangular matrix' is returned
      distances_matrix <- parDist(elements_matrix, distance_metric, threads = threads)
      distances_matrixes[[i]] <- distances_matrix
      distances_matrixes_sizes[i] <- attr(distances_matrix, "Size")
  }

  # Once we have all distances matrixes we proceed to evaluate in parallel with a different combination
  # of d and row.
  # For each of the combinations we order all the neighbors(elements) by proximity and evaluate with
  # all the posible values for k, taking each time the k-Nearest ones, to make k predictions.
  # Finally when we have all the predictions we calculate the error for each prediction and store them
  # in the variable of the foreach loop.

  clust <- makeCluster(threads)
  registerDoParallel(cl = clust)

  errors_matrix <- foreach(i = 1:ds, .combine = cbind, .packages = "forecast") %dopar% {
    predictions <- matrix(nrow = ks, ncol = n - init)
    errors <- vector(mode = "numeric", ks)
    distances_matrix <- distances_matrixes[[i]]
    distances_matrix_size <- attr(distances_matrix, "Size")

    for (j in (n - init + 1):2) {
      # Get column needed from the distances matrix and sort it
      initial_index <- distances_matrix_size * (j - 1) - j * (j - 1) / 2 + 1
      distances_col <- distances_matrix[initial_index:(initial_index + n - d[i] - j)]
      sorted_distances_col <- sort.int(distances_col, index.return = TRUE)

      for (k_index in 1:ks) {
        k_value <- k[k_index]

        # Get the indexes k nearest elements
        k_nn <- head(sorted_distances_col$ix, k_value)

        # Calculate the weights for the future computation of the weighted mean
        weights = switch(weight, proximity = {1 / (distances_col[k_nn] + .Machine$double.xmin * 1e150)},
                         same = {rep.int(1, k_value)},
                         trend = {k_value:1})

        # Calculate the predicted value
        predictions[k_index, n - init + 2 - j] <- weighted.mean(y[n - j + 2 - k_nn, v], weights)
        }
  }

    # Calculate error values between the known values and the predicted values, these values go from init to t - 1
    # and for all Ks
    for (k_index in 1:ks) {
      errors[k_index] <- accuracy(ts(predictions[k_index, ]), real_values)[error_type]
    }

    errors
  }

  registerDoSEQ()
  stopCluster(clust)

  # Construction of the list to be returned
  index_min_error <- which.min(errors_matrix)
  optK <- k[((index_min_error - 1) %% ks) + 1]
  optD <- d[ceiling(index_min_error / ks)]
  result <- list(errors = errors_matrix, k = optK, d = optD)

  result
}
