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

knn_optim_parallelf2 = function(x, k, d, v = 1, init = NULL, error_metric = "MAE", weight = "proximity", threads = NULL, file, rows){
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
  real_values <- matrix(rev(y[(init + 1):n, v]))
  errors <- matrix(nrow = ks, ncol = ds, dimnames = list(k, d))

  # Once we have all distances matrixes we proceed to evaluate in parallel with a different combination
  # of d and row.
  # For each of the combinations we order all the neighbors(elements) by proximity and evaluate with
  # all the posible values for k, taking each time the k-Nearest ones, to make k predictions.
  # Finally when we have all the predictions we calculate the error for each prediction and store them
  # in the variable of the foreach loop.

  clust <- makeCluster(threads)
  registerDoParallel(cl = clust)

  if (rows == 1) {
    num_of_file_array <- 2:(ceiling((n - init + 1) / rows))
  }
  else {
    num_of_file_array <- 1:(ceiling((n - init + 1) / rows))
  }

  all_predictions <- foreach(i = 1:ds, .combine = cbind) %:% foreach(num_of_file = num_of_file_array, .combine = cbind) %dopar% {
    num_cols_in_file <- ifelse(num_of_file * rows > n - init + 1, (n - init + 1) %% rows, rows)

    if (num_of_file == 1 && rows > 1) {
      predictions <- matrix(nrow = ks, ncol = num_cols_in_file - 1)
      j_in_file_array <- 2:num_cols_in_file
    }
    else {
      predictions <- matrix(nrow = ks, ncol = num_cols_in_file)
      j_in_file_array <- 1:num_cols_in_file
    }
    # Get distances matrix
    distances_matrix_size <- n - d[i] - rows * (num_of_file - 1) + 1
    distances_matrix <- readRDS(paste0(file, d[i], "_", num_of_file))

    # The ifelse its because the first column of the fist file is not use
    for (j_in_file in j_in_file_array) {
      # Get column and sort it
      initial_index <- distances_matrix_size * (j_in_file - 1) - j_in_file * (j_in_file - 1) / 2 + 1
      distances_col <- distances_matrix[initial_index:(initial_index + distances_matrix_size - j_in_file - 1)]
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

        #Calculate the predicted value
        if (num_of_file == 1 && rows > 1) {
          predictions[k_index, j_in_file - 1] <- weighted.mean(y[n - (num_of_file - 1) * rows - j_in_file + 2 - k_nn, v], weights)
        }
        else {
          predictions[k_index, j_in_file] <- weighted.mean(y[n - (num_of_file - 1) * rows - j_in_file + 2 - k_nn, v], weights)

        }

      }
    }

    predictions
  }

  registerDoSEQ()
  stopCluster(clust)

  # Calculate error values between the known values and the predicted values, these values go from init to t - 1
  # and for all Ks
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
