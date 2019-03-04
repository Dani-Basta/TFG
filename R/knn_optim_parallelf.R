#' Parallel k and d optimization reading from files
#'
#' Optimizes the values of k and d for a given time series. First, values corresponding to instants from init + 1 to the last one
#' are predicted. The first value predicted, which corresponds to instant init + 1, is calculated using instants from 1 to
#' instant init; the second value predicted, which corresponds to instant init + 2, is predicted using instants from 1
#' to instant init + 1; and so on until the last value, which corresponds to instant n (length of the given time series),
#' is predicted using instants from 1 to instant n - 1. Finally, the error is evaluated between the predicted values and
#' the real values of the series.
#' This version of the optimization function uses a parallelized distances calculation function, and the computation of
#' the predicted values is done parallelizing by the number of d's and the number of instants to be predicted. Each thread
#' that calculates predicted values reads only the part of the corresponding distances matrix in which the information used
#' to predict is contained.
#'
#' @param y A time series.
#' @param k Values of k;s to be analyzed.
#' @param d Values of d's to be analyzed.
#' @param v Variable to be predicted if given multivariate time series.
#' @param init Variable that determines the limit of the known past for the first instant predicted.
#' @param error_metric Type of metric to evaluate the prediction error.
#' Five metrics supported:
#' \describe{
#'   \item{ME}{Mean Error}
#'   \item{RMSE}{Root Mean Squared Error}
#'   \item{MAE}{Mean Absolute Error}
#'   \item{MPE}{Mean Percentage Error}
#'   \item{MAPE}{Mean Absolute Percentage Error}
#' }
#' @param weight Type of weight to be used at the time of calculating the predicted value with a weighted mean.
#' Three supported: proximity, same, linear.
#' \describe{
#'   \item{proximity}{the weight assigned to each neighbor is proportional to its distance}
#'   \item{same}{all neighbors are assigned with the same weight}
#'   \item{linear}{nearest neighbor is assigned with weight k, second closest neighbor with weight k-1, and so on until the
#'                least nearest neighbor which is assigned with a weight of 1.}
#' }
#' @param threads Number of threads to be used when parallelizing, default is number of cores detected - 1 or
#' 1 if there is only one core.
#' @param file Path and id of the files where the distances matrixes are.
#' @param cols Number of columns per file.
#' @return A matrix of errors, optimal k and d.
#' @examples
#' knn_distances(AirPassengers, 1:3, file = "AirPassengers", cols = 2, threads = 2)
#' knn_optim_parallelf(AirPassengers, 1:5, 1:3, file = "AirPassengers", cols = 2, threads = 2)
#' knn_distances(LakeHuron, 1:6, file = "LakeHuron", cols = 10, threads = 2)
#' knn_optim_parallelf(LakeHuron, 1:10, 1:6, file = "LakeHuron", cols = 10, threads = 2)
#' @import foreach
#' @export
knn_optim_parallelf <- function(y, k, d, v = 1, init = NULL, error_metric = "MAE", weight = "proximity", threads = NULL, file, cols){

  # Default number of threads to be used
  if (is.null(threads)) {
    cores <- parallel::detectCores()
    threads <- ifelse(cores == 1, cores, cores - 1)
  }

  # Choose the appropiate index of the accuracy result, depending on the error_metric
  error_type <- switch(error_metric,
                       ME = 1,
                       RMSE = 2,
                       MAE = 3,
                       MPE = 4,
                       MAPE = 5
  )

  # Sort k or d vector if they are unsorted
  if (is.unsorted(k)) {
    k <- sort(k)
  }
  if (is.unsorted(d)) {
    d <- sort(d)
  }

  # Initialization of variables to be used
  y <- matrix(y, ncol = NCOL(y))
  n <- NROW(y)
  ks <- length(k)
  ds <- length(d)
  init <- ifelse(is.null(init), floor(n * 0.7), init)
  real_values <- matrix(rev(y[(init + 1):n, v]))
  errors <- matrix(nrow = ks, ncol = ds, dimnames = list(k, d))

  # This next line is only there to avoid 'No visible binding for global variable' warning
  # in R CMD check due to num_of_file variable used in foreach loop
  num_of_file <- NULL

  # For each of the combinations of d's and instants init to n - 1, a distances vector
  # according to each combination is taken from a file and then ordered.
  # Later, the k's inner loop applies k-nn to predict values.

  clust <- parallel::makeCluster(threads)
  doParallel::registerDoParallel(cl = clust)

  # ids of files to be open
  if (cols == 1) {
    num_of_file_array <- 2:(ceiling((n - init + 1) / cols))
  }
  else {
    num_of_file_array <- 1:(ceiling((n - init + 1) / cols))
  }

  all_predictions <- foreach::foreach(i = 1:ds, .combine = cbind) %:% foreach::foreach(num_of_file = num_of_file_array, .combine = cbind) %dopar% {
    num_cols_in_file <- ifelse(num_of_file * cols > n - init + 1, (n - init + 1) %% cols, cols)

    if (num_of_file == 1 && cols > 1) {
      predictions <- matrix(nrow = ks, ncol = num_cols_in_file - 1)
      j_in_file_array <- 2:num_cols_in_file
    }
    else {
      predictions <- matrix(nrow = ks, ncol = num_cols_in_file)
      j_in_file_array <- 1:num_cols_in_file
    }
    # Get distances matrix
    distances_matrix_size <- n - d[i] - cols * (num_of_file - 1) + 1
    distances_matrix <- readRDS(paste0(file, d[i], "_", num_of_file))

    for (j_in_file in j_in_file_array) {
      # Get column and sort it
      initial_index <- distances_matrix_size * (j_in_file - 1) - j_in_file * (j_in_file - 1) / 2 + 1
      distances_col <- distances_matrix[initial_index:(initial_index + distances_matrix_size - j_in_file - 1)]
      sorted_distances_col <- sort.int(distances_col, index.return = TRUE)

      for (k_index in 1:ks) {
        k_value <- k[k_index]

        # Get the indexes of the k nearest 'elements', these are called neighbors
        k_nn <- utils::head(sorted_distances_col$ix, k_value)

        # Calculate the weights for the future computation of the weighted mean
        weights <- switch(weight,
                          proximity = 1 / (distances_col[k_nn] + .Machine$double.xmin * 1e150),
                          same = rep.int(1, k_value),
                          linear = k_value:1)

        # Calculate the predicted value
        if (num_of_file == 1 && cols > 1) {
          predictions[k_index, j_in_file - 1] <- stats::weighted.mean(y[n - (num_of_file - 1) * cols - j_in_file + 2 - k_nn, v], weights)
        }
        else {
          predictions[k_index, j_in_file] <- stats::weighted.mean(y[n - (num_of_file - 1) * cols - j_in_file + 2 - k_nn, v], weights)

        }

      }
    }

    predictions
  }

  foreach::registerDoSEQ()
  parallel::stopCluster(clust)

  # Calculate error values between the known values and the predicted values, these values
  # correspond to instants init to n - 1. These is done for all k's and d's analyzed
  for (i in 1:ds) {
    initial_index <- (i - 1) * (n - init) + 1
    for (k_index in 1:ks) {
      errors[k_index, i] <- forecast::accuracy(stats::ts(all_predictions[k_index, initial_index:(initial_index + n - init - 1)]), real_values)[error_type]
    }
  }

  # Construction of the list to be returned
  index_min_error <- which.min(errors)
  opt_k <- k[((index_min_error - 1) %% ks) + 1]
  opt_d <- d[ceiling(index_min_error / ks)]
  result <- list(errors = errors, k = opt_k, d = opt_d)

  result
}
