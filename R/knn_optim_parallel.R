#' Parallel k and d optimization
#'
#' Optimizes the values of K and D for a given time series. First, values corresponding to instants from init + 1 to the last one
#' are predicted. The first value predicted, which corresponds to instant init + 1, is calculated using instants from 1 to
#' instant init; the second value predicted, which corresponds to instant init + 2, is predicted using instants from 1
#' to instant init + 1; and so on until the last value, which corresponds to instant n (length of the given time series),
#' is predicted using instants from 1 to instant n - 1. Finally, the error is evaluated between the predicted values and
#' the real values of the series.
#' This version of the optimization function uses a parallelized distances calculation function, and the computation of
#' the predicted values is done parallelizing by the number of d's and the number of instants to be predicted.
#'
#' @param y A time series.
#' @param k Values of k's to be analyzed.
#' @param d Values of d's to be analyzed.
#' @param v Variable to be predicted if given multivariate time series.
#' @param init Variable that determines the limit of the known past for the first instant predicted.
#' @param distance_metric Type of metric to evaluate the distance between points. Many metrics are supported: euclidean, manhattan,
#' dynamic time warping, canberra and others. For more information about the supported metrics check the values that 'method'
#' argument of function 'parDist' (from 'parallelDist' package) can take as this is the function used to calculate the distances.
#' Link to the package info: \url{https://cran.r-project.org/package=parallelDist}.
#' Some of the values that this argument can take are "euclidean", "manhattan", "dtw", "canberra", "chord".
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
#' @return A matrix of errors, optimal k and d.
#' @examples
#' knn_optim_parallel(AirPassengers, 1:5, 1:3, threads = 2)
#' knn_optim_parallel(LakeHuron, 1:10, 1:6, threads = 2)
#' @import foreach
#' @export
knn_optim_parallel <- function(y, k, d, v = 1, init = NULL, distance_metric = "euclidean", error_metric = "MAE", weight = "proximity", threads = NULL){

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
    real_values <- matrix(y[(init + 1):n, v])
    errors <- matrix(nrow = ks, ncol = ds, dimnames = list(k, d))
    distances_matrixes <- vector("list", ds)
    distances_matrixes_sizes <- vector(mode = "numeric", ds)

    # This next line is only there to avoid 'No visible binding for global variable' warning
    # in R CMD check due to j variable used in foreach loop
    j <- NULL

    # Calculate one distances matrix for each d, as the distances variates
    # with the number of values that characterize each 'element'.
    for (i in 1:ds) {
        # Get 'elements' matrix
        elements_matrix <- knn_elements(y, d[i])

        # Calculate distances between every 'element', a 'triangular matrix' is returned
        distances_matrix <- parallelDist::parDist(elements_matrix, distance_metric, threads = threads)
        distances_matrixes[[i]] <- distances_matrix
        distances_matrixes_sizes[i] <- attr(distances_matrix, "Size")
    }

    # For each of the combinations of d's and instants init to n - 1, a distances vector
    # according to each combination is taken from the corresponding distances matrix and then
    # ordered. Later, the k's inner loop applies k-nn to predict values.

    clust <- parallel::makeCluster(threads)
    doParallel::registerDoParallel(cl = clust)

all_predictions <- foreach::foreach(i = 1:ds, .combine = cbind) %:% foreach::foreach(j = (n - init + 1):2, .combine = cbind) %dopar% {
        predictions <- vector(mode = "numeric", ks)

        # Get column needed from the distances matrix and sort it
        initial_index <- distances_matrixes_sizes[i] * (j - 1) - j * (j - 1) / 2 + 1
        distances_col <- distances_matrixes[[i]][initial_index:(initial_index + n - d[i] - j)]
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
            predictions[k_index] <- stats::weighted.mean(y[n - j + 2 - k_nn, v], weights)
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
