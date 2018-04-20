#' Calculate one distances matrix per each D for the given time series
#'
#' @param x A time series
#' @param d Values of Ds to be analyzed
#' @param distance_metric Type of metric to evaluate the distance between points. Many metrics are supported: euclidean, manhattan,
#' dynamic time warping, camberra and others. For more information about the supported metrics check the values that 'method'
#' argument of function parDist (from parallelDist package) can take as this is the function used to calculate the distances.
#' Link to the package info: https://cran.r-project.org/web/packages/parallelDist
#' Some of the values that this argument can take are "euclidean", "manhattan", "dtw", "camberra", "chord".
#' @param threads Number of threads to be used when parallelizing
#' @param cols Number of columns per file
#' @param file Name or id of the files where the distances matrixes will be saved
knn_distances2 = function(x, d, distance_metric = "euclidean", threads = NULL, file, cols = 1){
  require(parallelDist)
  require(parallel)

  threads <- ifelse(is.null(threads), parallel::detectCores() - 1, threads)

  # Initialization of variables to be used
  y <- matrix(x, ncol = NCOL(x))
  n <- NROW(y)

  # In order to parallelize we calculate the distances matrix just once for each d, as the distance variates
  # with the number of values that characterize each element

  #Calculate and save all distances matrixes
  for (act_d in d) {
    # Get elements matrix
    elements_matrix <- knn_elements(y, act_d)

    # Calculate distances between the last 'element' and each of the others 'elements'
    # This happens if d=1 and a univariate time series is given, a very unusual case
    if (is(elements_matrix, "numeric")) {
      elements_matrix <- matrix(elements_matrix, nrow = length(curr_elems))
    }
    distances <- parDist(elements_matrix, distance_metric, threads = threads)

    # Save distances matrix in file
    i <- 1
    num_of_file <- 1
    distances_length <- length(distances)
    act_column_length <- n - act_d
    while (i <= distances_length) {
      initial_i <- i
      j <- 1
      while (j <= cols && i <= distances_length) {
        i <- i + act_column_length
        act_column_length <-  act_column_length - 1
        j <- j + 1
      }
      saveRDS(distances[initial_i:(i - 1)], paste0(file, act_d, "_", num_of_file))
      num_of_file <- num_of_file + 1
    }

  }

}
