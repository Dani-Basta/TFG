#' Distances matrixes computation and saving in files with a maximum of columns
#'
#' Calculates one distances matrix per each d for the given time series and then save them in files. Each file will
#' contain a maximum of 'cols' number of columns from the corresponding distances matrix.
#'
#' @param y A time series.
#' @param d Values of d's to be analyzed.
#' @param distance_metric Type of metric to evaluate the distance between points. Many metrics are supported: euclidean, manhattan,
#' dynamic time warping, canberra and others. For more information about the supported metrics check the values that 'method'
#' argument of function 'parDist' (from 'parallelDist' package) can take as this is the function used to calculate the distances.
#' Link to the package info: \url{https://cran.r-project.org/package=parallelDist}.
#' Some of the values that this argument can take are "euclidean", "manhattan", "dtw", "canberra", "chord".
#' @param threads Number of threads to be used when parallelizing distances calculation, default is number of cores detected - 1 or
#' 1 if there is only one core.
#' @param file Path and id of the files where the distances matrixes will be saved.
#' @param cols Number of columns per file.
#' @examples
#' knn_distances(AirPassengers, 1:3, threads = 2, file = "AirPassengers", cols = 2)
#' knn_distances(LakeHuron, 1:6, threads = 2, file = "LakeHuron", cols = 10)
#' @export
knn_distances <- function(y, d, distance_metric = "euclidean", threads = NULL, file, cols = 1){

  # Default number of threads to be used
  if (is.null(threads)) {
    cores <- parallel::detectCores()
    threads <- ifelse(cores == 1, cores, cores - 1)
  }

  # Initialization of variables to be used
  y <- matrix(y, ncol = NCOL(y))
  n <- NROW(y)

  # Calculate one distances matrix for each d, as the distance variates
  # with the number of values that characterizes each 'element'. This matrixes
  # are saved in files.
  for (act_d in d) {
    # Get 'elements' matrix
    elements_matrix <- knn_elements(y, act_d)

    # Calculate distances between every 'element', a 'triangular matrix' is returned
    distances <- parallelDist::parDist(elements_matrix, distance_metric, threads = threads)

    # Save distances matrix in files that will contain a determined number of columns
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
