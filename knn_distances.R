#' Calculates one distances matrix per each d for the given time series and then save them in files (one distances
#' matrix per file).
#'
#' @param y A time series.
#' @param d Values of d's to be analyzed.
#' @param distance_metric Type of metric to evaluate the distance between points. Many metrics are supported: euclidean, manhattan,
#' dynamic time warping, camberra and others. For more information about the supported metrics check the values that 'method'
#' argument of function parDist (from parallelDist package) can take as this is the function used to calculate the distances.
#' Link to the package info: https://cran.r-project.org/web/packages/parallelDist
#' Some of the values that this argument can take are "euclidean", "manhattan", "dtw", "camberra", "chord".
#' @param threads Number of threads to be used when parallelizing distances calculation, default is number of cores detected - 1 or
#' 1 if there is only one core.
#' @param file Path and id of the files where the distances matrixes will be saved.
#' @examples
#' knn_distances(AirPassengers, 1:3, file = "AirPassengers")
#' knn_distances(LakeHuron, 1:6, file = "LakeHuron")
knn_distances <- function(y, d, distance_metric = "euclidean", threads = NULL, file){
  require(parallelDist)
  require(parallel)

  # Default number of threads to be used
  if (is.null(threads)) {
    cores <- parallel::detectCores()
    threads <- ifelse(cores == 1, cores, cores - 1)
  }

  # Initialization of variables to be used
  y <- as.matrix(sapply(y, as.numeric), ncol = NCOL(y))

  # Calculate one distances matrix for each d, as the distance variates
  # with the number of values that characterizes each element. This matrixes
  # are saved in files.
  for (act_d in d) {
      # Get 'elements' matrix
      elements_matrix <- knn_elements(y, act_d)

      # Calculate distances between every 'element', a 'triangular matrix' is returned
      distances_new_element <- parDist(elements_matrix, distance_metric, threads = threads)

      # Save distances matrix in file
      saveRDS(distances_new_element, paste0(file, "-", act_d))
  }

}
