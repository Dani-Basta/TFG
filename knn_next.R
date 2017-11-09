#' Predict next value of the time series using k-nearest neighbors algorithm
#'
#' @param x A time series
#' @param k Number of neighbors
#' @param d Length of the "neighbourhoods"
#' @param v Variable to be predicted if given multivariate time series
#' @param metric Type of metric to evaluate the distance between points
#' @param weight Type of weight to use at the time of the prediction. 3 supported: proximity, same, trend 
#' @return The predicted value

knn_next = function(x, k, d, v=1, metric="euclidean", weight="proximity"){
  require(rdist)
  y <- matrix(x, ncol = NCOL(x))
  n <- NROW(y)
  m <- NCOL(y)
  roof <- n - d
  
  # Obtenemos la matriz de vecindarios
  neighs <- knn_neighs(y, d)

  # Calculamos las distancias omitiendo las columnas que tiene los valores reales
  distances <- cdist(neighs[1:roof, 1:(d * m)], matrix(neighs[roof + 1, 1:(d * m)], nrow = 1) )
  
  # Buscamos las k distancias mas pequenas y luego los indices correspondientes
  #k_nn <- head( which( distances %in% head(sort.int(distances), k) ), k )
  # Esto: (sort.int(distances, index.return = TRUE))$ix da los indices antiguos de los elementos ordenados
  k_nn <- head((sort.int(distances, index.return = TRUE))$ix, k)
  
  # prediction <- weighted.mean(y[(k_nn + d), v], k:1)
  
  # Hay que acceder a la TS 1 única vez  al principio, a partir de ahí se usa la matriz de vecindarios
  # prediction <- weighted.mean(neighs[k_nn, k*d + v], k:1)
  
  # Hacemos la predicción sobre la matriz de vecindarios y ponderamos según el tipo elegido
  weights =  switch(weight,
        proximity = {
            # Falta mirar casos de 1/0
            1/distances[k_nn]
        },
        same = {
            rep.int(1,k)
        },
        trend = {
            k:1
        }
    )
  prediction <- weighted.mean(neighs[k_nn, k*d + v], weights)
  
  prediction
}