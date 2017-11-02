#' Predict next value of the time series using k-nearest neighbors algorithm
#'
#' @param y A time series
#' @param k Number of neighbors
#' @param d Dimension
#' @param v Variable to be predicted if given multivariate time series. By default the first one
#' @param method Type of distance to use. By default Euclidean distance
#' @return The predicted value
#' @examples
#' add(1, 1) --cambiar
#' add(10, 1) --cambiar

knn_funct = function(y, k, d, v=1, method="euclidean"){
  require(rdist)
  n <- NROW(y)
  m <- NCOL(y)
  roof <- n - d
  
  if (any(class(y) == "mts")) {
    target <- matrix(as.vector(y[(roof + 1):n,]), nrow = 1)
  } else {
    target <- t(y[(roof + 1):n])
  }
  
  #print(target)
  
  if (any(class(y) == "mts")) {
    # Pre-reservando la matriz
    neighs <- matrix(nrow = roof, ncol = d * m)
    h <- 1
    for (i in 1:m) {
      for (j in 1:d) {
        neighs[, h] <- y[(j:(j + roof - 1)), i]
        h <- h + 1
      }
    }
  } else {
    # Pre-reservando la matriz
    neighs = matrix(nrow = roof, ncol = d)
    for (i in 1:d) {
      neighs[,i] <- y[ i:(i + roof - 1)]
    }
  }
  
  #print(neighs)
  
  # Calculamos las distancias 
  distances <- cdist(neighs, target, metric = method)
  
  # Buscamos las k distancias mas pequenas y luego los indices correspondientes
  k_nn <- head( which( distances %in% head(sort.int(distances), k) ), k )
  
  if (any(class(y) == "mts")) {
    prediction <- array(dim = m)
    for (i in 1:m) {
      prediction[i] <- mean(y[(k_nn + d), i])
    }
    
  } else {
    prediction <- mean(y[(k_nn + d)])
  }
  
  prediction
}


#' Calculate the distance matrix
#'
#' @param y A time series
#' @param d Dimension
#' @param method Type of distance to use. By default Euclidean distance
#' @return The distance matrix

distances = function(y, d, method="euclidean"){
    require(rdist)
    n <- NROW(y)
    m <- NCOL(y)
    roof <- n - d
    
    #print(target)
    
    if (any(class(y) == "mts")) {
        # Pre-reservando la matriz
        neighs <- matrix(nrow = roof + 1, ncol = d * m)
        k <- 1
        for (i in 1:m) {
            for (j in 1:d) {
                neighs[, k] <- y[(j:(j + roof)), i]
                k <- k + 1
            }
        }
    } else {
        # Pre-reservando la matriz
        neighs = matrix(nrow = roof + 1, ncol = d)
        for (i in 1:d) {
            neighs[,i] <- y[ i:(i + roof - 1)]
        }
    }
    
    dist <- rdist(neighs, metric = method)
    
    dist
    
}


past_predict = function(y, k, d, init, method="euclidean"){
  require(rdist)
  n <- NROW(y)
  m <- NCOL(y)
  roof <- n - d
  preds <- n - init
  
  #print(target)
  
  if (any(class(y) == "mts")) {
    # Pre-reservando la matriz
    neighs <- matrix(nrow = roof + 1, ncol = d * m)
    k <- 1
    for (i in 1:m) {
      for (j in 1:d) {
        neighs[, k] <- y[(j:(j + roof)), i]
        k <- k + 1
      }
    }
  } else {
    # Pre-reservando la matriz
    neighs = matrix(nrow = roof + 1, ncol = d)
    for (i in 1:d) {
      neighs[,i] <- y[ i:(i + roof - 1)]
    }
  }
  
  #print(neighs)
  
  if (any(class(y) == "mts")) {
    prediction <- array(dim = c(m, preds))
  } else {
    prediction <- array(dim = preds)
  }
  
  for (j in init:n) {
    
    print(neighs[j - d + 1,])
    target <- matrix(neighs[j - d + 1,], nrow = 1)
    
    # Calculamos las distancias con el nuevo target
    print(neighs[(1:(j - d)), ])
    distances <- cdist(neighs[(1:(j - d)), ], target, metric = method)
    
    # Buscamos las k distancias mas pequenas y luego los indices correspondientes
    k_nn <- head( which( distances %in% head(sort.int(distances), k) ), k )
    
    if (any(class(y) == "mts")) {
      for (i in 1:m) {
        prediction[i,(j - init)] <- mean(y[(k_nn + d), i])
      }
      
    } else {
      prediction[j] <- mean(y[(k_nn + d)])
    }
  }
  
  prediction
  
}