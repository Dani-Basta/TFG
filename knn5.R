knn_funct = function(y, k, d, method="euclidean"){
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
    k <- 1
    for (i in 1:m) {
      for (j in 1:d) {
        neighs[, k] <- y[(j:(j + roof - 1)), i]
        k <- k + 1
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
    for (i in 1:m){
      prediction[i] <- mean(y[(k_nn + d), i])
    }
    
  } else {
    prediction <- mean(y[(k_nn + d)])
  }
  
  prediction
}

past_predict = function(y, k, d, init, method="euclidean"){
    require(rdist)
    n <- NROW(y)
    m <- NCOL(y)
    roof <- n - d
    preds <- n - init
    
    if (any(class(y) == "mts")) {
        target <- matrix(as.vector(y[(roof + 1):n,]), nrow = 1)
    } else {
        target <- t(y[(roof + 1):n])
    }
    
    #print(target)
    
    if (any(class(y) == "mts")) {
        # Pre-reservando la matriz
        neighs <- matrix(nrow = roof, ncol = d * m)
        k <- 1
        for (i in 1:m) {
            for (j in 1:d) {
                neighs[, k] <- y[(j:(j + roof - 1)), i]
                k <- k + 1
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
    
    if (any(class(y) == "mts")) {
        prediction <- array(dim = c(m, preds))
    } else {
        prediction <- array(dim = preds)
    }
    
    for (j in init:n) {
            
        # Buscamos las k distancias mas pequenas y luego los indices correspondientes
        k_nn <- head( which( distances %in% head(sort.int(distances[1:j]), k) ), k )
        
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