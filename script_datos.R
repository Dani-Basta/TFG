#####
####Still in development
#####
#Load to Enviroment "knn_optim", "knn_past" and "knn_elements"

#x <- sunspot.month
x <- rain

init <- floor(NROW(x)*0.7)
distance <- "euclidean"
error_metric <-  "MAE"
weight <- "proximity"
nThreads <- 7
ks <-  1:50
ds <- 1:30

res <- knn_optim(x = x, k = ks, d = ds, init  = init, distance_metric = distance, error_metric = error_metric, weight = weight)

EucPro <- knn_past(x = x, k = res$k, d = res$d, init = init, distance_metric = distance, weight = "proximity", threads = nThreads)
EucTre <- knn_past(x = x, k = res$k, d = res$d, init = init, distance_metric = distance, weight = "trend", threads = nThreads)
EucSame <- knn_past(x = x, k = res$k, d = res$d, init = init, distance_metric = distance, weight = "same", threads = nThreads)

subX <- x[(init+1):NROW(x)]
naive <- x[init:(NROW(x)-1)]
