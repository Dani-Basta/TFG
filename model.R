model <- list()
class(model) <- "kNN"

model$method <- "k-Nearest Neighbors"

model$tested_ds <- d
model$tested_ks <- k

model$opt_k <- opt_k
model$opt_d <- opt_d

model$error_tested_kd_matrix <- errors_matrix

model$distance <- distance_metric

model$error_measure <- error_metric

model$weighting_scheme <- weight

model$call <- deparse(sys.call())
