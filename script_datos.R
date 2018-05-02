library(zoo)

#####
####Still in development
#####
#Load to Enviroment "knn_optim", "knn_past" and "knn_elements"

x <- sunspot.month
#x <- rain

n <- NROW(x)
train_init <- floor(n * 0.7)
test_init <- floor(n * 0.9)
x_train <- x[1:test_init]
distance <- "euclidean"
error_metric <-  "MAE"
weight <- "proximity"
nThreads <- 7
ks <-  1:50
ds <- 1:30
dates <- as.Date(time(x))
min_x <- min(x)
max_x <- max(x)

# Get best k and d
res <- knn_optim(x = x_train, k = ks, d = ds, init= train_init, distance_metric = distance, error_metric = error_metric, weight = weight)

euc_prox_train <- knn_past(x = x_train, k = res$k, d = res$d, init = train_init, distance_metric = distance, weight = "proximity", threads = nThreads)
euc_prox_test <- knn_past(x = x, k = res$k, d = res$d, init = test_init, distance_metric = distance, weight = "proximity", threads = nThreads)
euc_prox <- c(euc_prox_train, euc_prox_test)

x_err <- ts(x[(train_init + 1):n])
x_train_err <- ts(x[(train_init + 1):test_init])
x_test_err <- ts(x[(test_init + 1):n])
sub_dates <- tail(dates, length(x) - train_init)
naive <- x[train_init:(NROW(x)-1)]
# Exponential smoothing:
# alpha = NULL - default alpha is estimated
# initial = "simple" - the initial values are set to values obtained using simple calculations on the first few observations
exp_smoothing <- fitted(ses(x, alpha = NULL, initial = "simple"))[(init+1):NROW(x)]

# Data for errors table
names_col <- c("Proximity")
train_error <- accuracy(ts(euc_prox_train), x_train_err)
test_error <- accuracy(ts(euc_prox_test), x_test_err)
errors_matrix <- matrix(c(train_error, test_error), nrow = 1)
