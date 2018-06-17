library(zoo)

#####
####Still in development
#####
#Load to Enviroment "knn_optim", "knn_past" and "knn_elements"

y <- sunspot.month

n <- NROW(y)
train_init <- floor(n * 0.7)
test_init <- floor(n * 0.9)
y_train <- y[1:test_init]
distance <- "euclidean"
error_metric <-  "RMSE"
weight <- "proximity"
n_threads <- 7
ks <-  1:60
ds <- 1:30
dates <- as.Date(time(y))
min_y <- min(y)
max_y <- max(y)

# Get errors matrix, and best k and d
res <- knn_optim(y = y_train, k = ks, d = ds, init = train_init, distance_metric = distance, 
                 error_metric = error_metric, weight = weight, threads = n_threads)

optimal_train <- knn_past(y = y_train, k = res$k, d = res$d, init = train_init, 
                           distance_metric = distance, weight = "proximity", threads = n_threads)
optimal_test <- knn_past(y = y, k = res$k, d = res$d, init = test_init, 
                          distance_metric = distance, weight = "proximity", threads = n_threads)
optimal <- c(optimal_train, optimal_test)

y_err <- ts(y[(train_init + 1):n])
y_train_err <- ts(y[(train_init + 1):test_init])
y_test_err <- ts(y[(test_init + 1):n])
sub_dates <- tail(dates, length(y) - train_init)
naive <- ts(y[train_init:(n - 1)])
cont_min <- min(res$errors)
cont_max_fix <- (max(res$errors) - (max(res$errors) - cont_min) * 0.4)
num_contours <- 20

minimums <- head(sort.int(res$errors, index.return = TRUE)$ix , 5)

x_minims <- ((minimums - 1) %% max(ks)) + 1
y_minims <- ceiling(minimums/max(ks))


# Data for residuals
residuals_matrix <- matrix(nrow = 5, ncol = length(y_train_err) + length(y_test_err))
residuals_matrix[1, ] <- y_err - optimal
residuals_matrix[2, ] <- y_err - naive

# Data for errors table
names_col <- c("Optimal", "Naive", "Seasonal Naive", "", "", "")
optimal_train_error <- accuracy(ts(optimal_train), y_train_err)
optimal_test_error <- accuracy(ts(optimal_test), y_test_err)
naive_train_error <- accuracy(naive[1:length(y_train_err)], y_train_err)
naive_test_error <- accuracy(naive[(length(y_test_err) + 1):length(naive)], y_test_err)

errors_matrix <- matrix(nrow = 5, ncol = 14)
errors_matrix[1, ] <- c(train_error, test_error)
errors_matrix[2, ] <- c(naive_train_error, naive_test_error)

errors_matrix_tab1 <- errors_matrix
errors_matrix_tab2 <- errors_matrix

# Data for selected methods
selected_methods <- rep(FALSE, 5)

# Data for selected points in contour
selected_points <- matrix(rep(FALSE, NROW(res$errors) * NCOL(res$errors)), nrow = NROW(res$errors), ncol = NCOL(res$errors))

