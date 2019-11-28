library(zoo)
library(forecast)

#####
#### Still in development
#####
# Load to Enviroment "knn_param_search", "knn_past" and "knn_elements"

# y <- nottem
y <- datasets::sunspot.month
dates <- as.Date(time(y))
full_dates <- as.Date(ts( c(y,1), start = time(y)[1], frequency = frequency(y) ))

# y <- as.matrix(interCompFore, ncols = 1)
# dates <- 1:length(y)

# y <- as.matrix(btcnWeekRend, ncols = 1)
# dates <- as.Date(read.csv("D:/Daniel/Investigación/BTC-EUR-weekly.csv")[,1])
# 
# y <- ts(get("rain"), start = 1914, frequency = 365.25)
# dates <- as.Date(time(y))

n <- NROW(y)
train_init <- floor(n * 0.85)
test_init <- floor(n * 0.95)
y_train <- head(y, test_init) # [1:test_init]
distance <- "euclidean"
error_measure <-  "RMSE"
weight <- "proportional"
n_threads <- 5
ks <-  1:80
ds <- 1:80
# dates <- as.Date(time(y))
min_y <- min(y)
max_y <- max(y)

# Get errors matrix, and best k and d
res <- knn_param_search(y = y_train, k = ks, d = ds, init = train_init, distance = distance, 
                 error_measure = error_measure, weight = weight, threads = n_threads)

# optimal_train <- knn_past(y = y_train, k = res$opt_k, d = res$opt_d, init = train_init, 
#                           distance_metric = distance, weight = weight, threads = n_threads)
# optimal_test <- knn_past(y = y, k = res$opt_k, d = res$opt_d, init = test_init, 
#                          distance_metric = distance, weight = weight, threads = n_threads)
# optimal <- c(optimal_train$mean, optimal_test$mean)

optimal <- knn_past(y = y, k = res$opt_k, d = res$opt_d, init = train_init, 
                    distance = distance, weight = weight, threads = n_threads)
optimal_train <- head(optimal$mean, test_init-train_init )
optimal_test <- tail(optimal$mean, n-test_init )


y_err <- tail(y, (n - train_init )) # ts(y[(train_init + 1):n]) #  
y_train_err <-  y[(train_init + 1):test_init] # tail( head(y, test_init), (test_init - train_init)) #
y_test_err <- y[(test_init + 1):n] #  tail(y, (n - test_init - 1) ) #
sub_dates <- tail(dates, length(y) - train_init)
# sub_dates <- c(as.Date(time(optimal_train$mean)), as.Date(time(optimal_test$mean))) 
# sub_dates <- as.Date(time(optimal)) 
naive <- y[train_init:(n - 1)] # ts(y[train_init:(n - 1)], start = time(y)[train_init + 1], frequency = frequency(y)) #
# naive_total_error <- accuracy(naive, y_err) [switch(error_measure, ME = 1, RMSE = 2, MAE = 3)]
cont_min <- min(res$errors)
cont_max <- max(res$errors)
cont_max_fix <- (cont_max - (cont_max - cont_min) * 0.4)
num_contours <- 18

# minimums <- head(sort.int(res$errors, index.return = TRUE)$ix , 5)
minimums <- sort.int(res$errors, index.return = TRUE)$ix

x_minims <- ((minimums - 1) %% length(ks)) + 1
y_minims <- ceiling(minimums/length(ks))


# Data for residuals
residuals_matrix <- matrix(nrow = 5, ncol = length(y_err))
residuals_matrix[1, ] <- y_err - optimal$mean
residuals_matrix[2, ] <- y_err - naive

# Data for errors table
names_col <- c("Optimal", "Naive", "Seasonal Naive", "", "", "")
optimal_train_error <- accuracy(optimal_train, y_train_err)
optimal_test_error <- accuracy(optimal_test, y_test_err)
naive_train_error <- accuracy(naive[1:length(y_train_err)], y_train_err)
naive_test_error <- accuracy(naive[(length(y_train_err) + 1):length(naive)], y_test_err)

naive_total_error <- naive_train_error[switch(error_measure, ME = 1, RMSE = 2, MAE = 3)]

errors_matrix <- matrix(nrow = 2, ncol = 10)
errors_matrix[1, ] <- c(optimal_train_error, optimal_test_error)
errors_matrix[2, ] <- c(naive_train_error, naive_test_error)

errors_matrix_tab1 <- errors_matrix
errors_matrix_tab2 <- errors_matrix

# Data for selected methods
selected_methods <- rep(FALSE, 5)

# Data for selected points in contour
selected_points <- matrix(rep(FALSE, NROW(res$errors) * NCOL(res$errors)), nrow = NROW(res$errors), ncol = NCOL(res$errors))
# selected_points_aux <<- selected_points
previous_countour <<- "default"

# Index of error type
# error_type <- switch(error_measure,
#                      ME = 1,
#                      RMSE = 2,
#                      MAE = 3,
#                      MPE = 4,
#                      MAPE = 5)
