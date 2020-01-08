library(zoo)
library(forecast)
library(tseries)

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
# full_dates <- 1:(length(y)+1)

# y <- as.matrix(btcnWeekRend, ncols = 1)
# dates <- as.Date(read.csv("D:/Daniel/Investigación/BTC-EUR-weekly.csv")[,1])
# full_dates <- as.Date(ts( c(y,1), start = time(y)[1], frequency = frequency(y) ))

# y <- ts(get("rain"), start = "1914-01-01", frequency = 365)
# dates <- as.Date(time(y))
# full_dates <- as.Date(ts( c(y,1), start = time(y)[1], frequency = frequency(y) ))

# y <- forecast::taylor
# # y <- tail(y, length(y)*0.9)
# dates <- 1:length(y)
# full_dates <- c(dates, length(y) + 1)

n <- NROW(y)
train_init <- floor(n * 0.75)
test_init <- floor(n * 0.9)
y_train <- head(y, test_init) # [1:test_init]
distance <- "euclidean"
error_measure <- "RMSE"
weight <- "proportional"
n_threads <- 6
ks <-  1:85
ds <- 1:85
min_y <- min(y)
max_y <- max(y)

# Get errors matrix, and best k and d
res <- knn_param_search(y = y_train, k = ks, d = ds, initial = train_init, distance = distance, 
                 error_measure = error_measure, weight = weight, threads = n_threads)

# optimal_train <- knn_past(y = y_train, k = res$opt_k, d = res$opt_d, init = train_init, 
#                           distance_metric = distance, weight = weight, threads = n_threads)
# optimal_test <- knn_past(y = y, k = res$opt_k, d = res$opt_d, init = test_init, 
#                          distance_metric = distance, weight = weight, threads = n_threads)
# optimal <- c(optimal_train$mean, optimal_test$mean)

optimal <- knn_past(y = y, k = res$opt_k, d = res$opt_d, initial = train_init, 
                    distance = distance, weight = weight, threads = n_threads)
optimal_train <- head(optimal$fitted, test_init - train_init )
optimal_test <- tail(optimal$fitted, n - test_init )

y_err <- tail(y, (n - train_init )) # ts(y[(train_init + 1):n]) #  
y_train_err <-  y[(train_init + 1):test_init] # tail( head(y, test_init), (test_init - train_init)) #
y_test_err <- y[(test_init + 1):n] #  tail(y, (n - test_init - 1) ) #
sub_dates <- tail(dates, length(y) - train_init)
# sub_dates <- c(as.Date(time(optimal_train$mean)), as.Date(time(optimal_test$mean))) 
# sub_dates <- as.Date(time(optimal)) 


naive <- y[train_init:(n - 1)] # ts(y[train_init:(n - 1)], start = time(y)[train_init + 1], frequency = frequency(y)) #
# naive_total_error <- accuracy(naive, y_err) [switch(error_measure, ME = 1, RMSE = 2, MAE = 3)]

# sesPred <- c(y_train_err, y_test_err) - tail(tsCV(y = y, ses, h = 1, initial = train_init, window = 1), n - train_init)
# etsPred <- y[(train_init + 1):n] - na.remove(tsCV(y = y, stlf, h = 1, initial = train_init - 1))
# attr(sesPred, "na.removed") <- NULL

# stlfPred <- y[(train_init + 1):n] - na.remove(tsCV(y = y, stlf, h = 1, initial = train_init - 1))
# attr(stlfPred, "na.removed") <- NULL

# stlfPred <- rep(1, length(length(naive)))
# for (i in 1:(n-train_init)) {
#     # aux[i] <- ses(y = head(y, (train_init+i-1)), h = 1)$mean
#     stlfPred[i] <- forecast::stlf(y = head(y, (train_init+i-1)), h = 1)$mean
# }

# etsPred <- rep(1, length(naive))
# for (i in 1:(n - train_init)) {
#     # aux[i] <- ses(y = head(y, (train_init+i-1)), h = 1)$mean
#     etsPred[i] <- forecast(forecast::ets(y = head(y, (train_init + i - 1))), h = 1)$mean
# }
# print("la diferencia es:")
# print(c(y_train_err, y_test_err) - aux)

# seasnaiPred <- c(y_train_err, y_test_err) - head(tail(tsCV(y = y, snaive, h = 1, initial = train_init - 1), length(optimal$mean)), n - train_init - 1)
# seasnaiPred <- y[(train_init + 1):(n)] - na.remove(tsCV(y = y, snaive, h = 1, initial = train_init - 1))
# attr(seasnaiPred, "na.removed") <- NULL

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
residuals_matrix[1, ] <- y_err - optimal$fitted
residuals_matrix[2, ] <- y_err - naive

# Data for errors table
names_col <- c("Optimal", "Naive", "Seasonal Naive", "", "")

optimal_train_error <- accuracy(optimal_train, y)
optimal_test_error  <- accuracy(optimal_test, y)

naive_train_error <- accuracy(naive[1:length(y_train_err)], y_train_err)
naive_test_error  <- accuracy(naive[(length(y_train_err) + 1):length(naive)], y_test_err)

# ets_train_error <- accuracy(etsPred[1:length(y_train_err)], y_train_err)
# ets_test_error  <- accuracy(etsPred[(length(y_train_err) + 1):length(naive)], y_test_err)
# 
# snai_train_error <- accuracy(seasnaiPred[1:length(y_train_err)], y_train_err)
# snai_test_error  <- accuracy(seasnaiPred[(length(y_train_err) + 1):length(naive)], y_test_err)

# stlf_train_error <- accuracy(stlfPred[1:length(y_train_err)], y_train_err)
# stlf_test_error  <- accuracy(stlfPred[(length(y_train_err) + 1):length(naive)], y_test_err)


naive_total_error <- naive_train_error[switch(error_measure, ME = 1, RMSE = 2, MAE = 3)]

errors_matrix <- matrix(nrow = 2, ncol = 10)
colnames(errors_matrix) <- c("Train-ME", "RMSE", "MAE", "MPE", "MAPE", "Test-ME", "RMSE", "MAE", "MPE", "MAPE")
rownames(errors_matrix) <- c("kNN", "Naive") #, "ets", "SeasNai")
errors_matrix[1, ] <- c(optimal_train_error[1:5], optimal_test_error[1:5])
errors_matrix[2, ] <- c(naive_train_error[1:5], naive_test_error[1:5])
# errors_matrix[3, ] <- c(ets_train_error[1:5], ets_test_error[1:5])
# errors_matrix[4, ] <- c(snai_train_error[1:5], snai_test_error[1:5])
# errors_matrix[5, ] <- c(stlf_train_error[1:5], stlf_test_error[1:5])
# print(errors_matrix)

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
