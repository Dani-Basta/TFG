source("../knn_forecast.R", chdir = TRUE)
require(testthat)

test_data_dir = "data/knn_forecast"
expected_res_file_path_1 = paste0(test_data_dir, "/expected_res_1") 
expected_res_file_path_2 = paste0(test_data_dir, "/expected_res_2") 
expected_res_file_path_3 = paste0(test_data_dir, "/expected_res_3") 
expected_res_file_path_4 = paste0(test_data_dir, "/expected_res_4") 

test_that("knn_forecast() for a univariate time series (length: 98)", {
	res = knn_forecast(LakeHuron, 3, 6)
	expected_res = readRDS(expected_res_file_path_1) 
	expect_equal(res, expected_res)
})

test_that("knn_forecast() for a multivariate time series (length: 72)", {
	res = knn_forecast(matrix(AirPassengers, ncol = 2), 5, 2)
	expected_res = readRDS(expected_res_file_path_2) 
	expect_equal(res, expected_res)
}) 

test_that("knn_forecast() for a univariate time series (length: 3177)", {
	res = knn_forecast(sunspot.month, 15, 12) 
	expected_res = readRDS(expected_res_file_path_3) 
	expect_equal(res, expected_res)
}) 

test_that("knn_forecast() for a multivariate time series (length: 1059)", {
	res = knn_forecast(matrix(sunspot.month, ncol = 3), 5, 6)
	expected_res = readRDS(expected_res_file_path_4) 
	expect_equal(res, expected_res)
})

