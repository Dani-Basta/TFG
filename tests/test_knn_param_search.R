source("../knn_param_search.R", chdir = TRUE)
require(testthat)

test_data_dir = "data/knn_param_search"
expected_res_file_path_1 = paste0(test_data_dir, "/expected_res_1") 
expected_res_file_path_2 = paste0(test_data_dir, "/expected_res_2") 
expected_res_file_path_3 = paste0(test_data_dir, "/expected_res_3") 
expected_res_file_path_4 = paste0(test_data_dir, "/expected_res_4") 

test_that("knn_param_search() for a univariate time series (length: 98)", {
	res = knn_param_search(LakeHuron, 1:5, 1:5)
	expected_res = readRDS(expected_res_file_path_1) 
	expect_equal(res, expected_res)
})

test_that("knn_param_search() for a multivariate time series (length: 72)", {
	res = knn_param_search(matrix(AirPassengers, ncol = 2), 1:4, 1:3, weight = "linear")
	expected_res = readRDS(expected_res_file_path_2) 
	expect_equal(res, expected_res)
}) 

test_that("knn_param_search() for a univariate time series (length: 3177)", {
	res = knn_param_search(sunspot.month, 1:14, 1:10, error_measure = "RMSE", threads = 3) 
	expected_res = readRDS(expected_res_file_path_3) 
	expect_equal(res, expected_res)
}) 

test_that("knn_param_search() for a multivariate time series (length: 1059)", {
	res = knn_param_search(matrix(sunspot.month, ncol = 3), 1:10, 5:15, distance = "manhattan", threads = 2)
	expected_res = readRDS(expected_res_file_path_4) 
	expect_equal(res, expected_res)
})

