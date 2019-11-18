source("../knn_past.R", chdir = TRUE)
require(testthat)

test_data_dir = "data/knn_past"
expected_res_file_path_1 = paste0(test_data_dir, "/expected_res_1") 
expected_res_file_path_2 = paste0(test_data_dir, "/expected_res_2") 
expected_res_file_path_3 = paste0(test_data_dir, "/expected_res_3") 
expected_res_file_path_4 = paste0(test_data_dir, "/expected_res_4") 

test_that("knn_past() for a univariate time series (length: 98)", {
	res = knn_past(LakeHuron, 5, 5)
	expected_res = readRDS(expected_res_file_path_1) 
	expect_equal(res, expected_res)
})

test_that("knn_past() for a multivariate time series (length: 72)", {
	res = knn_past(matrix(AirPassengers, ncol = 2), 4, 3)
	expected_res = readRDS(expected_res_file_path_2) 
	expect_equal(res, expected_res)
}) 

test_that("knn_past() for a univariate time series (length: 3177)", {
	res = knn_past(sunspot.month, 14, 10) 
	expected_res = readRDS(expected_res_file_path_3) 
	expect_equal(res, expected_res)
}) 

test_that("knn_past() for a multivariate time series (length: 1059)", {
	res = knn_past(matrix(sunspot.month, ncol = 3), 6, 8)
	expected_res = readRDS(expected_res_file_path_4) 
	expect_equal(res, expected_res)
})

