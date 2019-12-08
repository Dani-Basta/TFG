source("../knn.R", chdir = TRUE)
require(testthat)

test_data_dir = "data/knn"
expected_res_file_path_1 = paste0(test_data_dir, "/expected_res_1")
expected_res_file_path_2 = paste0(test_data_dir, "/expected_res_2")
expected_res_file_path_3 = paste0(test_data_dir, "/expected_res_3")
expected_res_file_path_4 = paste0(test_data_dir, "/expected_res_4")

test_that("knn() for a univariate time series (length: 98)", {
	res = knn(LakeHuron, 3, 6)
	expected_res = readRDS(expected_res_file_path_1)
	expect_equal(res, expected_res)
})

test_that("knn() for a multivariate time series (length: 72)", {
	res = knn(matrix(AirPassengers, ncol = 2), 1:10, 1:10)
	expected_res = readRDS(expected_res_file_path_2)
	expect_equal(res, expected_res)
})

test_that("knn() for a univariate time series (length: 3177)", {
	res = knn(sunspot.month, 1:15, 1:12, threads = 2)
	expected_res = readRDS(expected_res_file_path_3)
	expect_equal(res, expected_res)
}) 

test_that("knn() for a multivariate time series (length: 1059)", {
	res = knn(matrix(sunspot.month, ncol = 3), 1:5, 1:6)
	expected_res = readRDS(expected_res_file_path_4)
	expect_equal(res, expected_res)
})

