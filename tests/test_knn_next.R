source("../knn_next.R", chdir = TRUE)
require(testthat)

expect_equal_tolerance = .Machine$double.eps ^ 0.4

test_that("knn_next() for a univariate time series (length: 98)", {
	res = knn_next(LakeHuron, 3, 6)
	expect_equal(res$mean, 579.6878)
	expect_equal(res$neighbors, c(65, 67, 53))
})

test_that("knn_next() for a multivariate time series (length: 72)", {
	res = knn_next(matrix(AirPassengers, ncol = 2), 5, 2)
	expect_equal(res$mean, 224.3003, tolerance = expect_equal_tolerance)
	expect_equal(res$neighbors, c(9, 19, 11, 30, 21))
}) 

test_that("knn_next() for a univariate time series (length: 3177)", {
	res = knn_next(sunspot.month, 15, 12) 
	expect_equal(res$mean, 49.14888, tolerance = expect_equal_tolerance)
	expect_equal(res$neighbors, c(1581, 1299, 1811, 1802, 3007, 1573, 2645, 1535, 3043, 1165, 2647, 3044, 1935, 2641, 10))
}) 

test_that("knn_next() for a multivariate time series (length: 1059)", {
	res = knn_next(matrix(sunspot.month, ncol = 3), 5, 6)
	expect_equal(res$mean, 112.1015, tolerance = expect_equal_tolerance)
	expect_equal(res$neighbors, c(807, 1, 8, 9, 7))
})

