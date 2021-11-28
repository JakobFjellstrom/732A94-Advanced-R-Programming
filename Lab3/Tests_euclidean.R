library(testthat)

test_that("euclidean rejects wrong inputs", {
  expect_error(euclidean(1,"a"))
  expect_error(euclidean("a",2))
  expect_error(euclidean(1,a))
})

test_that("euclidean returns correct answer", {
  expect_equal(euclidean(1071, 462), 21)
  expect_equal(euclidean(210, 45), 15)
})
