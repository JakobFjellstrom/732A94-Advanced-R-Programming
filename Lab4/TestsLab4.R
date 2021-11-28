library(testthat)

test_that("linreg rejects errounous inputs", {
  expect_error(linmod <- linreg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(linmod <- linreg$new(formula = Petal.Length + Sepal.Width, data = iris))
  expect_error(linmod <- linreg$new(formula = Petal.Length~Petal.Width, data = iris$Species))
  expect_error(linmod <- linreg$new(formula = Petal.Length~Petal.Width, data = data))  
})

test_that("correct class", {
  linmod <- linreg$new(formula = Petal.Length~Petal.Width, data = iris)
  expect_true(class(linmod)[1] == "linreg")
})
