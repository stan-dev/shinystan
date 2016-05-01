library(shinystan)
library(rstanarm)

context("Creating sso")

stanreg <- stan_glm(mpg ~ wt, data = mtcars, seed = 12345, iter = 200)

array_test1 <- array(rnorm(300), dim = c(25, 4, 3))
array_test2 <- array(rnorm(300), dim = c(100, 3))

data(line, package = "coda")
mcmc_test1 <- line
mcmc_test2 <- line[[1L]]

chain1 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
chain2 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
chains_test <- list(chain1, chain2)


test_that("as.shinystan creates sso", {
  expect_is(as.shinystan(array_test1), "shinystan")
  expect_is(as.shinystan(mcmc_test1), "shinystan")
  expect_is(as.shinystan(chains_test), "shinystan")
  expect_is(as.shinystan(stanreg), "shinystan")
})

test_that("sso_check throws errors", {
  expect_error(sso_check(array_test1))
  expect_error(sso_check(chain2))
  expect_error(sso_check(chains_test))
  
  expect_true(sso_check(eight_schools))
  expect_true(sso_check(as.shinystan(array_test1)))
  expect_true(sso_check(as.shinystan(mcmc_test1)))
  expect_true(sso_check(as.shinystan(chains_test)))
})

test_that("is.shinystan works", {
  expect_true(is.shinystan(eight_schools))
  expect_false(is.shinystan(eight_schools@samps_all))
})

test_that("as.shinystan throws errors", {
  expect_error(as.shinystan(array_test2))
  expect_error(as.shinystan(mcmc_test2))
})

test_that("as.shinystan arguments works with rstanarm example", {
  sso1 <- as.shinystan(stanreg)
  sso2 <- as.shinystan(stanreg, ppd = FALSE)
  expect_is(sso1, "shinystan")
  expect_is(sso2, "shinystan")
  expect_false(is.null(sso1@misc$pp_check_plots))
  expect_null(sso2@misc$pp_check_plots)
})
