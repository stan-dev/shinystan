library(shinystan)

context("Creating sso")

array_test1 <- array(rnorm(300), dim = c(25, 4, 3))
array_test2 <- array(rnorm(300), dim = c(100, 3))

data(line, package = "coda")
mcmc_test1 <- line
mcmc_test2 <- line[[1L]]

chain1 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
chain2 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
chains_test <- list(chain1, chain2)


test_that("as.shinystan creates sso", {
  expect_that(is.shinystan(as.shinystan(array_test1)), is_true())
  expect_that(is.shinystan(as.shinystan(mcmc_test1)), is_true())
  expect_that(is.shinystan(as.shinystan(chains_test)), is_true())
})

test_that("sso_check throws errors", {
  expect_that(sso_check(array_test1), throws_error())
  expect_that(sso_check(chain2), throws_error())
  expect_that(sso_check(chains_test), throws_error())
  expect_that(sso_check(eight_schools), is_true())
  expect_that(sso_check(as.shinystan(array_test1)), is_true())
  expect_that(sso_check(as.shinystan(mcmc_test1)), is_true())
  expect_that(sso_check(as.shinystan(chains_test)), is_true())
})

test_that("as.shinystan throws errors", {
  expect_that(as.shinystan(array_test2), throws_error())
  expect_that(as.shinystan(mcmc_test2), throws_error())
})
