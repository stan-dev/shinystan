library(shinyStan)
context("shinystan objects")

array_test <- array(rnorm(300), dim = c(25, 4, 3))
array_test <- as.shinystan(array_test)

data(line, package = "coda")
mcmc_test <- as.shinystan(line)

chain1 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
chain2 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
chains_test <- as.shinystan(list(chain1, chain2))

test_that("as.shinystan creates sso", {
  expect_true(is.shinystan(array_test))
  expect_true(is.shinystan(mcmc_test))
  expect_true(is.shinystan(chains_test))
})

