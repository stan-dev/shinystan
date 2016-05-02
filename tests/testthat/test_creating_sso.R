library(shinystan)
suppressPackageStartupMessages(library(rstanarm))
library(coda)

sso <- eight_schools
stanreg1 <- suppressWarnings(stan_glm(mpg ~ wt, data = mtcars, seed = 12345, iter = 200, refresh = 0))
array1 <- array(rnorm(300), dim = c(25, 4, 3))
array2 <- array(rnorm(300), dim = c(100, 3))
chains1 <- list(chain1 = cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100)), 
                chain2 = cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100)))

data(line, package = "coda")
mcmc1 <- line
mcmc2 <- line[[1L]]

context("Creating and testing sso")

test_that("sso_check throws errors", {
  expect_error(sso_check(array1))
  expect_error(sso_check(chain2))
  expect_error(sso_check(chains1))
  
  expect_true(sso_check(sso))
  expect_true(sso_check(as.shinystan(array1)))
})


test_that("is.shinystan, is.stanfit, is.stanreg work", {
  expect_true(is.shinystan(sso))
  expect_false(is.shinystan(sso@samps_all))
  
  expect_true(is.stanfit(stanreg1$stanfit))
  expect_false(is.stanfit(stanreg1))
  
  expect_true(is.stanreg(stanreg1))
  expect_false(is.stanreg(stanreg1$stanfit))
})


test_that("as.shinystan creates sso", {
  # array
  expect_is(as.shinystan(array1, model_name = "test", note = "test"), "shinystan")
  
  # mcmc.list
  expect_is(as.shinystan(mcmc1, model_name = "test", note = "test", model_code = "test"), "shinystan")
  expect_is(as.shinystan(mcmc1[1]), "shinystan")
  
  # list of matrices
  expect_is(as.shinystan(chains1, model_code = "test"), "shinystan")
  expect_is(as.shinystan(chains1[1]), "shinystan")
  colnames(chains1[[1]]) <- colnames(chains1[[2]]) <- c(paste0("beta[",1:2,"]"), "sigma")
  sso2 <- as.shinystan(chains1, param_dims = list(beta = 2, sigma = 0))
  expect_identical(sso2@param_dims, list(beta = 2, sigma = numeric(0)))
  
  # stanreg
  expect_is(as.shinystan(stanreg1, model_name = "test"), "shinystan")
  
  # stanfit
  expect_is(as.shinystan(stanreg1$stanfit, model_name = "test", note = "test"), "shinystan")
})

test_that("as.shinystan throws errors", {
  expect_error(as.shinystan(array2))
  expect_error(as.shinystan(mcmc2))
})

test_that("as.shinystan arguments works with rstanarm example", {
  sso1 <- as.shinystan(stanreg1)
  sso2 <- as.shinystan(stanreg1, ppd = FALSE)
  expect_is(sso1, "shinystan")
  expect_is(sso2, "shinystan")
  expect_false(is.null(sso1@misc$pp_check_plots))
  expect_null(sso2@misc$pp_check_plots)
})
