library(shinystan)
suppressPackageStartupMessages(library(rstanarm))
library(coda)

sso <- eight_schools
array1 <- array(rnorm(300), dim = c(25, 4, 3))
array2 <- array(rnorm(300), dim = c(100, 3))
chains1 <- list(chain1 = cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100)), 
                chain2 = cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100)))

data(line, package = "coda")
mcmc1 <- line
mcmc2 <- line[[1L]]

stanreg1 <- suppressWarnings(stan_glm(mpg ~ wt, data = mtcars, seed = 12345, iter = 200, refresh = 0))
stanfit1 <- stanreg1$stanfit

# load 'old_sso', a shinystan object created by previous shinystan version
load("old_sso_for_tests.rda")

context("Checking shinystan objects")
# sso_check ---------------------------------------------------------------
test_that("sso_check throws errors", {
  expect_error(sso_check(array1))
  expect_error(sso_check(chain2))
  expect_error(sso_check(chains1))
  
  expect_true(sso_check(sso))
  expect_true(sso_check(as.shinystan(array1)))
  
  expect_error(sso_check(old_sso), 
               regexp = "use the 'update_sso' function to update your object")
})


# is.shinystan ------------------------------------------------------------
test_that("is.shinystan, is.stanfit, is.stanreg work", {
  expect_true(is.shinystan(sso))
  expect_false(is.shinystan(sso@posterior_sample))
  
  expect_true(is.stanfit(stanfit1))
  expect_false(is.stanfit(stanreg1))
  
  expect_true(is.stanreg(stanreg1))
  expect_false(is.stanreg(stanfit1))
})


context("Creating shinystan objects")
# as.shinystan helpers ----------------------------------------------------
test_that("as.shinystan stanfit helpers work", {
  expect_is(.rstan_max_treedepth(stanfit1), "integer")
  expect_equal(.rstan_warmup(stanfit1), 0)
  expect_equal(length(.rstan_sampler_params(stanfit1)), ncol(stanfit1))
  expect_is(.rstan_summary(stanfit1), "matrix")
  expect_identical(.stan_algorithm(stanfit1), "NUTS")
  expect_false(.used_vb(stanfit1))
  expect_false(.from_cmdstan(stanfit1))
  expect_is(.stan_args(stanfit1), "list")
  expect_true(all(c("iter", "seed", "warmup") %in% names(.stan_args(stanfit1))))
  
  stanfit1@stan_args[[1]]$method <- "variational"
  expect_true(.used_vb(stanfit1))
  expect_identical(.rstan_sampler_params(stanfit1), list(NA))
  
  stanfit1@stan_args[[1]]$control$max_treedepth <- NULL
  expect_equal(.rstan_max_treedepth(stanfit1), 11)
})



# as.shinystan ------------------------------------------------------------
test_that("as.shinystan creates sso", {
  # array
  expect_is(x <- as.shinystan(array1, model_name = "test", note = "test"), "shinystan")
  expect_identical(sso_version(x), utils::packageVersion("shinystan"))
  
  # mcmc.list
  expect_is(as.shinystan(mcmc1, model_name = "test", note = "test", model_code = "test"), "shinystan")
  expect_is(as.shinystan(mcmc1[1]), "shinystan")
  expect_identical(sso_version(x), utils::packageVersion("shinystan"))
  
  # list of matrices
  expect_is(as.shinystan(chains1, model_code = "test"), "shinystan")
  expect_is(as.shinystan(chains1[1]), "shinystan")
  colnames(chains1[[1]]) <- colnames(chains1[[2]]) <- c(paste0("beta[",1:2,"]"), "sigma")
  sso2 <- as.shinystan(chains1, param_dims = list(beta = 2, sigma = 0))
  expect_identical(sso2@param_dims, list(beta = 2, sigma = numeric(0)))
  expect_identical(sso_version(x), utils::packageVersion("shinystan"))
  
  # stanreg
  expect_is(as.shinystan(stanreg1, model_name = "test"), "shinystan")
  expect_identical(sso_version(x), utils::packageVersion("shinystan"))
  
  # stanfit
  expect_is(as.shinystan(stanfit1, model_name = "test", note = "test"), "shinystan")
  expect_identical(sso_version(x), utils::packageVersion("shinystan"))
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

test_that("as.shinystan 'pars' argument works with rstan example", {
  # load 'stanfit2' saved stanfit object
  load("stanfit2_for_tests.rda")
  
  expect_error(as.shinystan(stanfit2, pars = c("alpha[1,1]", "lp__")), 
               "elements of non-scalar parameters not allowed")
  
  sso0 <- as.shinystan(stanfit2)
  sso1 <- as.shinystan(stanfit2, pars = "alpha")
  sso2 <- as.shinystan(stanfit2, pars = "beta")
  sso3 <- as.shinystan(stanfit2, pars = c("alpha", "beta"))
  
  expect_identical(sso0, sso3)

  sso1names <- c("alpha[1,1]", "alpha[2,1]", "alpha[1,2]", "alpha[2,2]",
                 "alpha[1,3]", "alpha[2,3]", "log-posterior")
  expect_identical(sso1@param_names, sso1names)
  expect_identical(rownames(sso1@summary), sort(sso1names))
  expect_identical(sso2@param_names, c("beta", "log-posterior"))
  expect_identical(rownames(sso2@summary), c("beta", "log-posterior"))
  
  expect_equal(dim(sso1@posterior_sample), c(200, 2, 7))
  expect_equal(dim(sso2@posterior_sample), c(200, 2, 2))
})


# update_sso ---------------------------------------------------------------
context("Updating shinystan objects")
test_that("update_sso errors and messages are correct", {
  expect_error(update_sso(1234))
  expect_message(sso2 <- update_sso(sso), "already up-to-date")
  expect_is(sso2, "shinystan")
  
  expect_message(sso3 <- update_sso(old_sso), "object updated")
  expect_is(sso3, "shinystan")
  expect_identical(sso_version(sso3), utils::packageVersion("shinystan"))
  
  sso3@misc[["sso_version"]] <- "2.9.5"
  expect_error(update_sso(sso3), 
               regexp = "was created using a more recent version of shinystan")
})
