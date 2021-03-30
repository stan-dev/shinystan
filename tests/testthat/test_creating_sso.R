library(shinystan)
library(coda)
suppressPackageStartupMessages(library(rstan))

sso <- eight_schools
array1 <- array(rnorm(300), dim = c(25, 4, 3))
array2 <- array(rnorm(300), dim = c(100, 3))
chains1 <- list(chain1 = cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100)),
                chain2 = cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100)))

data(line, package = "coda")
mcmc1 <- line
mcmc2 <- line[[1L]]

if (requireNamespace("rstanarm", quietly = TRUE)) {
  suppressPackageStartupMessages(library(rstanarm))
  suppressWarnings(capture.output(
    stanreg1 <- stan_glmer(mpg ~ wt + (1 + wt|cyl), data = mtcars,
                           seed = 12345, iter = 200, chains = 2, refresh = 0)
  ))
  stanfit1 <- stanreg1$stanfit
}



context("Checking shinystan objects")
# sso_check ---------------------------------------------------------------
test_that("sso_check throws errors", {
  expect_error(sso_check(array1))
  expect_error(sso_check(chain2))
  expect_error(sso_check(chains1))

  expect_true(sso_check(sso))
  expect_true(sso_check(as.shinystan(array1)))

})


# is.shinystan ------------------------------------------------------------
test_that("is.shinystan, is.stanfit, is.stanreg work", {
  skip_if_not_installed("rstanarm")
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
  skip_if_not_installed("rstanarm")
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
  expect_equal(.rstan_max_treedepth(stanfit1), 10)
})


# as.shinystan ------------------------------------------------------------
test_that("as.shinystan (array) creates sso", {
  expect_s4_class(x <- as.shinystan(array1, model_name = "test", note = "test"), "shinystan")
  expect_identical(sso_version(x), utils::packageVersion("shinystan"))
  expect_equal(x@model_name, "test")
  expect_equal(x@user_model_info, "test")

  # with sampler_params
  samp <- sso@posterior_sample
  sp <- sso@sampler_params
  x <- as.shinystan(samp, sampler_params = sp, warmup = 759,
                    max_treedepth = 14, stan_algorithm = "NUTS")
  expect_s4_class(x, "shinystan")
  expect_equal(x@n_warmup, 759)
  expect_equal(x@n_chain, dim(samp)[2])
  expect_equal(x@n_iter, dim(samp)[1])
  expect_equivalent(x@posterior_sample, samp)
  expect_equal(x@misc$max_td, 14)
  expect_equal(x@misc$stan_algorithm, "NUTS")
})
test_that("as.shinystan (mcmc.list) creates sso", {
  expect_is(x <- as.shinystan(mcmc1, model_name = "test", note = "test", model_code = "test"), "shinystan")
  expect_is(as.shinystan(mcmc1[1]), "shinystan")
  expect_identical(sso_version(x), utils::packageVersion("shinystan"))
})
test_that("as.shinystan (list of matrices) creates sso", {
  expect_is(x <- as.shinystan(chains1, model_code = "test"), "shinystan")
  expect_is(as.shinystan(chains1[1]), "shinystan")
  colnames(chains1[[1]]) <- colnames(chains1[[2]]) <- c(paste0("beta[",1:2,"]"), "sigma")
  sso2 <- as.shinystan(chains1, param_dims = list(beta = 2, sigma = 0))
  expect_identical(sso2@param_dims, list(beta = 2, sigma = numeric(0)))
  expect_identical(sso_version(x), utils::packageVersion("shinystan"))

  # with sampler_params
  samp_list <- list()
  samp <- sso@posterior_sample
  for (j in 1:ncol(samp)) samp_list[[j]] <- samp[, j, ]
  sp <- sso@sampler_params
  x <- as.shinystan(samp_list, sampler_params = sp, warmup = 1000,
                    max_treedepth = 11, stan_algorithm = "NUTS")
  expect_s4_class(x, "shinystan")
})
test_that("as.shinystan (stanreg) creates sso", {
  skip_if_not_installed("rstanarm")
  x <- as.shinystan(stanreg1, model_name = "test")
  expect_is(x, "shinystan")

  # check that ppc plots created
  # ppc <- x@misc$pp_check_plots
  # expect_type(ppc, "list")
  # expect_s3_class(ppc[[1]], "ggplot")
  #
  # # without ppd
  # x <- as.shinystan(stanreg1, ppd = FALSE)
  # expect_null(x@misc$pp_check_plots)
})
test_that("as.shinystan (stanfit) creates sso", {
  skip_if_not_installed("rstanarm")
  expect_is(x <- as.shinystan(stanfit1, model_name = "test", note = "test"), "shinystan")
  expect_identical(sso_version(x), utils::packageVersion("shinystan"))
})

test_that("as.shinystan throws errors", {
  expect_error(as.shinystan(array2))
  expect_error(as.shinystan(mcmc2))
})

test_that("as.shinystan arguments works with rstanarm example", {
  skip_if_not_installed("rstanarm")
  sso1 <- as.shinystan(stanreg1)
  sso2 <- as.shinystan(stanreg1, ppd = FALSE)
  expect_is(sso1, "shinystan")
  expect_is(sso2, "shinystan")
})


test_that("as.shinystan works with CmdStanMCMC objects", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  fit <- try(cmdstanr::cmdstanr_example("schools", save_warmup = TRUE, iter_warmup = 500, chains = 2))
  if (!inherits(fit, "try-error")) {
    sso <- as.shinystan(fit)
    expect_s4_class(sso, "shinystan")
    expect_equal(sso@model_name, "schools")
    expect_equal(sso@param_names, c("log-posterior", "mu", "tau", paste0("theta[", 1:8, "]")))
    expect_equal(sso@n_chain, 2)
    expect_equal(sso@n_warmup, 500)
  }
})


test_that("as.shinystan works with CmdStanVB objects", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  fit <- try(cmdstanr::cmdstanr_example("logistic", method = "variational"))
  if (!inherits(fit, "try-error")) {
    sso <- as.shinystan(fit)
    expect_s4_class(sso, "shinystan")
    expect_equal(sso@model_name, "logistic")
    expect_equal(sso@n_chain, 1)
    expect_equal(sso@n_warmup, 0)
  }
})
