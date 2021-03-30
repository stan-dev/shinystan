library(shinystan)
library(rstanarm)
context("Misc")

test_that("options set when package loads", {
  expect_false(getOption("shinystan.rstudio"))
  options(shinystan.rstudio = TRUE)
  expect_true(getOption("shinystan.rstudio"))
  options(shinystan.rstudio = FALSE)
})

test_that("Can work with CSV files created by RStan", {
  skip_if_not_installed("rstanarm")
  test_data <- data.frame(Y = rnorm(10))
  samples_tmp <- tempfile()
  fit <- rstanarm::stan_glm(Y ~ 1, data = test_data, sample_file = samples_tmp, chains = 1, iter = 10, sample_file = samples_tmp)
  fit_csv <- read_stan_csv(samples_tmp)

  expect_identical(.stan_algorithm(fit_csv), .stan_algorithm(fit$stanfit))
  expect_identical(.stan_method(fit_csv), .stan_method(fit$stanfit))
})
  
