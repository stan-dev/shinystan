library(shinystan)
context("Deploying")

sso <- eight_schools

test_that("deploy_shinystan error checking works", {
  expect_error(deploy_shinystan(sso@posterior_sample), 
               regexp = "specify a shinystan object")
  expect_error(deploy_shinystan(sso), 
               regexp = "'appName' is required")
})

test_that("deploy_shinystan preprocessing doesn't error", {
  expect_silent(deploy_test <- deploy_shinystan(sso, appName = "test", deploy = FALSE))
  expect_true(grepl("shinystan", deploy_test, ignore.case = TRUE))
  expect_true(dir.exists(deploy_test))
})

test_that("deploy_shinystan pp_check processing functions ok", {
  x <- "123454321"
  expect_output(cat(.y_lines(x)), x)
  expect_output(cat(.yrep_lines(x)), x)
  
  deploy_pp_test <- deploy_shinystan(sso, appName = "test", deploy = FALSE, 
                                     ppcheck_data = rep(1, 8), 
                                     ppcheck_yrep = "yrep")
  expect_true(dir.exists(deploy_pp_test))
})

