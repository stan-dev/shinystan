library(shinystan)

context("Deploying")

sso <- eight_schools

test_that("deploy_shinystan error checking works", {
  expect_error(deploy_shinystan(sso@samps_all), 
               regexp = "specify a shinystan object")
  expect_error(deploy_shinystan(sso, ), 
               regexp = "specify a name")
})

test_that("deploy_shinystan preprocessing doesn't error", {
  expect_silent(deploy_test <- deploy_shinystan(sso, appName = "test", deploy = FALSE))
  expect_true(grepl("shinystan", deploy_test, ignore.case = TRUE))
  expect_true(dir.exists(deploy_test))
})
