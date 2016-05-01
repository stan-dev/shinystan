library(shinystan)

context("sso_info")

sso <- eight_schools

test_that("sso_info error checking", {
  expect_error(sso_info(sso@samps_all), "specify a shinystan object")
})


test_that("sso_info prints output", {
  expect_output(sso_info(sso), "sso")
  expect_output(sso_info(sso), "Model name: Demo")
  expect_output(sso_info(sso), "Parameters: 11")
  expect_output(sso_info(sso), "Chains: 4")
  expect_output(sso_info(sso), "Has model code: TRUE")
  expect_output(sso_info(sso), "Has user notes: FALSE")
})
