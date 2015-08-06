library(shinystan)

context("Working with sso")

source("data_for_tests.R")
sso <- eight_schools

test_that("retrieve works", {
  whats <- c("median", "mean", "rhat", "ess", "sd")
  for (what in whats) {
    expect_equal(retrieve(sso, what), get(paste0("demo_",what)))
  }
})
test_that("simple sso functions work", {
  sso2 <- rename_model(sso, "test_rename")
  expect_identical(sso2@model_name, "test_rename")
  
  sso2 <- model_code(sso, "test_code")
  expect_identical(model_code(sso2), "test_code")
  expect_identical(model_code(sso2), slot(sso2, "model_code"))
  
  sso2 <- notes(sso, "test_notes_replace", replace = TRUE)
  expect_identical(slot(sso2, "user_model_info"), "test_notes_replace")
  sso2 <- notes(sso2, "test_notes_keep", replace = FALSE)
  expect_identical(slot(sso2, "user_model_info"), notes(sso2))
})

