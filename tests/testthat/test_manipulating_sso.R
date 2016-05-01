library(shinystan)

context("Working with sso")

source("data_for_tests.R")
sso <- eight_schools
sso_msg <- "specify a shinystan object"
not_sso <- sso@model_name

test_that("simple sso functions work", {
  expect_error(rename_model(not_sso), sso_msg)
  expect_error(model_code(not_sso), sso_msg)
  expect_error(notes(not_sso), sso_msg)
  
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

test_that("retrieve works", {
  expect_error(retrieve(not_sso), sso_msg)
  expect_error(retrieve(not_sso, what = "mean"), sso_msg)
  
  whats <- c("median", "mean", "rhat", "ess", "sd")
  for (what in whats) {
    expect_equal(retrieve(sso, what), get(paste0("demo_",what)))
  }
})

test_that("generate_quantity works", {
  expect_error(generate_quantity(not_sso), sso_msg)
  
  sso2 <- generate_quantity(sso, fun = function(x) x^2,
                           param1 = "tau", new_name = "tau_sq")
  expect_equivalent(sso2@samps_all[,, "tau_sq", drop=FALSE], 
                    sso@samps_all[,, "tau", drop=FALSE]^2)
  
  sso2 <- generate_quantity(sso, fun = "-",
                           param1 = "theta[1]", param2 = "theta[2]",
                           new_name = "theta1minus2")
  expect_equivalent(sso2@samps_all[,, "theta1minus2", drop=FALSE], 
                    sso@samps_all[,, "theta[1]", drop=FALSE] - 
                      sso@samps_all[,, "theta[2]", drop=FALSE])
})

test_that("drop_parameters works", {
  pn <- sso@param_names
  pd <- sso@param_dims
  s <- sso@summary
  samp <- sso@samps_all
  
  expect_error(drop_parameters(not_sso, pars = "mu"), sso_msg)
  
  sso2 <- drop_parameters(sso, pars = "mu")
  expect_identical(sso2@param_names, pn[pn != "mu"])
  expect_identical(sso2@param_dims, pd[names(pd) != "mu"])
  expect_identical(sso2@summary, s[rownames(s) != "mu", ])
  expect_identical(sso2@samps_all, samp[,, dimnames(samp)[[3]] != "mu"])
  
  sso2 <- drop_parameters(sso, pars = "theta")
  expect_identical(sso2@param_names, grep("theta", pn, value = TRUE, invert = TRUE))
  expect_identical(sso2@param_dims, pd[names(pd) != "theta"])
  tmp <- s[grep("theta", rownames(s), value = TRUE, invert = TRUE), ]
  expect_identical(sso2@summary, tmp)
  tmp <- samp[,, grep("theta", dimnames(samp)[[3]], value = TRUE, invert = TRUE)]
  expect_identical(sso2@samps_all, tmp)
  
  sso2 <- drop_parameters(sso, pars = c("theta", "log-posterior"))
  tmp <- grep("theta|log-posterior", pn, value = TRUE, invert = TRUE)
  expect_identical(sso2@param_names, tmp)
  tmp <- pd[grep("theta|log-posterior", names(pd), value = TRUE, invert = TRUE)]
  expect_identical(sso2@param_dims, tmp)
  tmp <- s[grep("theta|log-posterior", rownames(s), value = TRUE, invert = TRUE), ]
  expect_identical(sso2@summary, tmp)
  tmp <- samp[,, grep("theta|log-posterior", dimnames(samp)[[3]], value = TRUE, invert = TRUE)]
  expect_identical(sso2@samps_all, tmp)
  
  
  expect_error(drop_parameters(sso, pars = c("theta[1]", "mu")), 
               regexp = "Individual elements")
  expect_error(drop_parameters(sso, pars = "omega"), regexp = "No matches")
  expect_warning(drop_parameters(sso, pars = c("mu", "omega")), 
                 regexp = "not found and ignored: omega")
})
