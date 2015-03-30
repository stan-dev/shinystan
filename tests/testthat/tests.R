library(shinyStan)


context("creating shinystan objects")

array_test1 <- array(rnorm(300), dim = c(25, 4, 3))
array_test2 <- array(rnorm(300), dim = c(100, 3))

data(line, package = "coda")
mcmc_test1 <- line
mcmc_test2 <- line[[1]]

chain1 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
chain2 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
chains_test <- list(chain1, chain2)


test_that("as.shinystan creates sso", {
  expect_that(is.shinystan(as.shinystan(array_test1)), is_true())
  expect_that(is.shinystan(as.shinystan(mcmc_test1)), is_true())
  expect_that(is.shinystan(as.shinystan(chains_test)), is_true())
})

test_that("sso_check throws errors", {
  expect_that(sso_check(array_test1), throws_error())
  expect_that(sso_check(chain2), throws_error())
  expect_that(sso_check(chains_test), throws_error())
  expect_that(sso_check(eight_schools), is_true())
  expect_that(sso_check(as.shinystan(array_test1)), is_true())
  expect_that(sso_check(as.shinystan(mcmc_test1)), is_true())
  expect_that(sso_check(as.shinystan(chains_test)), is_true())
})

test_that("as.shinystan throws errors", {
  expect_that(as.shinystan(array_test2), throws_error())
  expect_that(as.shinystan(mcmc_test2), throws_error())
})



context("naming shinystan objects")

out_name <- "test_shinystan"
sso_names1 <- c("test_shinystan")
sso_names2 <- c("test_shinystan", "test_shinystan.1")
sso_names9 <- c("test_shinystan", paste0("test_shinystan.",1:9))
new_name1 <- rename_sso(out_name, sso_names1)
new_name2 <- rename_sso(out_name, sso_names2)
new_name9 <- rename_sso(out_name, sso_names9)

test_that("rename_sso finds correct name", {
  expect_that(new_name1, equals("test_shinystan.1"))
  expect_that(new_name2, equals("test_shinystan.2"))
  expect_that(new_name9, equals("test_shinystan.10"))
})