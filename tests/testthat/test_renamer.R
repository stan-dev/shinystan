library(shinyStan)
context("renaming shinystan objects")

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
