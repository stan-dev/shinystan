library(shinystan)

context("launch options")

test_that("options set when package loads", {
  expect_that(getOption("shinystan.rstudio"), is_false())
  options(shinystan.rstudio = TRUE)
  expect_that(getOption("shinystan.rstudio"), is_true())
  options(shinystan.rstudio = FALSE)
})
