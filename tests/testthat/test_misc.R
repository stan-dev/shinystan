library(shinystan)
context("misc")

test_that("options set when package loads", {
  expect_false(getOption("shinystan.rstudio"))
  options(shinystan.rstudio = TRUE)
  expect_true(getOption("shinystan.rstudio"))
  options(shinystan.rstudio = FALSE)
})
