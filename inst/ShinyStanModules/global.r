# call relevant packages
library(shiny)
library(bayesplot)
library(dplyr)
library(ggplot2)
library(shinyjs)
# sso <- get(".SHINYSTAN_OBJECT", envir = shinystan:::.sso_env) 

.inverse <- function(x) 1/x
.cloglog <- function(x) log(-log1p(-x))
.square <- function(x) x^2