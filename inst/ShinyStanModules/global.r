# call relevant packages
library(shiny)
library(shinydashboard)
library(rstan)
library(bayesplot)
library(dplyr)

sso <- get(".SHINYSTAN_OBJECT", envir = shinystan:::.sso_env) 