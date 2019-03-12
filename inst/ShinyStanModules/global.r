# call relevant packages
library(shiny)
library(shinydashboard)
library(rstan)
library(bayesplot)
library(dplyr)

# load relevant modules

# home tab
source("MODULES/HOME/homepage.r")
source("MODULES/HOME/warnings.r")

# diagnoses tab
source("MODULES/DIAGNOSE/diagnoseHomepage.r")

source("MODULES/DIAGNOSE/divergentScatter.r")
source("MODULES/DIAGNOSE/divergentTransitions.r")
source("MODULES/DIAGNOSE/energy.r")
source("MODULES/DIAGNOSE/treedepth.r")
source("MODULES/DIAGNOSE/stepSize.r")
source("MODULES/DIAGNOSE/parallelCoordinates.r")
source("MODULES/DIAGNOSE/pairs.r")
source("MODULES/DIAGNOSE/acceptance.r")

source("MODULES/DIAGNOSE/tracePlot.r")
source("MODULES/DIAGNOSE/rhat_n_eff_se_mean.r")
source("MODULES/DIAGNOSE/autoCorrelation.r")

source("MODULES/DIAGNOSE/statsTableHMC.r")
source("MODULES/DIAGNOSE/rhat_n_eff_se_mean_stats.r")
source("MODULES/DIAGNOSE/autoCorrelationStats.r")

# estimate tab
source("MODULES/ESTIMATE/estimateHomepage.r")

source("MODULES/ESTIMATE/visualEstimate.r")
source("MODULES/ESTIMATE/scatterPlot.r")
source("MODULES/ESTIMATE/densityPlot.r")
source("MODULES/ESTIMATE/histogramPlot.r")
source("MODULES/ESTIMATE/intervalsPlot.r")
source("MODULES/ESTIMATE/areasPlot.r")

source("MODULES/ESTIMATE/numericalEstimate.r")
source("MODULES/ESTIMATE/summaryTable.r")

# more tab
source("MODULES/MORE/about.r")
source("MODULES/MORE/modelCode.r")
source("MODULES/MORE/help.r")
source("MODULES/MORE/glossary.r")
# source("MODULES/MORE/test.r")
# report
source("MODULES/REPORT/report2.r")

# Example data and fit in global environment
# Assume you have a SSO object in global environment
sso <- shinystan::eight_schools
# sso <- shinystan::as.shinystan(readRDS("DATA/fit_full_model_cp.rds"))
# sso <- readRDS("TEST MODELS/eight_schools_meanfield.rds")
# test different models
# sso <- readRDS("TEST MODELS/stan_demo_model_473_method_sampling.rds")
# sso <- readRDS("TEST MODELS/stan_demo_model_99_method_sampling.rds")
# sso <- readRDS("TEST MODELS/stan_demo_model_473_method_meanfield.rds")
# sso <- readRDS("TEST MODELS/stan_demo_model_99_method_meanfield.rds")
# sso <- readRDS("TEST MODELS/stan_demo_model_473_method_fullrank.rds")
# sso <- readRDS("TEST MODELS/stan_demo_model_99_method_fullrank.rds")