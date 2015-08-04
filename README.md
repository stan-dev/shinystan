[![Travis-CI Build Status](https://travis-ci.org/stan-dev/shinystan.svg?branch=shinystan2)](https://travis-ci.org/stan-dev/shinystan)

# ShinyStan


[![shinystan](https://cloud.githubusercontent.com/assets/7796803/6375377/a740c96c-bce4-11e4-82ef-4318a47a1116.png)](https://jgabry.shinyapps.io/ShinyStan2Preview)

[Try the online demo](https://jgabry.shinyapps.io/ShinyStan2Preview)
![shinystan screens](http://i60.tinypic.com/an1p29.png)
The ShinyStan app for exploring Bayesian models is available in
the [shinystan](https://github.com/stan-dev/shinystan/wiki/Installing-shinyStan) 
R package. ShinyStan provides immediate, informative, customizable visual and 
numerical summaries of model parameters and convergence diagnostics for 
MCMC simulations.

## Installing the R package

    devtools::install_github("stan-dev/shinystan", dependencies = TRUE, 
                             build_vignettes = TRUE)

## About ShinyStan

Most applied Bayesian data analysis requires employing a Markov chain Monte 
Carlo (MCMC) algorithm to obtain samples from the posterior distributions of 
the quantities of interest. Diagnosing convergence, checking the fit of the 
model, and producing graphical and numerical summaries of the parameters of 
interest is an essential but often laborious process that slows the down the 
creative and exploratory process of model building. The shinystan package and 
ShinyStan app are designed to facilitate this process in two primary ways:

### Providing interactive visual model exploration

ShinyStan provides immediate, informative, customizable visual and numerical 
summaries of model parameters and convergence diagnostics for MCMC simulations.
Although ShinyStan has some special features only available for users of the 
Rstan package (the R interface to the Stan programming language for Bayesian 
statistical inference), it can also easily be used to explore the output from 
any other program (e.g. Jags, Bugs, SAS) or any user-written MCMC algorithm.

### Making saving and sharing more convenient 

The shinystan package allows you to store the basic components of an entire 
project (code, posterior samples, graphs, tables, notes) in a single object. 
Users can save plots as ggplot2 objects for further customization and easy 
integration in reports or post-processing for publication.


### Licensing

The shinystan package is open source licensed under the GNU Public License, 
version 3 (GPLv3).
