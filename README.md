[![Travis-CI Build Status](https://travis-ci.org/stan-dev/shinystan.png?branch=develop)](https://travis-ci.org/stan-dev/shinystan)

# shinyStan R package and Shiny app

![shinystan](https://cloud.githubusercontent.com/assets/7796803/6375377/a740c96c-bce4-11e4-82ef-4318a47a1116.png)

## Installing shinyStan

For installation instructions see the **Installing shinyStan** page in the [shinyStan wiki](https://github.com/stan-dev/shinystan/wiki/Installing-shinyStan)

## About shinyStan

Most applied Bayesian data analysis requires employing a Markov chain Monte Carlo (MCMC) algorithm to obtain samples from the posterior distributions of the quantities of interest. Diagnosing convergence, checking the fit of the model, and producing graphical and numerical summaries of the parameters of interest is an essential but often laborious process that slows the down the creative and exploratory process of model building. The shinyStan package and Shiny app is designed to facilitate this process in two primary ways:

### Providing interactive visual model exploration

shinyStan provides immediate, informative, customizable visual and numerical summaries of model parameters and convergence diagnostics for MCMC simulations. Although shinyStan has some special features only available for users of the Rstan package (the R interface to the Stan programming language for Bayesian statistical inference), it can also easily be used to explore the output from any other program (e.g. Jags, Bugs, SAS) or any user-written MCMC algorithm.,

### Making saving and sharing more convenient 
shinyStan allows you to store the basic components of an entire project (code, posterior samples, graphs, tables, notes) in a single object. Users can also export graphics into their R sessions as ggplot2 objects for further customization and easy integration in reports or post-processing for publication.

***

### Licensing

shinyStan is licensed under GPLv3.
