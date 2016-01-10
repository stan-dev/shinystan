<a href="http://mc-stan.org">
<img src="https://raw.githubusercontent.com/stan-dev/logos/master/logo.png" width=200 alt="Stan Logo"/>
</a>

# ShinyStan

[![Travis-CI Build Status](https://travis-ci.org/stan-dev/shinystan.svg?branch=master)](https://travis-ci.org/stan-dev/shinystan)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/shinystan?color=blue)](http://cran.r-project.org/web/packages/shinystan)

ShinyStan provides immediate, informative, customizable visual and 
numerical summaries of model parameters and convergence diagnostics for 
MCMC simulations. The ShinyStan graphical user interface is available 
via the **shinystan** R package. [Try the online demo.](http://gelman-group-win.stat.columbia.edu:3500)

### Installing the shinystan R package
* Install from CRAN:

```{r}
install.packages("shinystan")
```
    
If this fails, try adding the arguments `type='source'` and/or `repos='http://cran.rstudio.com'`.
    
* Install from GitHub (requires [devtools](https://github.com/hadley/devtools) package):

```{r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("stan-dev/shinystan", build_vignettes = TRUE)
```
    
To take advantage of all the features in the **shinystan** package, it is also 
recommended to install the [rsconnect](https://github.com/rstudio/rsconnect) 
package. You can do this by running

```{r}
devtools::install_github("rstudio/rsconnect")
```

### About ShinyStan

Applied Bayesian data analysis is primarily implemented through the MCMC 
algorithms offered by various software packages. When analyzing a posterior sample 
obtained by one of these algorithms the first step is to check for signs that 
the chains have converged to the target distribution and and also for signs that 
the algorithm might require tuning or might be ill-suited for the given model. 
There may also be theoretical problems or practical inefficiencies with the 
specification of the model. 

ShinyStan provides interactive plots and tables helpful for analyzing a 
posterior sample, with particular attention to identifying potential problems
with the performance of the MCMC algorithm or the specification of the model. 
ShinyStan is powered by RStudio's Shiny web application framework and works with 
the output of MCMC programs written in any programming language (and has extended 
functionality for models fit using [RStan](http://mc-stan.org/interfaces/rstan.html) 
and the No-U-Turn sampler). 

### Saving and deploying (sharing)

The **shinystan** package allows you to store the basic components of an entire 
project (code, posterior samples, graphs, tables, notes) in a single object. 
Users can save many of the plots as ggplot2 objects for further customization 
and easy integration in reports or post-processing for publication.

**shinystan** also provides the `deploy_shinystan` function, 
which lets you easily deploy your own ShinyStan apps online using RStudio's 
[ShinyApps](https://www.shinyapps.io) service for any of 
your models. Each of your apps (each of your models) will have a unique url
and is compatible with Safari, Firefox, Chrome, and most other browsers.

### Licensing

The **shinystan** R package and ShinyStan interface are open source licensed under 
the GNU Public License, version 3 (GPLv3).
