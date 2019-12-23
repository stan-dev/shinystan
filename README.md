[<img src="https://raw.githubusercontent.com/stan-dev/logos/master/logo_tm.png" width=100 alt="Stan Logo"/>](http://mc-stan.org)

# ShinyStan

[![Travis-CI Build Status](https://travis-ci.org/stan-dev/shinystan.svg?branch=master)](https://travis-ci.org/stan-dev/shinystan)
[![Codecov](http://codecov.io/gh/stan-dev/shinystan/branch/master/graph/badge.svg)](https://codecov.io/gh/stan-dev/shinystan)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/shinystan?color=blue)](http://cran.r-project.org/web/packages/shinystan)
[![RStudio CRAN Mirror Downloads](http://cranlogs.r-pkg.org/badges/grand-total/shinystan?color=blue)](http://cran.rstudio.com/package=shinystan)

ShinyStan provides immediate, informative, customizable visual and
numerical summaries of model parameters and convergence diagnostics for
MCMC simulations. The ShinyStan interface is coded primarily in R using
the [Shiny](http://shiny.rstudio.com) web application framework and is
available via the **shinystan** R package.

### Resources

* [mc-stan.org/shinystan](http://mc-stan.org/shinystan) (Website with online documentation)
* [Ask a question](http://discourse.mc-stan.org) (Stan Forums on Discourse)
* [Open an issue](https://github.com/stan-dev/shinystan/issues) (GitHub issues for bug reports, feature requests)

### Installation

* Install the latest release from CRAN:

```r
install.packages("shinystan")
```

* Install the development version from GitHub (requires [devtools](https://github.com/r-lib/devtools) package):

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("stan-dev/shinystan", build_vignettes = TRUE)
```

* Install the alpha test of version 3.0 from GitHub (requires [devtools](https://github.com/r-lib/devtools) package):

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("stan-dev/shinystan", ref = "v3-alpha", build_vignettes = TRUE)
```

### Demo

After installing run

```r
library("shinystan")
launch_shinystan_demo()
```

### About ShinyStan

Applied Bayesian data analysis is primarily implemented through the MCMC
algorithms offered by various software packages. When analyzing a posterior sample
obtained by one of these algorithms the first step is to check for signs that
the chains have converged to the target distribution and also for signs that
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


### Licensing

The **shinystan** R package and ShinyStan interface are open source licensed under
the GNU Public License, version 3 (GPLv3).
