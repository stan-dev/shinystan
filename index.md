
[![Travis-CI Build Status](https://travis-ci.org/stan-dev/shinystan.svg?branch=master)](https://travis-ci.org/stan-dev/shinystan)
[![Codecov](http://codecov.io/gh/stan-dev/shinystan/branch/master/graph/badge.svg)](https://codecov.io/gh/stan-dev/shinystan)

<!--
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/shinystan?color=blue)](http://cran.r-project.org/web/packages/shinystan)
[![RStudio CRAN Mirror Downloads](http://cranlogs.r-pkg.org/badges/shinystan?color=blue)](http://cran.rstudio.com/package=shinystan)-->

<div style="text-align:left">
<span><a href="http://mc-stan.org">
<img src="https://raw.githubusercontent.com/stan-dev/logos/master/logo_tm.png" width=100 alt="Stan Logo"/> </a><h2><strong>shinystan</strong></h2>
<h4>Interactive diagnostics and posterior analysis for Bayesian models</h4></span>
</div>

<br> 
<div style="text-align:center">
<a href="/assets/img/shinystan/home.png">
<img src="/assets/img/shinystan/home.png" style="width: 27%;"/>
</a>
<a href="/assets/img/shinystan/explore.png">
<img src="/assets/img/shinystan/explore.png" style="width: 35%;"/>
</a>
<a href="/assets/img/shinystan/diagnose.png">
<img src="/assets/img/shinystan/diagnose.png" style="width: 35%;"/>
</a>
</div>

<br>

ShinyStan provides immediate, informative, customizable visual and
numerical summaries of model parameters and convergence diagnostics for
MCMC simulations. The ShinyStan interface is coded primarily in R using
the [Shiny](http://shiny.rstudio.com) web application framework and is
available via the **shinystan** R package.


## Installation

Install the latest release from **CRAN**:

```{r}
install.packages("shinystan")
```

Install the development version from **GitHub**:

```{r}
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("stan-dev/shinystan", build_vignettes = TRUE)
```

## Demo

After installing run

```{r}
library("shinystan")
launch_shinystan_demo()
```


## More info

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
functionality for models fit using [rstan](http://mc-stan.org/rstan/)
and the No-U-Turn sampler).

