# `shinystan news`

### Version 2.1.1
* Introduce `drop_parameters` function for removing parameters from a shinystan 
object (useful for very large objects when you only want to look at a subset of
parameters).
* Add **rstanarm** to Suggests (in the DESCRIPTION file) so `rstanarm::pp_check`
can be called.

### Version 2.1.0
* Compatibility with recent ggplot2 update
* Select parameters via regular expressions in parameters plot ('Estimate' page)
* Fix error when selecting an entire parameter group in parameters 
plot ('Estimate' page)

### Version 2.0.1
* Fixes small bugs and typos
* Adds support for stanreg objects
* Moves ggplot2 from Depends to Imports

### Version 2.0.0 (major update, initial CRAN release)

Version 2.0.0 has a new look, a new(ish) name, and a lot of new functionality. 
Many bugs have also been fixed (see GitHub issue tracker). 

[Available on CRAN](https://cran.rstudio.com/web/packages/shinystan/index.html).

### New names
* The name of the R package is **shinystan** and the app/GUI is **ShinyStan**. 

### Deploy ShinyStan apps to shinyapps.io (RStudio's ShinyApps service)
* The `deploy_shinystan` function lets you easily deploy ShinyStan apps 
to RStudio's shinyapps.io for any of your models. Each of your apps 
(i.e. each of your models) will have a unique url.

### Some of the new features in ShinyStan app 
* Rebranding (new look to reflect changes to Stan logo and website)
* HMC/NUTS diagnostic plots ('Diagnose' page, 'HMC/NUTS (plots)' tab)
* Specify transformations (e.g. log, logit, sqrt, etc.) for density, 
histogram, bivariate, trivariate plots (on 'Explore' page) and HMC/NUTS diagnostics
plots.
* Many plots can now also be saved as pdf
* Bivariate scatterplot plot also shows divergent transitions and max treedepth 
saturation (on 'Explore' page)
* More detailed glossary entries
* (Experimental) Introduce basic graphical posterior predictive 
checking ('Diagnose' page, 'PPcheck' tab) for limited class of models
* Option to show partial autocorrelations ('Diagnose' page, 'Autocorrelation' tab)
* Better customization of of posterior summary statistics table
* Many improvements to GUI design
