## shinyStan version 1.2.0

### New functions in shinyStan R package 
* 'deploy_shinystan' for easily deploying custom apps to RStudio's shinyapps.io. Each app/model will have a unique url. 
* 'generate_quantity' for adding a new parameter as a function of one or two existing parameters
* 'retrieve' for accessing summary stats and sampler diagnostics stored in shinystan object (e.g. rhat, neff, posterior sd, etc.)

### New features in shinyStan app 
* Specify arbitrary transformations (e.g. log, logit, sqrt, etc.) for density, histogram, bivariate and trivariate plots (on 'Explore' page)
* HMC/NUTS diagnostic plots ('Diagnose' page, 'HMC/NUTS (plots)' tab)
* Bivariate scatterplot plot also shows divergent iterations and max treedepth saturation (on 'Explore' page)
* (Experimental) Introduce basic graphical posterior predictive checking ('Diagnose' page, 'PPcheck' tab) for limited class of models
* Standard deviations of HMC/NUTS sampler statistics ('Diagnose' page, 'HMC/NUTS (stats)' tab)
* Option to change display to partial autocorrelations  ('Diagnose' page, 'Autocorrelation' tab)
* Better filtering for posterior summary statistics table
* Small design improvements to GUI

### Bug fixes 
GitHub issue number in parentheses

* Cooperation with dplyr (28) 
* Multiparameter traceplots y-axis limits (17)
* Multiview default parameter (19)
* Latex table decimal places (69)
* Small fixes for compatibility with shiny v0.12.0 (73)
* Many other small bugs