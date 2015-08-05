## Submission summary

This is the first submission of the package. 

## Test environments
* local OS X install, R 3.2.1 and 3.1.1
* ubuntu 12.04 (on travis-ci)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:
* checking dependencies in R code ... NOTE
  Namespace in Imports field not imported from: 'R6'

  RStudio's shinyapps can be found at the link provided in 
  Additional_repositories, but it is not essential to the functionality
  of this package. The one function that requires the shinyapps package
  provides a nice feature but is separate from the graphical user interface,
  which is the core contribution of this package. 

## Downstream dependencies
None
