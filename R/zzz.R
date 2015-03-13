.onAttach <- function(...) {
  lib <- dirname(system.file(package = "shinyStan"))
  des <- packageDescription("shinyStan", lib.loc = lib)
  version <- des$Version
  msg <- paste0("\n shinyStan version ", version, ". \n To check if a newer version is available visit github.com/stan-dev/shinystan/releases")
  packageStartupMessage(msg)
} 

