.onAttach <- function(...) {
  version <- "1.1.0"
  msg <- paste0("\n shinyStan version ", version, ". \n To check if a newer version is available visit github.com/stan-dev/shinystan/releases \n")
  packageStartupMessage(msg)
} 

