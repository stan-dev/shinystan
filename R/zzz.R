# .onAttach <- function(...) {
#   lib <- dirname(system.file(package = "shinyStan"))
#   des <- packageDescription("shinyStan", lib.loc = lib)
#   version <- des$Version
#   msg <- paste0("shinyStan v", version)
#   packageStartupMessage(msg)
# } 
# 
