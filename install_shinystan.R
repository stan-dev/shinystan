#######################################################
# shinyStan Installation instructions: 

# 1) Restart R
# 2) Run install_shinystan(), included below, to install
# 3) Restart R
# 4) library(shinyStan) 

# Troubleshooting                     https://github.com/stan-dev/shinystan/wiki/Troubleshooting
# Open an issue on github             https://github.com/stan-dev/shinystan/issues
# Ask on Stan users google group      https://groups.google.com/forum/#!forum/stan-users
#######################################################

install_shinystan <- function() {
  
  if (getRversion() < '3.1.2') stop("shinyStan requires R version 3.1.2 or greater.")
  
  msg <- "Note: this will install or update packages needed to run shinyStan. Do you want to continue?"
  continue <- select.list(choices = c("Yes", "No"), title = msg, graphics = FALSE)
  if (continue == "No") {
    message("Installation canceled by user.")
    return(invisible(NULL))
  }
  
  # remove old versions of shinyStan and shinyBS
  try(remove.packages("shinyStan"), silent = TRUE)
  try(remove.packages("shinyBS"), silent = TRUE)
  
  # check for devtools
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  } 
  
  # install needed packages from CRAN
  install.packages(c("shiny", "knitr", "markdown", "htmlwidgets", "maps"), dependencies = TRUE)
  
  # install needed packages from GitHub
  devtools::install_github("jgabry/shinyBS", ref = "shinyBS_for_shinyStan")
  devtools::install_github("bwlewis/rthreejs", dependencies = TRUE)
  
  # install shinyapps package for deploying to shinyapps.io
  devtools::install_github("rstudio/shinyapps")
  
  # install latest shinyStan release from GitHub
  devtools::install_github("stan-dev/shinystan", build_vignettes = TRUE)
  
  message("\n All set. \n You might need to restart R before using shinyStan. \n")
  return(invisible(NULL))
}
