#######################################################
# shinyStan Installation Instructions: 

# 1) Restart R
# 2) Run install_shinystan(), included below, to install
# 3) Restart R
# 4) library(shinyStan) 

# Troubleshooting                     https://github.com/stan-dev/shinystan/wiki/Troubleshooting
# Open an issue on github             https://github.com/stan-dev/shinystan/issues
# Ask on Stan users google group      https://groups.google.com/forum/#!forum/stan-users
#######################################################

install_shinystan <- function(branch = "master", build_vignettes = TRUE) {
  
  if (getRversion() < '3.1.1') stop("shinyStan requires R version 3.1.1 or greater.")
  
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
  
  install.packages(c("shiny", "knitr", "markdown", "htmlwidgets", "maps"), dependencies = TRUE)
  devtools::install_github("rstudio/DT")
  devtools::install_github("rstudio/shinyapps")
  devtools::install_github("stan-dev/shinystan", ref = branch, build_vignettes = build_vignettes)
  
  # stable version of shinyBS for use with shinyStan 
  devtools::install_github("jgabry/shinyBS", ref = "shinyBS_for_shinyStan")

  
  message("\n All set. \n You might need to restart R before using shinyStan. \n")
  return(invisible(NULL))
}
