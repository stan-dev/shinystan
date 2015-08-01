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
  if (!requireNamespace("devtools", quietly = TRUE)) 
    install.packages("devtools")
  dpkgs <- list(ggplot2 = "1.0.0", shiny = "0.11.2", shinyjs = "0.0.7.0", V8 = "0.6",
                DT = "0.1", gridExtra = "0.0", gtools = "0.0", htmlwidgets = "0.0", 
                knitr = "0.0", maps = "0.0", markdown = "0.7.4", plyr = "1.8.3", 
                reshape2 = "0.0", threejs = "0.0", xtable = "0.0", xts = "0.9-7")
  pkgs <- names(dpkgs)
  for (i in seq_along(pkgs)) {
    has <- requireNamespace(pkgs[i], quietly = TRUE)
    inst <- if (!has) TRUE else {
      a <- as.character(packageVersion(pkgs[i]))
      b <- dpkgs[[i]]
      0 > compareVersion(a,b)
    }
    if (inst) install.packages(pkgs[i], dependencies = TRUE)
  }
  
  devtools::install_github("rstudio/shinyapps")
  # stable version of shinyBS for use with shinyStan 
  devtools::install_github("jgabry/shinyBS", ref = "shinyBS_for_shinyStan")
  devtools::install_github("stan-dev/shinystan", ref = branch, 
                           dependencies = FALSE,
                           build_vignettes = build_vignettes)
  
  message("\n All set. \n You might need to restart R before using shinyStan. \n")
  return(invisible(NULL))
}
