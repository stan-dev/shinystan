#######################################################
# Installation instructions: 

# 1) Restart R
# 2) Run install_shinystan(), included below, to install
# 3) Restart R
# 4) library(shinyStan) 

# If installation fails you can open an issue on github
# (https://github.com/stan-dev/shinystan/issues) 
# or ask for help at the Stan users google group 
# (https://groups.google.com/forum/#!forum/stan-users)
#######################################################

install_shinystan <- function() {
  msg <- "Note: this will install or update any missing or out-of-date packages needed to run shinyStan. Do you want to continue?"
  continue <- select.list(choices = c("Yes", "No"), title = msg, graphics = FALSE)
  if (continue == "No") {
    message("Installation canceled by user.")
    return(invisible(NULL))
  }
  
  try(remove.packages("SHINYstan"), silent = TRUE)
  try(remove.packages("shinyBS"), silent = TRUE)
  
  if (!require(devtools)) install.packages("devtools")
  
  if (!("shiny" %in% installed.packages()[,"Package"])) {
    install.packages("shiny")
  } else {
    shiny_ok <- packageVersion("shiny") == "0.11.1"
    if (!shiny_ok) install.packages("shiny")
  }
  
  install.packages(c("htmlwidgets", "maps"), dependencies = TRUE)
  devtools::install_github("ebailey78/shinyBS", ref = "shinyBS3", dependencies = TRUE)
  devtools::install_github("bwlewis/rthreejs", dependencies = TRUE)
  devtools::install_github("stan-dev/shinyStan", build_vignettes = TRUE)
  
  message("\n All set. \n You might need to restart R before using shinyStan. \n")
  return(invisible(NULL))
}
