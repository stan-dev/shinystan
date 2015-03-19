.onAttach <- function(...) {
  version <- "1.1.0"
  msg <- paste0("\nshinyStan version ", version, ". \nTo check if a newer version is available visit github.com/stan-dev/shinystan/releases. \n")
  
  # check shinyBS version
  sbs_version <- utils::packageVersion("shinyBS") 
  if (sbs_version != "0.50.1") {
    msg <- paste0(msg, 
                  "\nMessage:", 
                  "\nshinyStan runs best with version ",
                  "0.50.1 of the shinyBS package (you ",
                  "have version ", sbs_version, ").",
                  "\nTo install the preferred version ",
                  "run \ndevtools::install_github('jgabry/shinyBS@shinyBS_for_shinyStan')"
    )
  }
  packageStartupMessage(msg)
} 

