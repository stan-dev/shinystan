# checks if an object is a stanfit object
is_stan <- function(X) inherits(X, "stanfit")

# gets names of all shinystan objects in user's global environment
get_sso_names <- function() {
  Filter(function(x) "shinystan" %in% class(get(x)), objects(envir = .GlobalEnv))
}

# generates new name for shinystan object if default name is already taken
rename_sso <- function(out_name, sso_names) {
  
  renamer <- function(i) {
    check_name <- paste0(out_name,".",i)
    check_name %in% sso_names
  }
  rename  <- out_name %in% sso_names  
  i <- 1
  while (rename) {
    rename <- renamer(i)
    i <- i + 1
  }
  new_out_name <- paste0(out_name,".",i-1)
  new_out_name
}

# function to run on exiting launch_shinystan
cleanup_shinystan <- function(shinystan_object, out_name, is_stanfit_object) {
  rename <- out_name %in% objects(envir = .GlobalEnv)
  if (is_stanfit_object && rename) {
    sso_names <- get_sso_names()
    out_name <- rename_sso(out_name, sso_names)
  }
  assign(out_name, shinystan_object, inherits = TRUE)
  message(paste("\n Name of shinystan object:", out_name))
  shinystan_object <<- NULL
  rm(list = "shinystan_object", envir = globalenv())
}

# launch the app
launch <- function(object) {
  shinystan_object <<- if (is.shinystan(object)) object else stan2shinystan(object)
  shiny::runApp(system.file("shinyStan", package = "shinyStan"))
}