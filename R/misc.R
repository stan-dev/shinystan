
# checks if an object is a stanfit object ---------------------------------
is_stan <- function(X) inherits(X, "stanfit")


# gets names of all shinystan objects in user's global environment --------
get_sso_names <- function() {
  Filter(function(x) "shinystan" %in% class(get(x)), objects(envir = .GlobalEnv))
}


# generates new name for shinystan object if default name is taken --------
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


# cleanup function to run on exiting launch_shinystan -----------------------------
cleanup_shinystan <- function(shinystan_object, out_name, is_stanfit_object) {
  rename <- out_name %in% objects(envir = .GlobalEnv)
  if (is_stanfit_object && rename) {
    sso_names <- get_sso_names()
    out_name <- rename_sso(out_name, sso_names)
  }
  assign(out_name, shinystan_object, inherits = TRUE)
  message(paste("\n Name of shinystan object:", out_name))
  rm(list = "shinystan_object", envir = globalenv())
}


# assignment function -----------------------------------------------------
assign_shinystan <- function(X) {
  assign("shinystan_object", X, inherits = TRUE)
}


# launch the app ----------------------------------------------------------
launch <- function(object) {
  if (is.shinystan(object)) assign_shinystan(object)
  else assign_shinystan(stan2shinystan(object))

  shiny::runApp(system.file("shinyStan", package = "shinyStan"))
}


# launch the demo ---------------------------------------------------------
launch_demo <- function(object) {
  assign_shinystan(object)
  shiny::runApp(system.file("shinyStan", package = "shinyStan"))
}


# mcmclist to matrix (adapted from Coda package) --------------------------
mcmclist2matrix <- function(x) {
  out <- matrix(nrow = coda::niter(x) * coda::nchain(x), ncol = coda::nvar(x))
  cols <- 1:coda::nvar(x)
  for (i in 1:coda::nchain(x)) {
    rows <- (i-1)*coda::niter(x) + 1:coda::niter(x)
    out[rows, cols] <- x[[i]]
  }
  rownames <- character(ncol(out))
  rownames[cols] <- coda::varnames(x, allow.null = FALSE)
  dimnames(out) <- list(NULL, rownames)
  out
}
