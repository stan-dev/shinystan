# This file is part of shinyStan
# Copyright (C) 2015 Jonah Sol Gabry & Stan Development Team
#
# shinyStan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinyStan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.



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

# check objects for as.shinystan
get_type <- function(x) {
  if (is.shinystan(x)) return("shinystan")
  if (is_stan(x)) return("stanfit")
  if (inherits(x, "mcmc.list")) return("mcmclist")
  if (is.list(x) & !inherits(x, "mcmc.list")) return("chainlist")
  return("other")
}

# functions to set defaults for ppcheck selectInputs for y and y_rep ---------------
y_lines <- function(y_name = "y") {
  paste0(
    "output$ui_pp_y_from_r <- renderUI({
    choices <- objects(envir = .GlobalEnv)
    selectizeInput('y_name', label = span(style = 'color: #337ab7;', 'y, a vector of observations'), 
    choices = c('', choices), 
    selected = '",y_name,"')
    })"
  )
}

yrep_lines <- function(yrep_name) {
  paste0(
    "output$ui_pp_yrep_from_sso <- renderUI({
    choices <- param_names
    choices <- strsplit(choices, split = '[', fixed = TRUE)
    choices <- lapply(choices, function(i) return(i[1]))
    choices <- unique(unlist(choices))
    selectizeInput('yrep_name', 
    label = span(style = 'color: #337ab7;', 'y_rep, posterior predictive replications'), 
    choices = c('', choices),
    selected = '",yrep_name,"')
    })"
  )
}

write_files <- function(files, lines) {
  stopifnot(length(files) == length(lines))
  for (f in seq_along(files)) {
    fileConn <- file(files[f])
    writeLines(lines[f], fileConn)
    close(fileConn)
  }
}

set_ppcheck_defaults <- function(appDir, yrep_name, y_name = "y") {
  fileDir <- file.path(appDir, "server_files", "pp_check", "dynamic_ui")
  y_file <- file.path(fileDir, "ui_pp_y_from_r.R")
  yrep_file <- file.path(fileDir, "ui_pp_yrep_from_sso.R")
  
  for (file in c("y_file", "yrep_file")) {
    f <- get(file)
    if (file.exists(f)) {
      file.remove(f)
      file.create(f)
    }
  }
  
  write_files(
    files = c(y_file, yrep_file), 
    lines = c(y_lines(y_name), yrep_lines(yrep_name))
  )
}

