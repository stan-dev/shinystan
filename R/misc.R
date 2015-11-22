# This file is part of shinystan
# Copyright (C) 2015 Jonah Gabry
#
# shinystan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinystan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.

sso_check <- function(sso) {
  if (!is.shinystan(sso)) 
    stop("Please specify a shinystan object", call. = FALSE)
  else 
    invisible(TRUE)
}
is.stanfit <- function(X) inherits(X, "stanfit")
is.stanreg <- function(X) inherits(X, "stanreg")

rstan_check <- function() {
  if (!requireNamespace("rstan", quietly = TRUE)) 
    stop("You need to have the RStan package installed to use this option.", 
         call. = FALSE)
}
coda_check <- function() {
  if (!requireNamespace("coda", quietly = TRUE)) 
    stop("You need to have the coda package installed to use this option.", 
         call. = FALSE)
}

launch <- function(object, rstudio = FALSE, ...) {
  stopifnot(is.shinystan(object))
  launch.browser <- if (!rstudio) 
    TRUE else getOption("shiny.launch.browser", interactive())
  .sso_env$.shinystan_temp_object <- object
  on.exit(.sso_env$.shinystan_temp_object <- NULL, add = TRUE)
  shiny::runApp(system.file("ShinyStan", package = "shinystan"), 
                launch.browser = launch.browser, ...)
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

grepl_ic <- function(pattern, x, ignore.case = TRUE) {
  grepl(pattern = pattern, x = x, ignore.case = ignore.case)
}

get_type <- function(x) {
  if (is.shinystan(x)) return("shinystan")
  else if (is.stanfit(x)) return("stanfit")
  else if (is.stanreg(x)) return("stanreg")
  else if (inherits(x, "mcmc.list")) return("mcmclist")
  else if (is.list(x)) return("chainlist")
  else return("other")
}

# functions to set defaults for ppcheck selectInputs for y and y_rep 
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
  fileDir <- file.path(appDir, "server_files", "pages", "diagnose", 
                       "ppcheck", "ui")
  y_file <- file.path(fileDir, "pp_y_from_r.R")
  yrep_file <- file.path(fileDir, "pp_yrep_from_sso.R")
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

.retrieve <- function(sso, what, ...) {
  if (what %in% c("rhat", "rhats", "Rhat", "Rhats", "r_hat", "R_hat")) {
    return(retrieve_rhat(sso, ...))
  }
  if (what %in% c("N_eff","n_eff", "neff", "Neff", "ess","ESS")) {
    return(retrieve_neff(sso, ...))
  }
  if (grepl_ic("mean", what)) {
    return(retrieve_mean(sso, ...))
  }
  if (grepl_ic("sd", what)) {
    return(retrieve_sd(sso, ...))
  }
  if (what %in% c("se_mean", "mcse")) {
    return(retrieve_mcse(sso, ...))
  }
  if (grepl_ic("quant", what)) {
    return(retrieve_quant(sso, ...))
  }
  if (grepl_ic("median", what)) {
    return(retrieve_median(sso, ...))
  }
  if (grepl_ic("tree", what) | grepl_ic("depth", what)) {
    return(retrieve_max_treedepth(sso, ...))
  }
  if (grepl_ic("step", what)) {
    return(retrieve_avg_stepsize(sso, ...))
  }
  if (grepl_ic("diverg", what)) {
    return(retrieve_prop_divergent(sso, ...))
  }
  if (grepl_ic("accept", what)) {
    return(retrieve_avg_accept(sso, ...))
  }
}