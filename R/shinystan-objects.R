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


# shinystan class definition ------------------------------------------
#' S4 shinystan objects
#'
#' @aliases shinystan-class
#' @description See \code{\link{as.shinystan}} for documentation on creating
#'   shinystan objects and \code{\link{eight_schools}} for an example object.
#'  
#' @slot model_name (\code{"character"}) Model name.
#' @slot param_names (\code{"character"}) Parameter names.
#' @slot param_dims (\code{"list"}) Parameter dimensions.
#' @slot posterior_sample (\code{"array"}) MCMC sample.
#' @slot summary (\code{"matrix"}) Summary stats for \code{posterior_sample}.
#' @slot sampler_params (\code{"list"}) Sampler parameters (for certain Stan
#'   models only).
#' @slot n_chain (\code{"integer"}) Number of chains.
#' @slot n_iter (\code{"integer"}) Number of iterations per chain.
#' @slot n_warmup (\code{"integer"}) Number of warmup iterations per chain.
#' @slot user_model_info (\code{"character"}) Notes to display on ShinyStan's
#'   \strong{Notepad} page.
#' @slot model_code (\code{"character"}) Model code to display on ShinyStan's
#'   \strong{Model Code} page.
#' @slot misc (\code{"list"}) Miscellaneous, for internal use.
#' 
#' @template seealso-as.shinystan
#' @template seealso-drop_parameters
#' @template seealso-generate_quantity
#' @seealso \code{\link{shinystan-metadata}} to view or change metadata
#'   associated with a shinystan object.
#' 
shinystan <- setClass(
  Class = "shinystan",
  slots = list(
    model_name       = "character",
    param_names      = "character",
    param_dims       = "list",
    posterior_sample = "array",
    summary          = "matrix",
    sampler_params   = "list",
    n_chain          = "numeric",
    n_iter           = "numeric",
    n_warmup         = "numeric",
    user_model_info  = "character",
    model_code       = "character",
    misc             = "list"
  ),
  prototype = list(
    model_name = "No name",
    param_names = "",
    param_dims = list(),
    posterior_sample = array(NA, c(1, 1)),
    summary = matrix(NA, nr = 1, nc =1),
    sampler_params = list(NA),
    n_chain = 0,
    n_iter = 0,
    n_warmup = 0,
    user_model_info = "Use this space to store notes about your model",
    model_code = "Use this space to store your model code",
    misc = list(sso_version = utils::packageVersion("shinystan"))
  )
)



# create shinystan objects ------------------------------------------------
  
# as.shinystan (generic) --------------------------------------------------
#' Create and test shinystan objects
#' 
#' @description The \code{as.shinystan} function creates shinystan objects that 
#'   can be used with \code{\link{launch_shinystan}} and various other functions
#'   in the \pkg{shinystan} package. \code{as.shinystan} is a generic for which 
#'   the \pkg{shinystan} package provides several methods. Currently methods are
#'   provided for creating shinystan objects from arrays, lists of matrices,
#'   stanfit objects (\pkg{rstan}), stanreg objects (\pkg{rstanarm}), and
#'   mcmc.list objects (\pkg{coda}).
#'   
#'   \code{is.shinystan} tests if an object is a shinystan object.
#'
#' @name as.shinystan
#' @export
#' @param X For \code{as.shinystan}, an object to be converted to a shinystan 
#'   object. See the Methods section below. For \code{is.shinystan}, an object
#'   to check.
#' @param ... Arguments passed to the individual methods.
#'   
#' @return \code{as.shinystan} returns a shinystan object, which is an instance 
#'   of S4 class \code{"shinystan"}.
#'   
#'   \code{is.shinystan} returns \code{TRUE} if the tested object is a shinystan
#'   object and \code{FALSE} otherwise.
#'
#' @template seealso-launch
#' @template seealso-drop_parameters   
#' @template seealso-generate_quantity
#'
setGeneric("as.shinystan", function(X, ...) {
  if (inherits(X, "shinystan"))
    stop("Already a shinystan object.")
  standardGeneric("as.shinystan")
})

#' @export
#' @rdname as.shinystan
is.shinystan <- function(X) inherits(X, "shinystan")

# as.shinystan (array) ---------------------------------------------------
#' @describeIn as.shinystan Create a shinystan object from a 3-D
#'   \code{\link{array}} of simulations. The array should have dimensions
#'   corresponding to iterations, chains, and parameters, in that order.
#'
#' @param model_name A string giving a name for the model.
#' @param burnin The number of iterations to treat as burnin (warmup). Should be
#'   \code{0} if warmup iterations are not included in \code{X}.
#' @param param_dims Rarely used and never necessary. A named list giving the 
#'   dimensions for all parameters. For scalar parameters use \code{0} as the 
#'   dimension. See Examples.
#' @param model_code Optionally, a character string with the code used to run
#'   the model. This can also be added to your \code{shinystan} object later
#'   using the \code{\link[shinystan]{model_code}} function. See 
#'   \code{\link[shinystan]{model_code}} for additional formatting instructions.
#'   After launching the app the code will be viewable in the \strong{Model 
#'   Code} tab. For \code{stanfit} (\pkg{rstan}) and \code{stanreg}
#'   (\pkg{rstanarm}) objects the model code is automatically retrieved from the
#'   object.
#' @param note Optionally, text to display on ShinyStan's notes page (stored in 
#'   \code{user_model_info} slot).
#'   
#' @examples  
#' \dontrun{
#' sso <- as.shinystan(X, ...) # replace ... with optional arguments or omit it
#' launch_shinystan(sso)
#' }
#'
setMethod(
  "as.shinystan",
  signature = "array",
  definition = function(X,
                        model_name = "unnamed model",
                        burnin = 0,
                        param_dims = list(),
                        model_code = NULL,
                        note = NULL,
                        ...) {
    validate_model_code(model_code)
    is3D <- isTRUE(length(dim(X)) == 3)
    if (!is3D)
      stop ("'X' must have 3 dimensions.")
    
    if (is.null(dimnames(X)[[3]]))
      dimnames(X)[[3]] <- paste0("V", seq_len(dim(X)[3]))
    param_names <- dimnames(X)[[3]]
    dimnames(X) <- list(
      iterations = seq_len(nrow(X)),
      chains = paste0("chain:", seq_len(ncol(X))),
      parameters = param_names
    )
    
    sso <- shinystan(
      model_name = model_name,
      param_names = param_names,
      param_dims = .set_param_dims(param_dims, param_names),
      posterior_sample = X,
      summary = shinystan_monitor(X, warmup = burnin),
      n_chain = ncol(X),
      n_iter = nrow(X),
      n_warmup = burnin
    )
    if (!is.null(note))
      sso <- suppressMessages(notes(sso, note = note, replace = TRUE))
    if (!is.null(model_code))
      sso <- suppressMessages(model_code(sso, code = model_code))
    
    return(sso)
  }
)

.set_param_dims <- function(param_dims = list(), 
                            param_names = character(length(param_dims))) {
  if (!length(param_dims)) {
    param_dims <- list()
    param_dims[seq_along(param_names)] <- NA
    names(param_dims) <- param_names
    for (i in seq_along(param_names))
      param_dims[[i]] <- numeric(0)
  } else {
    zeros <- sapply(seq_along(param_dims), function(i)
      0 %in% param_dims[[i]])
    for (i in which(zeros))
      param_dims[[i]] <- numeric(0)
  }
  param_dims 
}


# as.shinystan (list) ---------------------------------------------------
#' @describeIn as.shinystan Create a shinystan object from a \code{\link{list}}
#'   of matrices. Each \code{\link{matrix}} (or 2-D array) should contain the
#'   simulations for an individual chain and all of the matrices should have the
#'   same number of iterations (rows) and parameters (columns). Parameters
#'   should have the same names and be in the same order.
#'   
#' @examples 
#' \dontrun{   
#' ########################
#' ### list of matrices ###
#' ########################
#'
#' # Generate some fake data
#' chain1 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
#' chain2 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
#' sso <- as.shinystan(list(chain1, chain2))
#' launch_shinystan(sso)
#' 
#' # We can also specify some or all of the optional arguments
#' # note: in order to use param_dims we need to rename 'beta1' and 'beta2'
#' # to 'beta[1]' and 'beta[2]'
#' colnames(chain1) <- colnames(chain2) <- c(paste0("beta[",1:2,"]"), "sigma")
#' sso2 <- as.shinystan(list(chain1, chain2), 
#'                      model_name = "Example", burnin = 0, 
#'                      param_dims = list(beta = 2, sigma = 0))
#' launch_shinystan(sso2)
#' }
#'
setMethod(
  "as.shinystan",
  signature = "list",
  definition = function(X,
                        model_name = "unnamed model",
                        burnin = 0,
                        param_dims = list(),
                        model_code = NULL,
                        note = NULL,
                        ...) {
    validate_model_code(model_code)
    if (!length(X))
      stop('List is empty.')
    
    dims <- sapply(X, function(x) length(dim(x)))
    if (!isTRUE(all(dims == 2)))
      stop("All elements of X should be matrices / 2-D arrays.")
    
    nChain <- length(X)
    for (i in seq_len(nChain)) {
      nms <- colnames(as.matrix(X[[i]]))
      if (is.null(nms) || !all(nzchar(nms)))
        stop(
          "Some parameters are missing names. ",
          "Check the column names for the matrices in your list of chains."
        )
    }
    
    if (nChain == 1) {
      n_iter <- nrow(X[[1]])
      param_names <- colnames(X[[1]])
    } else {
      n_iter <- sapply(X, nrow)
      same_iters <- length(unique(n_iter)) == 1
      if (!same_iters)
        stop("Each chain should contain the same number of iterations.")
      cnames <- sapply(X, colnames)
      if (is.array(cnames)) {
        same_params <- identical(cnames[, 1], cnames[, 2])
        param_names <- cnames[, 1]
      } else {
        same_params <- length(unique(cnames)) == 1
        param_names <- cnames
      }
      if (!same_params)
        stop("The parameters for each chain should be in the same order ",
             "and have the same names.")
      n_iter <- n_iter[1]
    }
    param_names <- unique(param_names)
    nParam <- length(param_names)
    out <- array(NA, dim = c(n_iter, nChain, nParam))
    for (i in seq_len(nChain))
      out[, i,] <- X[[i]]

    dimnames(out) <- list(
      iterations = NULL,
      chains = paste0("chain:", seq_len(nChain)),
      parameters = param_names
    )
    as.shinystan(
      out,
      model_name = model_name,
      burnin = burnin,
      param_dims = param_dims,
      model_code = model_code,
      note = note,
      ...
    )
  }
)


# as.shinystan (mcmc.list) -----------------------------------------------
setOldClass("mcmc.list")
#' @describeIn as.shinystan Create a shinystan object from an mcmc.list 
#'   (\pkg{coda}).
#' 
setMethod(
  "as.shinystan",
  signature = "mcmc.list",
  definition = function(X,
                        model_name = "unnamed model",
                        burnin = 0,
                        param_dims = list(),
                        model_code = NULL,
                        note = NULL,
                        ...) {
    check_suggests("coda")
    validate_model_code(model_code)
    
    if (length(X) == 1) {
      return(
        as.shinystan(
          X = list(.mcmclist2matrix(X)),
          model_name = model_name,
          burnin = burnin,
          param_dims = param_dims,
          model_code = model_code,
          note = note,
          ...
        )
      )
    }
    
    posterior <- array(
      NA,
      dim = c(coda::niter(X), coda::nvar(X), coda::nchain(X)),
      dimnames = list(
        iter = time(X),
        var = coda::varnames(X),
        chain = coda::chanames(X)
      )
    )
    for (c in seq_len(coda::nchain(X)))
      posterior[, , c] <- X[[c]]
    
    posterior <- aperm(drop(posterior), c(1, 3, 2))
    dimnames(posterior) <- list(
      iterations = seq_len(nrow(posterior)),
      chains = paste0("chain:", seq_len(ncol(posterior))),
      parameters = dimnames(posterior)[[3]]
    )
    param_names <- dimnames(X[[1]])[[2]]
    
    sso <- shinystan(
      model_name = model_name,
      param_names = param_names,
      param_dims = .set_param_dims(param_dims, param_names),
      posterior_sample = posterior,
      summary = shinystan_monitor(posterior, warmup = burnin),
      n_chain = ncol(posterior),
      n_iter = nrow(posterior),
      n_warmup = burnin
    )
    if (!is.null(note))
      sso <- suppressMessages(notes(sso, note = note, replace = TRUE))
    if (!is.null(model_code))
      sso <- suppressMessages(model_code(sso, code = model_code))
    
    return(sso)
  }
)

.mcmclist2matrix <- function(x) {
  # adapted from Coda package
  out <- matrix(nrow = coda::niter(x) * coda::nchain(x), ncol = coda::nvar(x))
  cols <- seq_len(coda::nvar(x))
  for (i in seq_len(coda::nchain(x))) {
    rows <- (i-1)*coda::niter(x) + seq_len(coda::niter(x))
    out[rows, cols] <- x[[i]]
  }
  rownames <- character(ncol(out))
  rownames[cols] <- coda::varnames(x, allow.null = FALSE)
  dimnames(out) <- list(NULL, rownames)
  out
}


# as.shinystan (stanfit) -------------------------------------------------
setClass("stanfit", getClass("stanfit", where = getNamespace("rstan")))

#' @describeIn as.shinystan Create a shinystan object from a stanfit object 
#'   (\pkg{\link[rstan]{rstan}}). Fewer optional arguments are available for 
#'   this method because all important information can be taken automatically 
#'   from the stanfit object.
#'   
#' @param pars For stanfit objects (\pkg{rstan}), an optional character vector
#'   specifying which parameters should be included in the shinystan object.
#'
#' @examples
#' \dontrun{
#' ######################
#' ### stanfit object ###
#' ######################
#' library("rstan")
#' fit <- stan_demo("eight_schools")
#' sso <- as.shinystan(fit, model_name = "example")
#' }
#' 
setMethod(
  "as.shinystan",
  signature = "stanfit",
  definition = function(X,
                        pars,
                        model_name = X@model_name,
                        note = NULL,
                        ...) {
    check_suggests("rstan")
    if (!missing(pars)) {
      any_indiv_els <- any(grepl("[", pars, fixed = TRUE))
      if (any_indiv_els)
        stop("Individual elements of non-scalar parameters not allowed in 'pars'.")
      if (!"lp__" %in% pars)
        pars <- c(pars, "lp__")
    }
      
    posterior <-
      rstan::extract(X,
                     pars = pars,
                     permuted = FALSE,
                     inc_warmup = TRUE)
    
    param_dims <- X@sim$dims_oi
    if (!missing(pars)) {
      pd <- which(names(param_dims) %in% pars)
      if (length(pd))
        param_dims <- param_dims[pd]
    }
    
    sso <- shinystan(
      model_name = model_name,
      param_names = dimnames(posterior)[[3L]],
      param_dims = param_dims,
      posterior_sample = posterior,
      summary = .rstan_summary(X, pars = pars),
      sampler_params = .rstan_sampler_params(X),
      n_chain = ncol(X),
      n_iter = nrow(posterior),
      n_warmup = .rstan_warmup(X),
      model_code = rstan::get_stancode(X),
      misc = list(
        max_td = .rstan_max_treedepth(X),
        stan_method = .stan_args(X, "method"),
        stan_algorithm = .stan_algorithm(X),
        sso_version = utils::packageVersion("shinystan")
      )
    )
    sso <- .rename_scalar(sso, oldname = "lp__", newname = "log-posterior")
    if (!is.null(note))
      sso <- suppressMessages(notes(sso, note, replace = TRUE))

    return(sso)
  }
)

# rename a scalar parameter in a shinystan object
.rename_scalar <- function(sso, 
                           oldname = "lp__", 
                           newname = "log-posterior") {
  p <- which(sso@param_names == oldname)
  if (identical(integer(0), p)) 
    return(sso)
  
  sso@param_names[p] <- 
    dimnames(sso@posterior_sample)$parameters[p] <-
    names(sso@param_dims)[which(names(sso@param_dims) == oldname)] <- 
    rownames(sso@summary)[p] <- newname
  return(sso)
}

# Get stan_args from stanfit object
# @param x stanfit object
# @param which which of the entries in x@stan_args[[1]] is of interest? If NULL 
#   the full list x@stan_args is returned
.stan_args <- function(x, which = NULL) {
  stan_args <- x@stan_args[[1L]]
  if (!is.null(which)) 
    return(stan_args[[which]])
  stan_args
}

# Check if model was fit using cmdstan rather than rstan
# @param x stanfit object
.from_cmdstan <- function(x) {
  isTRUE("engine" %in% names(.stan_args(x)))
}

# Check if model fit using variational algorithm
# @param x stanfit object
.used_vb <- function(x) {
  isTRUE(.stan_args(x, "method") == "variational")
}

# Check which algorithm was used to fit model
# @param x stanfit object
.stan_algorithm <- function(x) {
  algo <- if (.from_cmdstan(x))
    toupper(.stan_args(x, "engine")) else .stan_args(x, "algorithm")
  
  if (.used_vb(x) || !(algo %in% c("NUTS", "HMC")))
    warning("Many features are only available for models fit using
            algorithm NUTS or algorithm HMC.", call. = FALSE)
  
  algo
}

# Get summary stats from a stanfit object
# @param x stanfit object
# @param pars optional vector of parameter names
.rstan_summary <- function(x, pars) {
  stan_summary <- rstan::summary(x, pars = pars)$summary
  if (!.used_vb(x))
    return(stan_summary)
  cbind(stan_summary, Rhat = NA, n_eff = NA, se_mean = NA)
}

# Get sampler params from a stanfit object
# @param x stanfit object
.rstan_sampler_params <- function(x) {
  if (.used_vb(x))
    return(list(NA))
  sp <- suppressWarnings(rstan::get_sampler_params(x))
  sp <- .rename_sampler_param(sp, 
                              oldname = "n_divergent__", 
                              newname = "divergent__")
  sp
}

# @param x list of sampler param arrays
.rename_sampler_param <- function(x, oldname, newname) {
  if (!identical(x, list(NA))) {
    for (j in seq_along(x)) {
      sel <- which(colnames(x[[j]]) == oldname)
      if (length(sel))
        colnames(x[[j]])[sel] <- newname
    }
  }
  return(x)
}

# Calculate correct value for number of warmup iterations
# @param x stanfit object
.rstan_warmup <- function(x) {
  warmup <- if (.from_cmdstan(x))
    x@sim$warmup2[1L] else x@sim$warmup
  
  saved <- .stan_args(x, "save_warmup")
  if (!is.null(saved) && !saved) 
    warmup <- 0
  
  if (.from_cmdstan(x)) 
    return(warmup)
  
  floor(warmup / x@sim$thin)
}

# Get value of max_treedepth parameter from stanfit object
# @param x stanfit object
.rstan_max_treedepth <- function(x) {
  cntrl <- .stan_args(x, "control")
  if (is.null(cntrl)) {
    max_td <- 11
  } else {
    max_td <- cntrl$max_treedepth
    if (is.null(max_td))
      max_td <- 11
  }
  max_td
}



# as.shinystan (stanreg) -------------------------------------------------
setOldClass("stanreg")
#' @describeIn as.shinystan Create a shinystan object from a stanreg object 
#'   (\pkg{\link[rstanarm]{rstanarm}}).
#'   
#' @param ppd For stanreg objects (\pkg{rstanarm}), \code{ppd} 
#'   (logical) indicates whether to draw from the posterior predictive 
#'   distribution before launching ShinyStan. The default is \code{TRUE}, 
#'   although for very large objects it can be convenient to set it to 
#'   \code{FALSE} as drawing from the posterior predictive distribution can be 
#'   time consuming. If \code{ppd} is \code{TRUE} then graphical posterior
#'   predictive checks are available when ShinyStan is launched.
#' @param seed Passed to \code{\link[rstanarm]{pp_check}} (\pkg{rstanarm}) if 
#'   \code{ppd} is \code{TRUE}.
#'   
#' @examples
#' \dontrun{
#' ######################
#' ### stanreg object ###
#' ######################
#' library("rstanarm")
#' example("example_model")
#' sso <- as.shinystan(example_model)
#' launch_shinystan(sso)
#' }
#'
setMethod(
  "as.shinystan",
  signature = "stanreg",
  definition = function(X,
                        ppd = TRUE,
                        seed = 1234,
                        model_name = NULL,
                        note = NULL,
                        ...) {
    check_suggests("rstanarm")
    sso <- as.shinystan(X$stanfit, ...)
    
    mname <- if (!is.null(model_name))
      model_name else paste0("rstanarm model (", sso@model_name, ")")
    sso <- suppressMessages(model_name(sso, mname))
    
    if (!is.null(note))
      sso <- suppressMessages(notes(sso, note, replace = TRUE))
    
    param_names <- slot(sso, "param_names")
    sel <- grep(":_NEW_", dimnames(slot(sso, "posterior_sample"))[[3L]], 
                fixed = TRUE)
    if (length(sel)) {
      param_names <- param_names[-sel]
      slot(sso, "posterior_sample") <- 
        slot(sso, "posterior_sample")[, , -sel, drop = FALSE]
      slot(sso, "summary")  <- 
        slot(sso, "summary")[-sel, , drop = FALSE]
    }
    param_dims <- rep(list(numeric(0)), length(param_names))
    names(param_dims) <- param_names
    
    slot(sso, "param_names") <- param_names
    slot(sso, "param_dims") <- param_dims
    slot(sso, "misc")[["stanreg"]] <- TRUE
    if (isTRUE(ppd))
      slot(sso, "misc")[["pp_check_plots"]] <- .rstanarm_pp_checks(X, seed)
    
    return(sso)
  }
)

.rstanarm_pp_checks <- function(X, seed, ...) {
  message(
    "\nHang on... preparing graphical posterior predictive checks for rstanarm model.",
    "\nSee help('shinystan', 'rstanarm') for how to disable this feature."
  )
  ppc <- rstanarm::pp_check
  pp_check_plots <- list()
  
  pp_check_plots[["pp_check_hist"]] <-
    do.call("ppc",
            list(
              object = X,
              check = "dist",
              nreps = 8,
              overlay = FALSE,
              seed = seed
            ))
  pp_check_plots[["pp_check_dens"]] <-
    do.call("ppc",
            list(
              object = X,
              check = "dist",
              nreps = 8,
              overlay = TRUE,
              seed = seed
            ))
  pp_check_plots[["pp_check_resid"]] <-
    do.call("ppc", list(
      object = X,
      check = "resid",
      nreps = 8,
      seed = seed
    ))
  pp_check_plots[["pp_check_scatter"]] <-
    do.call("ppc",
            list(
              object = X,
              check = "scatter",
              nreps = NULL,
              seed = seed
            ))
  pp_check_plots[["pp_check_stat_mean"]] <-
    do.call("ppc",
            list(
              object = X,
              check = "test",
              test = "mean",
              seed = seed
            ))
  pp_check_plots[["pp_check_stat_sd"]] <-
    do.call("ppc", list(
      object = X,
      check = "test",
      test = "sd",
      seed = seed
    ))
  pp_check_plots[["pp_check_stat_min"]] <-
    do.call("ppc", list(
      object = X,
      check = "test",
      test = "min",
      seed = seed
    ))
  pp_check_plots[["pp_check_stat_max"]] <-
    do.call("ppc", list(
      object = X,
      check = "test",
      test = "max",
      seed = seed
    ))
  
  pp_check_plots
}


# as.shinystan (stanjm) -------------------------------------------------
setOldClass("stanjm")
#' @describeIn as.shinystan Create a shinystan object from a stanjm object 
#'   (\pkg{\link[rstanjm]{rstanjm}}).
#'   
#' @param ppd For stanjm objects (\pkg{rstanarm}), \code{ppd} 
#'   (logical) indicates whether to draw from the posterior predictive 
#'   distribution before launching ShinyStan. The default is \code{TRUE}, 
#'   although for very large objects it can be convenient to set it to 
#'   \code{FALSE} as drawing from the posterior predictive distribution can be 
#'   time consuming. If \code{ppd} is \code{TRUE} then graphical posterior
#'   predictive checks are available when ShinyStan is launched.
#' @param seed Passed to \code{\link[rstanjm]{pp_check}} (\pkg{rstanjm}) if 
#'   \code{ppd} is \code{TRUE}.
#'
setMethod(
  "as.shinystan",
  signature = "stanjm",
  definition = function(X,
                        ppd = TRUE,
                        seed = 1234,
                        model_name = NULL,
                        note = NULL,
                        ...) {
    check_suggests("rstanjm")
    sso <- as.shinystan(X$stanfit, ...)
    
    mname <- if (!is.null(model_name))
      model_name else paste0("Fitted joint model")
    sso <- suppressMessages(model_name(sso, mname))
    
    if (!is.null(note))
      sso <- suppressMessages(notes(sso, note, replace = TRUE))
    
    param_names <- slot(sso, "param_names")
    sel <- grep(":_NEW_", dimnames(slot(sso, "posterior_sample"))[[3L]], 
                fixed = TRUE)
    if (length(sel)) {
      param_names <- param_names[-sel]
      slot(sso, "posterior_sample") <- 
        slot(sso, "posterior_sample")[, , -sel, drop = FALSE]
      slot(sso, "summary")  <- 
        slot(sso, "summary")[-sel, , drop = FALSE]
    }
    param_dims <- rep(list(numeric(0)), length(param_names))
    names(param_dims) <- param_names
    
    slot(sso, "param_names") <- param_names
    slot(sso, "param_dims") <- param_dims
    slot(sso, "misc")[["stanjm"]] <- TRUE
    if (isTRUE(ppd))
      slot(sso, "misc")[["pp_check_plots"]] <- .rstanjm_pp_checks(X, seed)
    
    return(sso)
  }
)

.rstanjm_pp_checks <- function(X, seed, ...) {
  message(
    "\nHang on... preparing graphical posterior predictive checks for rstanarm model.",
    "\nSee help('shinystan', 'rstanarm') for how to disable this feature."
  )
  ppc <- rstanjm::pp_checkLong
  pp_check_plots <- list()
  
  pp_check_plots[["pp_check_hist"]] <-
    do.call("ppc",
            list(
              object = X,
              check = "dist",
              nreps = 8,
              overlay = FALSE,
              seed = seed
            ))
  pp_check_plots[["pp_check_dens"]] <-
    do.call("ppc",
            list(
              object = X,
              check = "dist",
              nreps = 8,
              overlay = TRUE,
              seed = seed
            ))
  pp_check_plots[["pp_check_resid"]] <-
    do.call("ppc", list(
      object = X,
      check = "resid",
      nreps = 8,
      seed = seed
    ))
  pp_check_plots[["pp_check_scatter"]] <-
    do.call("ppc",
            list(
              object = X,
              check = "scatter",
              nreps = NULL,
              seed = seed
            ))
  pp_check_plots[["pp_check_stat_mean"]] <-
    do.call("ppc",
            list(
              object = X,
              check = "test",
              test = "mean",
              seed = seed
            ))
  pp_check_plots[["pp_check_stat_sd"]] <-
    do.call("ppc", list(
      object = X,
      check = "test",
      test = "sd",
      seed = seed
    ))
  pp_check_plots[["pp_check_stat_min"]] <-
    do.call("ppc", list(
      object = X,
      check = "test",
      test = "min",
      seed = seed
    ))
  pp_check_plots[["pp_check_stat_max"]] <-
    do.call("ppc", list(
      object = X,
      check = "test",
      test = "max",
      seed = seed
    ))
  
  pp_check_plots
}




