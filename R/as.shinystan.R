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

#' Create and test shinystan objects
#'
#' @export
#' @param X An object to be converted to a shinystan object. Can be
#' one of the following:
#' \describe{
#'   \item{stanfit}{An object of class stanfit (\pkg{rstan})}
#'   \item{stanreg}{An object of class stanreg (\pkg{rstanarm})}
#'   \item{mcmc.list}{An object of class \code{mcmc.list} (\pkg{coda})}
#'   \item{3D array}{A 3D array of posterior simulations with dimensions corresponding
#'   to iterations, chains, and parameters, in that order.}
#'   \item{chain list}{A list of matrices/2D arrays each corresponding to a single chain,
#'   and with dimensions corresponding to iterations (rows) and parameters (columns).
#'   }
#' }
#' @param object An object to test.
#' @param ... Additional arguments. See Details, below.
#'   
#' @return For \code{as.shinystan} an object of class shinystan that can be used
#'   with \code{\link{launch_shinystan}}. For \code{is.shinystan} a logical value
#'   indicating whether the tested object is a shinystan object.
#'   
#' @details If \code{X} is a stanfit object then no additional arguments should
#'   be specified in \code{...} (they are taken automatically from the stanfit
#'   object). 
#'   
#'   If \code{X} is a stanreg object the argument \code{ppd} (logical)
#'   can be specified indicating whether to draw from the posterior predictive
#'   distribution before launching ShinyStan. The default is \code{TRUE}, 
#'   although for large objects it can be wise to set it to \code{FALSE} as 
#'   drawing from the posterior predictive distribution can be time consuming.
#'   
#'   If \code{X} is not a stanfit or stanreg object then the following arguments
#'   can be specified but are not required:
#'   
#' \describe{
#'   \item{\code{model_name}}{A character string giving a name for the model.}
#'   \item{\code{burnin}}{The number of burnin (warmup) iterations. \code{burnin}
#'   should only be specified if the burnin samples are included in \code{X}.}
#'   \item{\code{param_dims}}{Rarely used and never necessary. A named list
#'   giving the dimensions for all parameters. (For scalar parameters use
#'   \code{0} as the dimension.) This allows shinystan to group parameters in
#'   vectors/arrays/etc together for certain features. See \strong{Examples}.}
#'   \item{\code{model_code}}{A character string with the code for your model.}
#' }
#' 
#' @seealso \code{\link{launch_shinystan}}, \code{\link{launch_shinystan_demo}}
#'
#' @examples
#' \dontrun{
#' #################
#' ### Example 1 ###
#' #################
#'
#' # If X is a mcmc.list, 3D array or list of 2D chains then just do:
#' X_sso <- as.shinystan(X, ...) # replace ... with optional arguments or omit it
#' 
#' # You can also do the above if X is a stanfit object although it is not
#' # necessary since launch_shinystan accepts stanfit objects. 
#'
#'
#' ##############################################
#' ### Example 2: if X is a list of 2D chains ###
#' ##############################################
#'
#' # Generate some fake data
#' chain1 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
#' chain2 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
#' X <- list(chain1, chain2)
#' X_sso <- as.shinystan(X)
#'
#' # We can also specify some or all of the optional arguments
#' # note: in order to use param_dims we need to rename 'beta1' and 'beta2'
#' # to 'beta[1]' and 'beta[2]'
#' colnames(chain1) <- colnames(chain2) <- c(paste0("beta[",1:2,"]"), "sigma")
#' X_sso <- as.shinystan(X, param_dims = list(beta = 2, sigma = 0),
#'                          model_name = "Example",
#'                          burnin = 0)
#' launch_shinystan(my_shinystan)
#'}

as.shinystan <- function(X, ...) {
  Xname <- deparse(substitute(X))
  if (is.shinystan(X)) {
    message(paste0(Xname, " is already a shinystan object.\n",
             "You can use launch_shinystan(", Xname, ") to launch ShinyStan."))
    return(X)
  }
  X_is <- get_type(X)
  if (X_is == "stanfit") return(stan2shinystan(X, ...))
  if (X_is == "stanreg") return(stanreg2shinystan(X, ...))
  if (X_is == "mcmclist") return(mcmc2shinystan(X, ...))
  if (X_is == "chainlist") return(chains2shinystan(X, ...))
  if (X_is == "other") {
    if (!is.array(X)) 
      stop(paste(Xname, "is not a valid input type. See ?as.shinystan"))
    array2shinystan(X, ...)
  }
}

#' @rdname as.shinystan
is.shinystan <- function(object) inherits(object, "shinystan")
