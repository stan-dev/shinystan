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

#' \code{shinystan} Objects
#'
#' @param X An object to be converted to a \code{shinystan} object. Can be
#' one of the following:
#' \describe{
#'   \item{stanfit}{An object of class \code{stanfit} (\pkg{rstan})}
#'   \item{mcmc.list}{An object of class \code{mcmc.list} (\pkg{coda})}
#'   \item{3D array}{A 3D array of posterior simulations with dimensions corresponding
#'   to iterations, chains, and parameters, in that order.}
#'   \item{chain list}{A list of matrices/2D arrays each corresponding to a single chain,
#'   and with dimensions corresponding to iterations (rows) and parameters (columns).
#'   }
#' }
#'
#' @param Y An object to test.
#' @param ... Additional arguments. See \strong{Details}, below, for instructions.
#' @return For \code{as.shinystan} an object of class \code{shinystan} that can
#' be used with \code{\link[shinyStan]{launch_shinystan}}. For
#' \code{is.shinystan} a logical value indicating whether the tested object
#' is a \code{shinystan} object.
#' @details If \code{X} is a \code{stanfit} object then no additional arguments
#' should be specified in \code{...} (they are taken automatically from the \code{stanfit}
#' object). If \code{X} is not a \code{stanfit} object then the following arguments can be
#' specified but are not required:
#' \describe{
#'   \item{\code{model_name}}{A character string giving a name for the model.}
#'   \item{\code{burnin}}{The number of burnin (warmup) iterations. \code{burnin}
#'   should only be specified if the burnin samples are included in \code{X}.}
#'   \item{\code{param_dims}}{Rarely used and never necessary. A named list giving the dimensions for all parameters.
#'   For scalar parameters use \code{0} as the dimension. See \strong{Examples}.}
#'   \item{\code{model_code}}{A character string with the code you used to run your model.
#'   This can also be added to your \code{shinystan} object later using the
#'   \code{\link[shinyStan]{include_model_code}} function. See \code{\link[shinyStan]{include_model_code}}
#'   for additional formatting instructions. After launching the app \code{model_code}
#'   will be viewable in the \strong{Model Code} tab.}
#' }
#' @seealso \code{\link[shinyStan]{launch_shinystan}}, \code{\link[shinyStan]{launch_shinystan_demo}}
#' @export
#'
#' @examples
#' \dontrun{
#' #################
#' ### Example 1 ###
#' #################
#'
#' # If X is a stanfit, mcmc.list, 3D array
#' # or list of 2D chains then just do:
#' X_shinystan <- as.shinystan(X)
#' launch_shinystan(X_shinystan)
#'
#'
#' ##############################################
#' ### Example 2: if X is a list of 2D chains ###
#' ##############################################
#'
#' # Generate some fake data
#' chain1 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
#' chain2 <- cbind(beta1 = rnorm(100), beta2 = rnorm(100), sigma = rexp(100))
#'
#' # We can make a shinystan object without specifying any optional arguments
#' my_shinystan <- as.shinystan(X = list(chain1, chain2))
#' launch_shinystan(my_shinystan)
#'
#' # We can also specify some or all of the optional arguments
#'  # note: in order to use param_dims we need to rename 'beta1' and 'beta2'
#'  # to 'beta[1]' and 'beta[2]'
#' colnames(chain1) <- colnames(chain2) <- c(paste0("beta[",1:2,"]"), "sigma")
#' my_shinystan <- as.shinystan(X = list(chain1, chain2),
#'                              param_dims = list(beta = 2, sigma = 0),
#'                              model_name = "example",
#'                              burnin = 0
#'                              )
#' launch_shinystan(my_shinystan)
#'}

as.shinystan <- function(X, ...) {
  get_type <- function(x) {
    if (inherits(x, "shinystan")) return("shinystan")
    if (inherits(x, "stanfit")) return("stanfit")
    if (inherits(x, "mcmc.list")) return("mcmclist")
    if (is.list(x) & !inherits(x, "mcmc.list")) return("chainlist")
    return("other")
  }

  Xname <- deparse(substitute(X))
  what_X_is <- get_type(X)

  if (what_X_is == "shinystan") {
    print(paste0(Xname,
               " is already a shinystan object.", "\n",
               " You can use launch_shinystan(",Xname,") to launch shinyStan."))
    return(X)
  }
  if (what_X_is == "stanfit") return(stan2shinystan(X, ...))
  if (what_X_is == "mcmclist") return(mcmc2shinystan(X, ...))
  if (what_X_is == "chainlist") return(chains2shinystan(X, ...))
  if (what_X_is == "other") {
    if (!is.array(X)) stop(paste(Xname, "is not a valid input type. See ?as.shinystan"), call. = FALSE)
    array2shinystan(X, ...)
  }

}

#' @rdname as.shinystan
is.shinystan <- function(Y) inherits(Y, "shinystan")
