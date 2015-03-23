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


#' Add model code to a \code{shinystan} object
#'
#' @param sso A \code{shinystan} object.
#' @param code The code you want to add. See \strong{Details} below for
#' formatting instructions.
#' @return The \code{shinystan} object \code{sso} with \code{code} in the
#' \code{model_code} slot.
#'
#' @details \code{code} should be a character string that can be used as an argument
#' to \code{cat}. See \strong{Examples}, below.
#' @note This is intended for users who did not run their models using \pkg{rstan}.
#' For \pkg{rstan} users the model code will be automatically available. 
#' 
#' @seealso \code{cat}
#' @export
#'
#' @examples
#' \dontrun{
#' # Some JAGS-style code we might want to add
#' my_code <- "
#'  model {
#'    for (i in 1:length(Y)) {
#'      Y[i] ~ dpois(lambda[i])
#'      log(lambda[i]) <- inprod(X[i,], theta[])
#'    }
#'    for (j in 1:J) {
#'      theta[j] ~ dt(0.0, 1.0, 1.0)
#'    }
#'  }
#' "
#'
#' # Add the code to a shinystan object X
#' X <- include_model_code(X, my_code)
#' launch_shinystan(X) # code is visible in the Model Code tab
#'
#'}

include_model_code <- function(sso, code) {
  sso_name <- deparse(substitute(sso))
  if (!is.shinystan(sso)) stop(paste(sso_name, "is not a shinystan object."))
  if (!is.character(code)) stop("'code' should be a character string.")
  
  sso@model_code <- code
  
  message(paste0("Successfully added code to ", sso_name, 
                 ". \nYou can view the added code in the shinyStan GUI on the 'Model Code' page."))
  sso
}
