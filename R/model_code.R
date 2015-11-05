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


#' Add model code to a shinystan object or see the code currently stored in 
#' a shinystan
#'
#' @export
#' @param sso A shinystan object.
#' @param code Optionally, the code you want to add. See \strong{Details} below for 
#'   formatting instructions.
#' @return If \code{code} is missing then any code currently stored in
#'   \code{sso} is returned as a character string. If \code{code} is specified
#'   then then any previous code is overwritten by the text in \code{code} and
#'   an updated shinystan object is returned.
#'   
#' @details If \code{code} is specified it should be be a character string that
#'   can be used as an argument to \code{cat}. See \strong{Examples}, below.
#' @note For \pkg{rstan} users the model code will be automatically taken
#' from the stanfit object. 
#' 
#' @seealso \code{cat}
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
#' # Add the code to a shinystan object sso
#' sso <- model_code(sso, my_code)
#' 
#' # View the code currently stored in sso
#' model_code(sso)
#'
#'}

model_code <- function(sso, code) {
  sso_check(sso)
  if (missing(code)) {
    return(slot(sso, "model_code"))
  } 
  
  if (!is.character(code)) 
    stop("'code' should be a character string.")
  slot(sso, "model_code") <- code
  message(paste0("Successfully added code.", "\nYou can view the code in the", 
                 "ShinyStan GUI on the 'Model Code' page."))
  sso
}
