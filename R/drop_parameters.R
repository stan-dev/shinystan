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


#' Drop parameters from a shinystan object
#' 
#' Remove selected parameters from a shinystan object. This is useful if you 
#' have a very large shinystan object when you only want to look at a subset of 
#' parameters. With a smaller shinystan object, \code{\link{launch_shinystan}} 
#' will be faster and you should experience better performance (responsiveness) 
#' after launching when using the ShinyStan app.
#'
#' @export
#' @template args-sso
#' @param pars A character vector of parameter names. If the name of a 
#'   non-scalar (e.g. vector, matrix) parameter is included in \code{pars} all 
#'   of its elements will be removed. Currently it is not possible to remove 
#'   only a subset of the elements of a non-scalar parameter.
#' @return \code{sso}, with \code{pars} dropped.
#' 
#' @examples 
#' # Using example shinystan object 'eight_schools'
#' print(eight_schools@param_names)
#' 
#' # Remove the scalar parameters mu and tau
#' sso <- drop_parameters(eight_schools, pars = c("mu", "tau"))
#' print(sso@param_names)
#' 
#' # Remove all elements of the parameter vector theta
#' sso <- drop_parameters(sso, pars = "theta")
#' print(sso@param_names)
#' 
drop_parameters <- function(sso, pars) {
  sso_check(sso)
  stopifnot(is.character(pars))
  any_indiv_els <- any(grepl("[", pars, fixed = TRUE))
  if (any_indiv_els)
    stop("Currently, individual elements of non-scalar parameters can't be removed.")
  
  any_non_scalar <- any(names(sso@param_dims) %in% pars)
  if (any_non_scalar) {
    param_dims <- slot(sso, "param_dims")
    param_names <- slot(sso, "param_names")
    pd <- which(names(param_dims) %in% pars)
    nms <- names(param_dims[pd])
    for (j in seq_along(nms)) {
      if (!nms[j] %in% param_names) {
        pars <- pars[pars != nms[j]]
        tmp <- grep(paste0(nms[j], "["), param_names, fixed = TRUE, value = TRUE)
        pars <- c(pars, tmp)
      }
    }
    slot(sso, "param_dims") <- slot(sso, "param_dims")[-pd]
  }
  
  sel <- match(pars, slot(sso, "param_names"))
  if (!any_non_scalar && all(is.na(sel))) {
    stop("No matches for 'pars' were found.", call. = FALSE)
  } else if (any(is.na(sel))) {
    warning(paste(
      "Some 'pars' not found and ignored:",
      paste(pars[is.na(sel)], collapse = ", ")
    ))
  }
  
  .drop_parameters(sso, na.omit(sel))
}


# @param rmv A vector of indices indicating the positions of parameters to be
#   removed
.drop_parameters <- function(sso, rmv) {
  slot(sso, "param_names") <- slot(sso, "param_names")[-rmv]
  slot(sso, "posterior_sample") <- slot(sso, "posterior_sample")[, , -rmv, drop = FALSE]
  slot(sso, "summary") <- slot(sso, "summary")[-rmv, , drop = FALSE]
  sso
}
