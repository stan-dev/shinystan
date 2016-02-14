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

#' Drop parameters from a shinystan object
#'
#' @export
#' @param sso A shinystan object.
#' @param pars A character vector of parameter names. For a non-scalar parameter
#'   \code{theta}, all elements will be removed if \code{"theta"} is included in
#'   \code{pars}. A subset of the elements of \code{theta} can be removed by 
#'   naming just those elements, e.g. \code{pars <- c("theta[1]", "theta[4]")}.
#' @return \code{sso} with \code{pars} dropped.
#' 
drop_parameters <- function(sso, pars) {
  stopifnot(is.character(pars))
  any_pd <- any(names(sso@param_dims) %in% pars)
  if (any_pd) {
    pd <- which(names(sso@param_dims) %in% pars)
    nms <- names(sso@param_dims[pd])
    for (j in seq_along(nms)) {
      if (!nms[j] %in% sso@param_names) {
        pars <- pars[pars != nms[j]]
        pars <- c(pars, grep(paste0(nms, "["), sso@param_names, value = TRUE))
      }
    }
    sso@param_dims <- sso@param_dims[-pd]
  }
  
  sel <- match(pars, sso@param_names)
  if (!any_pd && all(is.na(sel))) {
    stop("No matches for 'pars' were found.", call. = FALSE)
  } else if (any(is.na(sel))) {
    warning(paste("Some 'pars' not found and ignored:",
                  paste(pars[is.na(sel)], collapse = ", ")), call. = FALSE)
  }
  
  .drop_parameters(sso, na.omit(sel))
}


# @param rmv A vector of indices indicating the positions of parameters to be
#   removed
.drop_parameters <- function(sso, rmv) {
  sso@param_names <- sso@param_names[-rmv]
  sso@samps_all <- sso@samps_all[, ,-rmv, drop = FALSE]
  sso@summary <- sso@summary[-rmv, , drop = FALSE]
  sso
}
