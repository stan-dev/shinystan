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


# Convert list of chains to shinystan object
# 
# @param chain_list A list of 2D-arrays or matrices of iterations (rows) and parameters (columns). 
# Each chain in chain_list should have the same number of iterations and the same parameters 
# (with the same names and in the same order).
# @param ... Arguments to pass to \code{array2shinystan}
# 
# @return An object of class \code{shinystan} that can be used with 
# \code{\link[shinyStan]{launch_shinystan}}. 
# 

chains2shinystan <- function(chain_list, ...) {
  if (!is.list(chain_list)) {
    name <- deparse(substitute(chain_list))
    stop(paste(name, "is not a list."), call. = FALSE)
  }
  nChain <- length(chain_list)
  for (i in 1:nChain) {
    nms <- colnames(chain_list[[i]])
    if (is.null(nms) || !all(nzchar(nms)))
      stop("Some parameters are missing names. ",
           "Check the column names for the matrices in your list of chains.")
  }
  if (nChain > 1) {
    nIter <- sapply(chain_list, nrow)
    same_iters <- length(unique(nIter)) == 1
    if (!same_iters) 
      stop("Each chain should contain the same number of iterations.")
    cnames <- sapply(chain_list, colnames)
    if (is.array(cnames)) {
      same_params <- identical(cnames[,1], cnames[,2])
      param_names <- cnames[,1]
    } else {
      same_params <- length(unique(cnames)) == 1
      param_names <- cnames
    }
    if (!same_params) 
      stop("The parameters for each chain should be in the same order ",
           "and have the same names.")
    nIter <- nIter[1]
  } else {
    if (nChain == 1) {
      nIter <- nrow(chain_list[[1]])
      param_names <- colnames(chain_list[[1]])
    } else {
      stop("You don't appear to have any chains.")
    }
  }
  param_names <- unique(param_names)
  nParam <- length(param_names)
  out <- array(NA, dim = c(nIter, nChain, nParam))
  for(i in 1:nChain) {
    out[,i,] <- chain_list[[i]]
  }
  dimnames(out) <- list(iterations = NULL,
                        chains = paste0("chain:", 1:nChain),
                        parameters = param_names)
  array2shinystan(out, ...)
}
