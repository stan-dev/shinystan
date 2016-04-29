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

#' S4 shinystan objects
#' 
#' @aliases shinystan-class
#' @description See \code{\link{as.shinystan}} for documentation on creating 
#'   shinystan objects and \code{\link{eight_schools}} for an example object.
#'   
shinystan <- setClass("shinystan",
                      slots = list(model_name      = "character",
                                   param_names     = "character",
                                   param_dims      = "list",
                                   samps_all       = "array",
                                   summary         = "matrix",
                                   sampler_params  = "list",
                                   nChains         = "numeric",
                                   nIter           = "numeric",
                                   nWarmup         = "numeric",
                                   user_model_info = "character",
                                   model_code      = "character",
                                   misc            = "list"
                      ),
                      prototype = list(model_name = "No name",
                                       param_names = "",
                                       param_dims = list(),
                                       samps_all = array(NA, c(1,1)),
                                       summary = matrix(NA, nr=1,nc=1),
                                       sampler_params = list(NA),
                                       nChains = 0,
                                       nIter = 0,
                                       nWarmup = 0,
                                       user_model_info = 
                                         "Use this space to store notes about your model",
                                       model_code = 
                                         "Use this space to store your model code",
                                       misc = list()
                      ))
