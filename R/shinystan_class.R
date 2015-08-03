# This file is part of shinystan
# Copyright (C) Jonah Gabry
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

#' S4 shinystan objects (sso)
#' 
#' @description shinystan objects
#' 
#' @section Creating shinystan objects:
#' 
#' See \code{\link{as.shinystan}}.
#' 
#' @section Turning shinystan objects into custom ShinyStan apps hosted online:
#' 
#' See \code{\link{deploy_shinystan}}.
#'   
#' @section Functions for manipulating shinystan objects:
#' 
#' 
#' \code{\link{notes}} Add to or replace notes
#' 
#' \code{\link{rename_model}} Change the model name.
#' 
#' \code{\link{generate_quantity}} Add new parameters/quantities as a function of one or two existing parameters.
#' 
#' \code{\link{model_code}} Add or change model code.
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
                                       user_model_info = "Use this space to store notes about your model.",
                                       model_code = "No code found. After closing shinyStan you can use the include_model_code function in R to add your code.",
                                       misc = list()
                      ))
