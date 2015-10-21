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

#' Change the model name associated with a shinystan object
#' 
#' @param sso shinystan object.
#' @param new_model_name Character string giving the new model name to use.
#' @return sso, updated.
#' @seealso \code{\link{as.shinystan}}
#' @export
#' @examples
#' \dontrun{
#' sso <- rename_model(sso, "new name for model")
#' }
#' 

rename_model <- function(sso, new_model_name) {
  sso_check(sso)
  sso@model_name <- new_model_name
  sso
}