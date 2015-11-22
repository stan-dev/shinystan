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

.sso_env <- new.env(parent=emptyenv()) 

.onAttach <- function(...) {
  ver <- utils::packageVersion("shinystan")
  msg <- paste0("\nThis is shinystan version ", ver,"\n")
  packageStartupMessage(msg)
} 

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.shinystan <- list(
    shinystan.rstudio = FALSE
  )
  set_ops <- !(names(op.shinystan) %in% names(op))
  if (any(set_ops)) options(op.shinystan[set_ops])
  invisible()
}
