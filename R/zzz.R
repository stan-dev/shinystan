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

.onAttach <- function(...) {
  ver <- utils::packageVersion("shinyStan")
  msg <- paste0("\nThis is shinyStan version ", ver, 
                "\nTo check if a newer version is available visit github.com/stan-dev/shinystan/releases. \n")
  # check shinyBS version
  sbs_version <- utils::packageVersion("shinyBS") 
  if (sbs_version != "0.50.1") {
    msg <- paste0(msg, 
                  "\nMessage:", 
                  "\nshinyStan runs best with version ",
                  "0.50.1 of the shinyBS package (you ",
                  "have version ", sbs_version, ").",
                  "\nTo install the preferred version ",
                  "restart R and then run ",
                  "\ndevtools::install_github('jgabry/shinyBS@shinyBS_for_shinyStan')"
    )
  }
  packageStartupMessage(msg)
} 

.onLoad <- function(libname, pkgname) { }