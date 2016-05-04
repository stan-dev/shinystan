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

# check object types
sso_check <- function(sso) {
  if (!is.shinystan(sso)) {
    stop("Please specify a shinystan object", call. = FALSE)
  } else if (sso_version(sso) < utils::packageVersion("shinystan")) {
    stop("Your shinystan object was created with a previous version of shinystan. ", 
         "Please use the 'update_sso' function to update your object.", 
         call. = FALSE)
  }
  invisible(TRUE)
}
is.stanfit <- function(X) inherits(X, "stanfit")
is.stanreg <- function(X) inherits(X, "stanreg")

sso_version <- function(sso) {
  ver <- sso@misc[["sso_version"]]
  if (!is.null(ver)) {
    package_version(ver)
  } else {
    package_version("2.0")
  }
}

# check for suggested (not required) packages
check_suggests <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) 
    stop("You need to have the ", pkg," package installed to use this option.", 
         call. = FALSE)
}

# grepl with ignore.case defaulting to TRUE
grepl_ic <- function(pattern, x, ignore.case = TRUE) {
  grepl(pattern = pattern, x = x, ignore.case = ignore.case)
}


# nocov start
# release reminders (for devtools)
release_questions <- function() {
  c(
    "Have you updated version numbers in the citation?"
  )
}
# nocov end
