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

sso_check <- function(sso) {
  if (!is.shinystan(sso)) 
    stop("Please specify a shinystan object", call. = FALSE)
  else 
    invisible(TRUE)
}
is.stanfit <- function(X) inherits(X, "stanfit")
is.stanreg <- function(X) inherits(X, "stanreg")

check_suggests <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) 
    stop("You need to have the ", pkg," package installed to use this option.", 
         call. = FALSE)
}

# grepl with ignore.case defaulting to TRUE
grepl_ic <- function(pattern, x, ignore.case = TRUE) {
  grepl(pattern = pattern, x = x, ignore.case = ignore.case)
}


# mcmclist to matrix (adapted from Coda package) --------------------------
mcmclist2matrix <- function(x) {
  out <- matrix(nrow = coda::niter(x) * coda::nchain(x), ncol = coda::nvar(x))
  cols <- 1:coda::nvar(x)
  for (i in 1:coda::nchain(x)) {
    rows <- (i-1)*coda::niter(x) + 1:coda::niter(x)
    out[rows, cols] <- x[[i]]
  }
  rownames <- character(ncol(out))
  rownames[cols] <- coda::varnames(x, allow.null = FALSE)
  dimnames(out) <- list(NULL, rownames)
  out
}

# nocov start
# release reminders (for devtools)
release_questions <- function() {
  c(
    "Have you updated version numbers in the citation?"
  )
}
# nocov end
