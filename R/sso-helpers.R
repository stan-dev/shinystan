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


# sso_info ----------------------------------------------------------------
#' Print summary of shinystan object
#'
#' Prints basic summary info including number of parameters, chains, iterations, 
#' warmup iterations, etc. 
#'
#' @export
#' @template args-sso
#' @examples 
#' # Using example shinystan object 'eight_schools'
#' sso_info(eight_schools)
#'
sso_info <- function(sso) {
  sso_check(sso)
  sso_name <- deparse(substitute(sso))
  has_notes <- 
    sso@user_model_info != "Use this space to store notes about your model"
  has_code <- 
    sso@model_code != "Use this space to store your model code" 
  
  cat(
    sso_name, "---------------------",
    paste("Model name:", sso@model_name),
    paste("Parameters:", length(sso@param_names)),
    paste("Parameter groups:", length(names(sso@param_dims))),
    paste("Chains:", sso@nChains),
    paste("Iterations:", sso@nIter),
    paste("Warmup:", sso@nWarmup),
    paste("Has model code:", has_code),
    paste("Has user notes:", has_notes),
    sep = "\n"
  )
}



# model_code --------------------------------------------------------------
#' Add model code to a shinystan object or see the code currently stored in 
#' a shinystan
#'
#' @export
#' @template args-sso
#' @param code Optionally, the code you want to add. See \strong{Details} below for 
#'   formatting instructions.
#' @return If \code{code} is missing then any code currently stored in
#'   \code{sso} is returned as a character string. If \code{code} is specified
#'   then then any previous code is overwritten by the text in \code{code} and
#'   an updated shinystan object is returned.
#'   
#' @details If \code{code} is specified it should be be a character string that
#'   can be used as an argument to \code{cat}. See \strong{Examples}, below.
#' @note For \pkg{rstan} users the model code will be automatically taken
#' from the stanfit object. 
#' 
#' @seealso \code{cat}
#'
#' @examples
#' # View model code in example shinystan object 'eight_schools'
#' cat(model_code(eight_schools))
#' 
#' # Below, assume sso is a shinystan object created using draws obtained 
#' # from fitting a model using JAGS
#' \dontrun{
#' # Some JAGS-style code we might want to add
#' my_code <- "
#'  model {
#'    for (i in 1:length(Y)) {
#'      Y[i] ~ dpois(lambda[i])
#'      log(lambda[i]) <- inprod(X[i,], theta[])
#'    }
#'    for (j in 1:J) {
#'      theta[j] ~ dt(0.0, 1.0, 1.0)
#'    }
#'  }
#' "
#' # Add the code to a shinystan object sso
#' sso <- model_code(sso, my_code)
#'}
#'
model_code <- function(sso, code = NULL) {
  sso_check(sso)
  validate_model_code(code)
  
  if (is.null(code))
    return(slot(sso, "model_code"))
  
  slot(sso, "model_code") <- code
  message(paste0("Successfully added code.", 
                 "\nYou can view the code in the", 
                 "ShinyStan GUI on the 'Model Code' page."))
  sso
}

validate_model_code <- function(code) {
  if (is.null(code) || is.character(code)) {
    invisible(TRUE) 
  } else {
    stop("Model code should be NULL or a string", call. = FALSE)
  }
}



# notes -------------------------------------------------------------------
#' Additional or replacement notes
#' 
#' Notes are viewable on ShinyStan's Notepad page
#' 
#' @export
#' @template args-sso
#' @param note Optional. A character string containing a note to add to any
#'   existing notes or replace existing notes, depending on the value of 
#'   \code{replace}.
#' @param replace If \code{TRUE} the existing notes are overwritten by 
#'   \code{note} if \code{note} is specified. If \code{FALSE} (the default) 
#'   if \code{note} is specified then its content is appended to the existing
#'   notes.
#' @return If \code{note} is missing then any existing notes stored in 
#'   \code{sso} are returned as a character string. If \code{note} is specified
#'   then an updated shinystan object is returned with either \code{note} added
#'   to the previous notes (if \code{replace=FALSE}) or overwritten by
#'   \code{note} (if \code{replace = TRUE}).
#'   
#' 
#' @seealso \code{\link{as.shinystan}}
#' @examples 
#' # Using example shinystan object 'eight_schools'
#' sso <- eight_schools
#' 
#' # View existing notes
#' notes(sso)
#' 
#' # Add a note to the existing notes
#' sso <- notes(sso, "New note")
#' notes(sso)
#' cat(notes(sso))
#' 
#' # Replace existing notes
#' sso <- notes(sso, "replacement note", replace = TRUE)
#' notes(sso)
#'  
notes <- function(sso, note, replace = FALSE) {
  sso_check(sso)
  if (missing(note))
    return(slot(sso, "user_model_info"))
  
  slot(sso, "user_model_info") <- if (replace) 
    note else c(slot(sso, "user_model_info"), paste0("\n\n", note))
  
  message(paste("Successfully added note.", "\nYou can view the notes in the", 
                "ShinyStan GUI on the 'Notepad' page."))
  sso
}



# rename_model ------------------------------------------------------------
#' Change the model name associated with a shinystan object
#' 
#' @template args-sso
#' @param new_model_name Character string giving the new model name to use.
#' @return sso, updated.
#' @seealso \code{\link{as.shinystan}}
#' @export
#' @examples
#' # Using example shinystan object 'eight_schools'
#' sso <- eight_schools
#' sso@model_name
#' sso <- rename_model(sso, "New name")
#' sso@model_name
#' 
rename_model <- function(sso, new_model_name) {
  sso_check(sso)
  sso@model_name <- new_model_name
  sso
}



#' Update an object created by the previous version of shinystan
#' 
#' If you encounter any errors when using an old shinystan object (\code{sso}) 
#' created by the previous version of \pkg{shinystan} you might need to run
#' \code{update_sso}. If \code{update_sso} does not resolve the problem and 
#' you still have the object (e.g. stanfit,  stanreg, mcmc.list) from which 
#' \code{sso} was originally created, you can create a new shinystan object 
#' using \code{\link{as.shinystan}}.
#' 
#' @export
#' @param sso An old shinystan object to update.
#' @return \code{sso}, updated.
#' 
update_sso <- function(sso) {
  stopifnot(is.shinystan(sso))
  sso@sampler_params <- .rename_sampler_param(sso@sampler_params, 
                                              oldname = "n_divergent__", 
                                              newname = "divergent__")
  sso
}

