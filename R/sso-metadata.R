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


#' View or change metadata associated with a shinystan object
#' 
#' @name shinystan-metadata
#' @template args-sso
#' 
#' @template seealso-as.shinystan
#' @template seealso-drop_parameters
#' @template seealso-generate_quantity
#' 
#' @examples 
#' # use eight_schools example object
#' sso <- eight_schools
#' 
NULL

# sso_info ----------------------------------------------------------------
#' @rdname shinystan-metadata
#' @export
#' 
#' @return \code{sso_info} prints basic metadata including number of parameters, 
#'   chains, iterations, warmup iterations, etc. It does not return anything.
#' 
#' @examples 
#' ################
#' ### sso_info ###
#' ################
#' 
#' sso_info(sso)
#'
sso_info <- function(sso) {
  sso_check(sso)
  sso_name <- deparse(substitute(sso))
  has_notes <-
    sso@user_model_info != "Use this space to store notes about your model"
  has_code <-
    sso@model_code != "Use this space to store your model code"
  
  cat(
    sso_name,
    "---------------------",
    paste("Model name:", sso@model_name),
    paste("Parameters:", length(sso@param_names)),
    paste("Parameter groups:", length(names(sso@param_dims))),
    paste("Chains:", sso@n_chain),
    paste("Iterations:", sso@n_iter),
    paste("Warmup:", sso@n_warmup),
    paste("Has model code:", has_code),
    paste("Has user notes:", has_notes),
    sep = "\n"
  )
}



# model_code --------------------------------------------------------------
#' @rdname shinystan-metadata
#' @export
#' @param code A string, containing model code to be added, that can be
#'   used as an argument to \code{\link{cat}}. See \strong{Examples}.
#'   
#' @return \code{model_code} returns or replaces model code stored in a 
#'   shinystan object. If \code{code} is \code{NULL} then any existing model
#'   code stored in \code{sso} is returned as a character string. If \code{code}
#'   is specified then an updated shinystan object is returned with \code{code}
#'   added. For shinystan objects created from stanfit (\pkg{rstan}) and stanreg
#'   (\pkg{rstanarm}) objects, model code is automatically taken from that
#'   object and does not need to be added manually. From within the ShinyStan
#'   interface model code can be viewed on the \strong{Model Code} page.
#'
#' @examples
#' ##################
#' ### model_code ###
#' ##################
#' 
#' # view model code in example shinystan object 'eight_schools'
#' cat(model_code(sso))
#' 
#' # change the model code in sso 
#' # some jags style code
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
#' sso <- model_code(sso, my_code)
#' cat(model_code(sso))
#'
model_code <- function(sso, code = NULL) {
  sso_check(sso)
  validate_model_code(code)
  
  if (is.null(code))
    return(slot(sso, "model_code"))
  
  slot(sso, "model_code") <- code
  message(
    paste0(
      "Successfully added code.",
      "\nYou can view the code in the",
      "ShinyStan GUI on the 'Model Code' page."
    )
  )
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
#' @rdname shinystan-metadata
#' @export
#' @param note A string containing a note to add to any existing notes
#'   or replace existing notes, depending on the value of \code{replace}.
#' @param replace If \code{TRUE} the existing notes are overwritten by 
#'   \code{note} if \code{note} is specified. If \code{FALSE} (the default) 
#'   if \code{note} is specified then its content is appended to the existing
#'   notes.
#'   
#' @return \code{notes} returns, amends, or replaces notes stored in a shinystan
#'   object. If \code{note} is \code{NULL} then any existing notes stored in 
#'   \code{sso} are returned as a character string. If \code{note} is specified 
#'   then an updated shinystan object is returned with either \code{note} added 
#'   to the previous notes (if \code{replace=FALSE}) or overwritten by 
#'   \code{note} (if \code{replace = TRUE}). From within the ShinyStan
#'   interface, notes are viewable on the \strong{Notepad} page.
#'   
#' @examples 
#' #############
#' ### notes ###
#' #############
#' 
#' # view existing notes
#' notes(sso)
#' 
#' # add a note to the existing notes
#' sso <- notes(sso, "New note")
#' notes(sso)
#' cat(notes(sso))
#' 
#' # replace existing notes
#' sso <- notes(sso, "replacement note", replace = TRUE)
#' notes(sso)
#'  
notes <- function(sso, note = NULL, replace = FALSE) {
  sso_check(sso)
  if (is.null(note))
    return(slot(sso, "user_model_info"))
  
  if (!is.character(note) || !isTRUE(length(note) == 1))
    stop("'note' should be a single string")
  
  slot(sso, "user_model_info") <- if (replace)
    note else c(slot(sso, "user_model_info"), paste0("\n\n", note))
  
  message(
    paste(
      "Successfully added note.",
      "\nYou can view the notes in the",
      "ShinyStan GUI on the 'Notepad' page."
    )
  )
  sso
}



# model_name (renaming) -----------------------------------------------------#' 
#' @rdname shinystan-metadata
#' @export
#' @param name A string giving the new model name to use.
#'   
#' @return \code{model_name} returns or replaces the model name associated with 
#'   a shinystan object. If \code{name} is \code{NULL} then the current model
#'   name is returned. If \code{name} is specified then \code{sso} is returned
#'   with an updated model name.
#' 
#' @examples
#' ##################
#' ### model_name ###
#' ##################
#' 
#' # view model name
#' model_name(sso)
#' 
#' # change model name
#' sso <- model_name(sso, "some other name")
#' identical(model_name(sso), "some other name")
#' 
model_name <- function(sso, name = NULL) {
  sso_check(sso)
  if (is.null(name))
    return(slot(sso, "model_name"))
  
  if (!is.character(name) || !isTRUE(length(name) == 1))
    stop("'name' should be a single string")
  
  slot(sso, "model_name") <- name
  message(paste("Successfully changed model name to", name))
  sso
}


# nocov start
#' rename_model (deprecated)
#' 
#' This function is deprecated and will be removed in a future release. Please 
#' use the \code{\link{model_name}} function instead.
#' 
#' @export
#' @keywords internal
#' @param sso,new_model_name Use the \code{\link{model_name}} function instead.
#' 
rename_model <- function(sso, new_model_name) {
  .Deprecated("model_name()")
  model_name(sso, new_model_name)
}
# nocov end
