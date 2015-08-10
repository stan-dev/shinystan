#' Additional or replacement notes
#' 
#' Notes are viewable on ShinyStan's Notepad page
#' 
#' @export
#' @param sso shinystan object.
#' @param notes Optional. A character vector of additional or replacement notes.
#' @param replace If \code{TRUE} the existing notes are overwritten by 
#'   \code{notes} if \code{notes} is specified. If \code{FALSE} (the default) 
#'   if \code{notes} is specified then its content is appended to the existing
#'   notes.
#' @return If \code{notes} is missing then any existing notes stored in 
#'   \code{sso} are returned as a character string. If \code{notes} is specified
#'   then an updated shinystan object is returned with either \code{notes} added
#'   to the previous notes (if \code{replace=FALSE}) or overwritten by
#'   \code{notes} (if \code{replace = TRUE}).
#'   
#' 
#' @seealso \code{\link{as.shinystan}}
#' @examples 
#' \dontrun{
#' sso <- notes(sso, "new note")
#' sso <- notes(sso, c("a different note", "another note"), replace = TRUE)
#' 
#' # See any notes currently in sso
#' notes(sso)
#' }
#'  
notes <- function(sso, notes, replace = FALSE) {
  if (missing(notes)) {
    return(slot(sso, "user_model_info"))
  }
  if (length(notes) > 1L) {
    notes <- c(notes[1L], paste0("\n\n", notes[-1L]))
  }
  slot(sso, "user_model_info") <- if (replace) 
    notes else c(slot(sso, "user_model_info"), paste0("\n\n", notes))
  
  message(paste0("Successfully added notes.", "\nYou can view the notes in the", 
                 "ShinyStan GUI on the 'Notepad' page."))
  sso
}
