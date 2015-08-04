#' Additional or replacement notes
#' 
#' Notes are viewable on ShinyStan's Notepad page
#' 
#' @export
#' @param sso shinystan object.
#' @param notes A character vector of additional or replacement notes.
#' @param replace If \code{TRUE} the existing notes are overwritten by 
#'   \code{notes}. Otherwise the new notes are appended to the existing notes.
#' @return \code{sso}, updated. See Examples.
#' 
#' @seealso \code{\link{as.shinystan}}
#' @examples 
#' \dontrun{
#' sso <- notes(sso, "new note")
#' sso <- notes(sso, c("new note", "another new note"), replace = TRUE)
#' }
#'  
notes <- function(object, notes, replace = FALSE) {
  sso@user_model_info <- 
    if (replace) paste0("\n\n", notes)
  else c(sso@user_model_info, paste0("\n\n", notes))
  sso
}
