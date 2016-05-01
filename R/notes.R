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
