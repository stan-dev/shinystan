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


# options(shiny.trace=TRUE)
source("global_utils.R", local = TRUE)
helpers <- list.files("helper_functions", full.names = TRUE, recursive = TRUE)
for (h in helpers) source(h, local = TRUE)
source("server_files/utilities/ppcheck_names_descriptions.R", local = TRUE)
source("server_files/utilities/extract_shinystan_object.R", local=TRUE)

# Begin shinyServer -------------------------------------------------------
# _________________________________________________________________________
function(input, output, session) {
  
  # Stop the app when "Quit" button is clicked
  observe({
    if (input$nav == "quit") stopApp("Session ended")
  })
  
  # source all files from server_files directory and subdirectories
  files <- list.files("server_files", full.names = TRUE, recursive = TRUE)
  for (f in files) source(f, local = TRUE)

  # tooltips
  for (id in seq_along(tooltip_ids)) {
    addTooltip(session, id = tooltip_ids[id], trigger = "hover", 
               placement = tooltip_placements[id],
               title = tooltip_msgs[id], options = list(container = 'body'))
  }

  # user's notes 
  observeEvent(input$save_user_model_info, handlerExpr = {
    if (input$user_model_info != "")
      shinystan_object@user_model_info <<- input$user_model_info
  })
  output$user_text_saved <- renderText({
    input$save_user_model_info # take dependency on action button
    if (input$save_user_model_info != 0) {
      print(paste("Saved:  ", format(Sys.time(), "%a %b %d %Y %X")))
    }
  })
  
  
  shinyjs::onclick("trivariate_options_show", shinyjs::toggle(id = "trivariate_options", anim = TRUE, animType = "slide", time = 0.75))
  shinyjs::onclick("density_options_show", shinyjs::toggle(id = "density_options", anim = TRUE, animType = "slide", time = 0.75))
  shinyjs::onclick("hist_options_show", shinyjs::toggle(id = "hist_options", anim = TRUE, animType = "slide", time = 0.75))
  
} # End shinyServer

