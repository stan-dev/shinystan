# This file is part of shinystan
# Copyright (C) Jonah Gabry
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

# options(shiny.trace=TRUE)
source("global_utils.R", local = TRUE)
helpers <- list.files("helper_functions", full.names = TRUE, recursive = TRUE)
for (h in helpers) source(h, local = TRUE)
source("server_files/utilities/ppcheck_names_descriptions.R", local = TRUE)
source("server_files/utilities/extract_sso.R", local=TRUE)

# Begin shinyServer -------------------------------------------------------
# _________________________________________________________________________
function(input, output, session) {
  
  # Stop the app when "Quit" button is clicked
  observe({
    if (input$save_and_close_button > 0) 
      stopApp(shinystan_object)
  })
  
  # source all files from server_files directory and subdirectories
  files <- list.files("server_files", full.names = TRUE, recursive = TRUE)
  for (f in files) source(f, local = TRUE)
  
  options_inputs <- c("table", "multiparam", "autocorr", "rhat_warnings", # multitrace
                      "bivariate", "trivariate", "density", "hist")
  dens_inputs <- c("point_est", "ci", "x_breaks", "fill_color", "line_color")
  observe({
    lapply(seq_along(options_inputs), function(j){
      shinyjs::onclick(paste0(options_inputs[j], "_options_show"),
                       shinyjs::toggle(id = paste0(options_inputs[j], "_options"), 
                                       anim = TRUE, animType = "slide", time = 0.4))
    })
    lapply(seq_along(dens_inputs), function(j) {
      shinyjs::toggleState(id = paste0("dens_", dens_inputs[j]), 
                           condition = input$dens_chain_split == "Together")
    })
    shinyjs::toggleState(id = "ac_flip", condition = input$ac_combine == FALSE)
  })
  observe({
    shinyjs::onclick("toc_estimate", 
                     updateTabsetPanel(session, "nav", selected = "Estimate"))
    shinyjs::onclick("toc_diagnose", 
                     updateTabsetPanel(session, "nav", selected = "Diagnose"))
    shinyjs::onclick("toc_explore", 
                     updateTabsetPanel(session, "nav", selected = "Explore"))
    shinyjs::onclick("open_glossary_from_table",
                     updateTabsetPanel(session, "nav", selected = "Help"))
    shinyjs::onclick("open_glossary_from_nuts_table", 
                     updateTabsetPanel(session, "nav", selected = "Help"))
    shinyjs::onclick("open_glossary_from_rhat", 
                     updateTabsetPanel(session, "nav", selected = "Help"))
    shinyjs::onclick("toggle_help_glossary", {
      shinyjs::toggle(id = "help_div", anim = TRUE, animType = "slide")
      shinyjs::toggle(id = "glossary_div", anim = TRUE, animType = "slide")
    })
#     shinyjs::onclick("toc_code", updateTabsetPanel(session, "nav", selected = "Model Code"))
#     shinyjs::onclick("toc_help", updateTabsetPanel(session, "nav", selected = "Help"))
#     shinyjs::onclick("toc_about", updateTabsetPanel(session, "nav", selected = "About"))
#     shinyjs::onclick("toc_quit", updateTabsetPanel(session, "nav", selected = "Quit"))
  })
  observeEvent(input$shinystan_citation_show,
               shinyjs::toggle(id = "citation_div", anim = TRUE, animType = "slide"))
  
} # End shinyServer

