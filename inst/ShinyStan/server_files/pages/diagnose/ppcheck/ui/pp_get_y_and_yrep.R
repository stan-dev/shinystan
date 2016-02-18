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
output$ui_pp_get_y <- renderUI({
  if (is.null(pp_y)) {
    div(
      h4(withMathJax("Select \\(\\mathbf{y}\\) (vector of observations)")),
      selectizeInput("y_name",
                     label = "Object from global environment",
                     choices = c("", objects(envir = .GlobalEnv)), 
                     options = list(placeholder = "Select an object"),
                     width = "50%")
    )
  } 
  else {
    helpText("All set: y found in shinystan object.")  
  }
  
})

output$ui_pp_get_yrep <- renderUI({
  if (is.null(pp_yrep)) {
    choices <- param_names
    choices <- strsplit(choices, split = "[", fixed = TRUE)
    choices <- lapply(choices, function(i) return(i[1]))
    choices <- unique(unlist(choices))
    div(
      h4(withMathJax("Select \\(\\mathbf{y^{rep}}\\) (posterior predictive replications)")),
      flowLayout(
        selectizeInput("yrep_name", 
                       label = "Parameter/generated quantity from model", 
                       choices = c("", choices), 
                       options = list(placeholder = "Select a parameter name")),
        selectizeInput("yrep_name2", 
                       label = "Or object from global environment", 
                       choices = c("", objects(envir = .GlobalEnv)), 
                       options = list(placeholder = "Select an object")) 
      )
    )
  }
  else {
    helpText("All set: yrep found in shinystan object. Select a plot to view.")   
  }
})
