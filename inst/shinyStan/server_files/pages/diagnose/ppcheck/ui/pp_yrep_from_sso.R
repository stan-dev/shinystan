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


output$ui_pp_yrep_from_sso <- renderUI({
  choices <- param_names
  choices <- strsplit(choices, split = "[", fixed = TRUE)
  choices <- lapply(choices, function(i) return(i[1]))
  choices <- unique(unlist(choices))
  selectizeInput("yrep_name", label = span(style = "color: #337ab7;", withMathJax("\\(\\mathbf{y^{rep}}\\), posterior predictive replications")), choices = c("", choices), 
                 options = list(placeholder = "Select a parameter name"))
})