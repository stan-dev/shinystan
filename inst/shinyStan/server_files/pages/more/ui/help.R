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


output$ui_help <- renderUI({
  tags$div(
  br(),
  navlistPanel(well = FALSE,
               "Help topics",
               tabPanel("Questions, bugs, and new features",
                        h4("Stan users group"),
                        p("To ask a question or suggest a new feature visit the",
                          a("Stan users message board.", href = "https://github.com/stan-dev/shinystan/issues")),
                        h4("GitHub issue tracker"),
                        p("To report a bug visit the",
                          a("GitHub issue tracker.", href = "https://github.com/stan-dev/shinystan/issues"),
                          "You can also use the issue tracker to suggest new features.")
               ),             
               tabPanel("Saving plots",
                        h4("Saving plots as ggplot2 objects"),
                        p("Clicking on a 'Save ggplot2 object' button will be save an .RData 
                          file that you can load into your Global Environment using the",
             code("load"), "function in R. 
             You can then make changes to the plot using the functions in the 
             ggplot2 package."
           )
  )
  )
  )
})
