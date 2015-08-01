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



output$ui_help <- renderUI({
  tags$div(
  br(),
  # h3("shinyStan help"),
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
  ),
  tabPanel("Performance",
           h4("Launch speed with large stanfit objects"),
           p("When used with a very large stanfit object",
            strong("shinyStan"), "may launch very slowly because first a", 
            "shinystan object is created from the specified", 
            "stanfit object. When", strong("shinyStan"),"is closed you will",
            "have access to this object in the",
            "global environment and next time you use shinyStan for the",
            "same model you can launch it quicker by using the", 
            "shinystan object rather than the original", 
            "stanfit object."),
           p("Alternatively, a shinystan object can be",
             "created before launching the app by using the", 
             code("as.shinystan"), "function.")
           )
  )
  )
})
