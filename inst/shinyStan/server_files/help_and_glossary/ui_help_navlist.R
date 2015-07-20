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



output$help_navlist <- renderUI({
  navlistPanel(well = FALSE,
  tabPanel("Saving plots",
           h4("Saving plots as ggplot2 objects"),
           p("You can save any plots as ggplot2 objects by clicking on the
                                      'Save ggplot2 object' button. The object will be saved as an .RData
                                      file that you can load into your Global Environment using the",
             code("load"), "function in R. You can then make changes to
                                    the plot using the functions in the ggplot2 package."
           )
  ),
  tabPanel("Performance",
           h4("Launch speed with large stanfit objects"),
           p("When used with a large stanfit object",
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
#   tabPanel("Appearance settings",
#            h4("Saving and loading appearance settings"),
#            
#            p("After customizing the appearance of a plot,
#                                     click on the 'Save appearance settings' button
#                                     to save the settings to a .RData file.
#                                     To load previously saved settings simply load
#                                     the .RData file into your Global Environment",
#              em("before"), "launching", strong("shinyStan"),
#              "and then click on 'Load settings' in the
#             appropriate plot window.")
#   )
  )
})
