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
