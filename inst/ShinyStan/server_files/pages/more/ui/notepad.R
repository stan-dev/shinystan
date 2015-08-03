output$ui_notepad <- renderUI({
  div(id = "notepad_div",
      sidebarLayout(
        sidebarPanel(width = 2, style = "height: 500px;", 
               # br(),br(),
               helpText(style = "font-size: 12px;", 
                        p("Use this space to store notes about your model."),
                        p("The text will be saved in the", code("user_model_info"),
                          "slot of your shinystan object and displayed here each", 
                          "time you launch ShinyStan with this shinystan object.")
               ),
               br(),
               actionButton("save_user_model_info", label = "Save", 
                            icon = icon("save")),
               div(style = "font-size: 11px;", textOutput("user_text_saved")),
               conditionalPanel(condition = "input.save_user_model_info > 0",
                                br(),
                                helpText(id = "save_user_model_info_safe_quit", 
                                         "In order to make sure the changes", 
                                         "aren't lost use the SAVE & QUIT",
                                         "button to exit the app before", 
                                         "closing the browser window.")
                                )
        ),
        mainPanel(width = 10,
               h4("Notes"),
               tags$textarea(id="user_model_info", rows=20, cols=80, 
                             object@user_model_info)
        )
      )
  )
})