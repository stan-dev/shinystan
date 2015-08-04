output$ui_notepad <- renderUI({
  div(id = "notepad_div",
      sidebarLayout(
        sidebarPanel(width = 3, style = "height: 550px;", 
               br(),
               h4("Model code"),
               helpText(style = "font-size: 12px;", 
                        p("Notes are displayed here each time you launch ShinyStan 
                          with this shinystan object.")
               ),
               br(),
               actionButton("save_user_model_info", label = "Save notes", 
                            icon = icon("save")),
               div(style = "font-size: 11px;", textOutput("user_text_saved")),
               conditionalPanel(condition = "input.save_user_model_info > 0",
                                br(),
                                helpText(id = "save_user_model_info_safe_quit", 
                                         p("In order to make sure the changes", 
                                         "aren't lost use the"),
                                         p("SAVE & QUIT"),
                                         "button to exit the app before", 
                                         "closing the browser window.")
                                )
        ),
        mainPanel(width = 9,
               h4("Notes"),
               tags$textarea(id="user_model_info", rows=20, cols=80, 
                             object@user_model_info)
        )
      )
  )
})