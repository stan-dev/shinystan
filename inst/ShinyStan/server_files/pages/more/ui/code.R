output$ui_model_code <- renderUI({
  sidebarLayout(
    sidebarPanel(width = 2, style = "height: 500px;", 
                 # br(),br(),
                 helpText(style = "font-size: 12px;", 
                          p("Use this space to store your model code."),
                          p("The code is saved in the", code("model_code"),
                            "slot of your shinystan object and displayed here each", 
                            "time you launch ShinyStan with this shinystan object.")
                 ),
                 br(),
                 actionButton("save_user_model_code", label = "Save", 
                              icon = icon("save")),
                 div(style = "font-size: 11px;", textOutput("user_code_saved")),
                 conditionalPanel(condition = "input.save_user_model_code > 0",
                                  br(),
                                  helpText(id = "save_user_model_code_safe_quit", 
                                           "In order to make sure the changes", 
                                           "aren't lost use the SAVE & QUIT",
                                           "button to exit the app before", 
                                           "closing the browser window.")
                 )
    ),
    mainPanel(width = 10,
              h4("Model Code"),
              tags$textarea(id="model_code", wrap = "off", cols = 80,
                            readonly = TRUE, object@model_code)
    )
  )
})

