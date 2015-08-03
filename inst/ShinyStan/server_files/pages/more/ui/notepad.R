output$ui_notepad <- renderUI({
  div(id = "notepad_div",
      fluidRow(
        column(2, 
               br(),br(),
               helpText(style = "font-size: 12px;", 
                        p("Use this space to store notes about your model."),
                        p("The text will be saved in the", code("user_model_info"),
                          "slot of your shinystan object and displayed here each", 
                          "time you launch ShinyStan with this shinystan object.")
               ),
               br(),
               actionButton("save_user_model_info", label = "Save changes", 
                            icon = icon("save")),
               textOutput("user_text_saved")
        ),
        column(10,
               h4("Notes"),
               tags$textarea(id="user_model_info", rows=20, cols=80, 
                             object@user_model_info)
        )
      )
  )
})