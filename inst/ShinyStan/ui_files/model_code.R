sidebarLayout(
  sidebarPanel(
    width = 3,
    style = "height: 550px;",
    br(),
    h4("Model Code"),
    helpText(
      style = "font-size: 12px;",
      p(
        "Model code will be displayed here each",
        "time you launch ShinyStan with this shinystan object."
      )
    ),
    br(),
    actionButton(
      "save_user_model_code",
      label = "Save code",
      icon = icon("save")
    ),
    div(style = "font-size: 11px;", textOutput("user_code_saved")),
    conditionalPanel(
      condition = "input.save_user_model_code > 0",
      br(),
      save_and_close_reminder("save_user_model_code_safe_quit")
    )
  ),
  mainPanel(
    width = 9,
    br(), br(),
    tags$textarea(
      id = "user_model_code",
      wrap = "off",
      cols = 80,
      rows = 20,
      .model_code
    )
  )
)
