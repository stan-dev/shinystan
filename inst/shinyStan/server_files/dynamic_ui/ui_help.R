output$ui_help <- renderUI({
  tags$div(
  br(),
  a(style = "color: maroon; font-size: 15px;", "Click here to report a bug, request a new feature, or ask us a question.", href = "https://github.com/stan-dev/shinystan/issues"),
  br(),br(),
  h3("shinyStan help"),
  p("More coming soon."),
  uiOutput("help_navlist"), # output navlist with help files
  br(),
  hr(),
  h3("Stan help"),
  a("Stan website", href = "http://mc-stan.org"),
  br(),
  a("Stan users google group", href = "https://groups.google.com/forum/#!forum/stan-users")
  )
})