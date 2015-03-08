output$ui_pp_y_from_r <- renderUI({
  choices <- objects(envir = .GlobalEnv)
  selectizeInput("y_name", label = span(style = "color: #428bca;", withMathJax("\\(\\mathbf{y}\\), a vector of observations")), choices = c("", choices), 
                 options = list(placeholder = "Select an object"))
})
