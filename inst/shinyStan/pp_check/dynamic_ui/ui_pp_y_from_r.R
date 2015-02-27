output$ui_pp_y_from_r <- renderUI({
  choices <- objects(envir = .GlobalEnv)
  selectizeInput("y_name", label = withMathJax("\\(\\mathbf{y}\\), the dependent variable, a vector of observations"), choices = c("", choices), 
                 options = list(placeholder = "data vector"))
})
