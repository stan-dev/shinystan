output$ui_pp_yrep_from_sso <- renderUI({
  choices <- param_names
  choices <- strsplit(choices, split = "[", fixed = TRUE)
  choices <- lapply(choices, function(i) return(i[1]))
  choices <- unique(unlist(choices))
  selectizeInput("yrep_name", label = withMathJax("\\(\\mathbf{y_{rep}}\\), posterior predictive replications"), choices = c("", choices), 
                 options = list(placeholder = "y_rep"))
})