output$ui_pp_yrep_from_sso <- renderUI({
  choices <- param_names
  choices <- strsplit(choices, split = "[", fixed = TRUE)
  choices <- lapply(choices, function(i) return(i[1]))
  choices <- unique(unlist(choices))
  selectizeInput("yrep_name", label = span(style = "color: #428bca;", withMathJax("\\(\\mathbf{y^{rep}}\\), posterior predictive replications")), choices = c("", choices), 
                 options = list(placeholder = "Select a parameter name"))
})