output$ui_pp_get_y <- renderUI({
  if (is.null(pp_y)) {
    div(
      h4(
        withMathJax("Select \\(\\mathbf{y}\\) (vector of observations)")
      ),
      selectizeInput(
        "y_name",
        label = "Object from global environment",
        choices = c("", objects(envir = .GlobalEnv)),
        options = list(placeholder = "Select an object"),
        width = "50%"
      )
    )
  } else {
    helpText("All set: y found in shinystan object.")
  }
})

output$ui_pp_get_yrep <- renderUI({
  if (is.null(pp_yrep)) {
    choices <- PARAM_NAMES
    choices <- strsplit(choices, split = "[", fixed = TRUE)
    choices <- lapply(choices, function(i) return(i[1]))
    choices <- unique(unlist(choices))
    div(h4(
      withMathJax(
        "Select \\(\\mathbf{y^{rep}}\\) (posterior predictive replications)"
      )
    ),
    flowLayout(
      selectizeInput(
        "yrep_name",
        label = "Parameter/generated quantity from model",
        choices = c("", choices),
        options = list(placeholder = "Select a parameter name")
      ),
      selectizeInput(
        "yrep_name2",
        label = "Or object from global environment",
        choices = c("", objects(envir = .GlobalEnv)),
        options = list(placeholder = "Select an object")
      )
    ))
  } else {
    helpText("All set: yrep found in shinystan object. Select a plot to view.")
  }
})
