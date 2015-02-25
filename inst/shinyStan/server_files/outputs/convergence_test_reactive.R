convergence_test <- reactive({

  if (is.null(input$convergence_R) | is.null(input$convergence_thin)) {
    return()
  }

  validate(need((nIter %% input$convergence_thin) == 0,
                message = "Error: this value for 'Thin' leaves a remainder."))

  do.call(".convergence_test", args = list(
    session = session, # needed for progress bar
    object  = samps_all,
    R       = input$convergence_R,
    thin    = input$convergence_thin
  ))
})
