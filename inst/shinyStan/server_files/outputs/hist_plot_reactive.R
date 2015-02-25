# two functions: hist_plot, hist_plot_multiview
hist_plot <- reactive({

  validate(need(input$param, message = FALSE),
           need(!is.null(input$hist_chain), message = FALSE))

  chain <- input$hist_chain
  if (is.na(chain)) chain <- 0

  binwd <- input$hist_binwd
  if (is.na(binwd)) binwd <- 0

  do.call(".param_hist", args = list(
    param       = input$param,
    dat         = par_samps_post_warmup(),
    chain       = chain,
    binwd       = binwd,
    fill_color  = input$hist_fill_color,
    line_color  = input$hist_line_color
  ))
})


hist_plot_multiview <- reactive({

  if (input$param == "") {
    return()
  }

  do.call(".param_hist", args = list(
    param       = input$param,
    dat         = par_samps_post_warmup(),
    chain       = 0,
    binwd       = 0,
    title       = FALSE
  ))
})
