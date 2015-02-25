
dynamic_trace_plot <- reactive({

  validate(need(input$param, message = FALSE),
           need(input$dynamic_trace_chain, message = FALSE))

  stack <- input$dynamic_trace_stack == "stacked"
  chain <- input$dynamic_trace_chain
  if (is.na(chain)) chain <- 0 # set chain to zero (all chains) if input field is blank

  do.call(".param_trace_dynamic", args = list(
    param_samps = par_samps_all(),
    param_name = input$param,
    chain = chain,
    warmup_val = warmup_val,
    warmup_shade = (input$dynamic_trace_warmup_shade == "show"),
    stack = stack,
    grid = (input$dynamic_trace_grid == "show")
  ))
})

dynamic_trace_plot_multiview <- reactive({
  if (input$param == "") {
    return()
  }

  stack <- FALSE  # stack <- input$dynamic_trace_stack
  chain <- 0      # input$dynamic_trace_chain

  do.call(".param_trace_dynamic", args = list(
    param_samps = par_samps_all(),
    chain = chain,
    warmup_val = warmup_val,
    stack = stack)
  )
})
