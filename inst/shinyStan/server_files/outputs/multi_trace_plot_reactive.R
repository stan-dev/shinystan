# calc_height_trace_plot
calc_height_trace_plot <- reactive({
  params <- input$multi_trace_params
  grid <- FALSE
  if (!is.null(input$multi_trace_layout)) {
    if (input$multi_trace_layout == "Grid") grid <- TRUE
  }
  params <- .update_params_with_groups(params, param_names)
  LL <- length(params)
  if (LL == 0) LL <- 4
  if (LL == 1) LL <- 2
  if (grid) {
    if (LL > 5) return(30*LL)
    if (LL < 5) return(60*LL)
  }
  round(100*LL)
})

# multi_trace_plot
multi_trace_plot <- reactive({

  validate(need(!is.null(input$multi_trace_rect), message = "Loading..."))

  zoom <- "On"
  do.call(".param_trace_multi", args = list(
    params      = input$multi_trace_params,
    all_param_names = param_names,
    dat         = samps_all,
    chain       = input$multi_trace_chain,
    warmup_val  = warmup_val,
    inc_warmup  = TRUE, # input$multi_trace_warmup == "include",
    palette     = input$multi_trace_palette ,
    rect        = input$multi_trace_rect,
    rect_color  = "skyblue",
    rect_alpha  = input$multi_trace_rect_alpha,
    layout      = input$multi_trace_layout,
    x1          = ifelse(zoom == "On", input$multi_xzoom[1], NA),
    x2          = ifelse(zoom == "On", input$multi_xzoom[2], NA)
  ))
})
