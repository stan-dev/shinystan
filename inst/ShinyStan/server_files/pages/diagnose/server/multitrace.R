# 
# # multiparameter traceplots -----------------------------------------------
# calc_height_trace_plot <- reactive({
#   params <- input$multitrace_params
#   grid <- FALSE
#   if (!is.null(input$multitrace_layout)) {
#     if (input$multitrace_layout == "Grid") grid <- TRUE
#   }
#   params <- .update_params_with_groups(params, param_names)
#   LL <- length(params)
#   if (LL == 0) LL <- 4
#   if (LL == 1) LL <- 2
#   if (grid) {
#     if (LL > 5) return(30*LL)
#     if (LL < 5) return(60*LL)
#   }
#   round(100*LL)
# })
# 
# # multitrace_plot
# multitrace_plot <- reactive({
#   validate(need(!is.null(input$multitrace_rect), message = "Loading..."))
#   x1 <- input$multi_xzoom[1]
#   x2 <- input$multi_xzoom[2]
#   dat <- samps_all[x1:x2,,,drop=FALSE]
#   # zoom <- "On"
#   do.call(".param_trace_multi", args = list(
#     params      = input$multitrace_params,
#     all_param_names = param_names,
#     dat         = dat,
#     chain       = input$multitrace_chain,
#     warmup_val  = warmup_val,
#     palette     = input$multitrace_palette ,
#     rect        = input$multitrace_rect,
#     rect_color  = "skyblue",
#     rect_alpha  = input$multitrace_rect_alpha,
#     layout      = input$multitrace_layout,
#     x1          = x1,
#     x2          = x2
#   ))
# })
# 
# output$multitrace_plot_out <- renderPlot({
#   x <- multitrace_plot()
#   suppressWarnings(print(x)) # this avoids warnings about removing rows when using tracezoom feature
# }, height = calc_height_trace_plot, bg = "transparent")
# 
# # download the plot
# output$download_multitrace <- downloadHandler(
#   filename = paste0('shinystan_multitrace.RData'),
#   content = function(file) {
#     shinystan_multitrace <- multitrace_plot()
#     save(shinystan_multitrace, file = file)
#   }
# )
