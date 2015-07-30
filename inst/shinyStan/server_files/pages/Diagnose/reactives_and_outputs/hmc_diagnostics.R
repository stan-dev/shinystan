sp_nuts_check <- reactive({
  validate(
    need(sampler_params[[1L]] != "Not Stan", message = "Only available for Stan models"),
    need(stan_algorithm == "NUTS", message = "Only available for algorithm = NUTS"),
    need(input$diagnostic_chain, message = "Loading..."))
})
accept_stat_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params_post_warmup, which = "accept_stat__", 
                    warmup_val = warmup_val)
})
treedepth_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params_post_warmup, which = "treedepth__", 
                    warmup_val = warmup_val)
})
ndivergent_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params_post_warmup, which = "n_divergent__", 
                    warmup_val = warmup_val)
})
stepsize_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params_post_warmup, which = "stepsize__", 
                    warmup_val = warmup_val)
})

diagnostic_chain <- reactive({
  validate(need(input$diagnostic_chain, message = "Waiting for chain (0 for all)"))
  input$diagnostic_chain
})
diagnostic_param <- reactive({
  validate(need(input$diagnostic_param, message = "Waiting for parameter"))
  input$diagnostic_param
})
diagnostic_param_transform <- eventReactive(
  input$diagnostic_param_transform_go > 0, 
  input$diagnostic_param_transform)

selected_range <- debounce({
  panel <- input$diagnostics_navlist
  nm <- switch(panel, 
               "By model parameter" = "parameter",
               "Sample information" = "lp",
               "Treedepth information" = "treedepth",
               "Step size information" = "stepsize",
               "N divergent information" = "ndivergent")
  input_nm <- paste0("dynamic_trace_diagnostic_", nm, "_out_date_window")
  validate(need(input[[input_nm]], "Loading"))
  sel <- input[[input_nm]]
  high <- as.integer(strsplit(sel[[2]], "[-]")[[1]][1])
  low <- as.integer(if (is.nan(sel[[1]])) "1" else strsplit(sel[[1]], "[-]")[[1]][1])
  low:high
}, millis = 100)

# stepsize ----------------------------------------------------------------
dynamic_trace_diagnostic_stepsize <- reactive({
  chain <- diagnostic_chain()
  samps <- stepsize_pw()[, -1]
  lab <- "Sampled Step Size"
  stack <- FALSE  
  `%>%` <- dygraphs::`%>%`
  graph <- do.call(".dynamic_trace_diagnostics", args = list(
    param_samps = samps,
    param_name = lab,
    chain = chain,
    stack = stack,
    group = "stepsize_information")) 
  graph %>% 
    dygraphs::dyAxis("y", pixelsPerLabel = 40)
})
stepsize_vs_lp <- reactive({
  chain <- diagnostic_chain()
  sel <- selected_range()
  stepsize <- stepsize_pw()[if (!is.null(sel)) sel, -1L] # drop iterations column
  lp <- samps_post_warmup[if (!is.null(sel)) sel,,"lp__"]
  .sampler_param_vs_param(p = lp, sp = stepsize, 
                          p_lab = "Log Posterior",
                          sp_lab = "Sampled Step Size", 
                          chain = chain, violin = TRUE)
})
stepsize_vs_accept_stat <- reactive({
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_ss <- stepsize_pw()[if (!is.null(sel)) sel, -1L] # drop iterations column
  df_as <- accept_stat_pw()[if (!is.null(sel)) sel, -1L] 
  .sampler_param_vs_sampler_param_violin(round(df_ss, 4), df_as, 
                                         lab_x = "Sampled Step Size",
                                         lab_y = "Mean Metrop. Acceptance",
                                         chain = chain)
})


# sample (accept_stat, lp) ------------------------------------------------
dynamic_trace_diagnostic_lp <- reactive({
  chain <- diagnostic_chain()
  samps <- samps_post_warmup[,, "lp__"]
  lab <- "Log Posterior"
  stack <- FALSE  
  do.call(".dynamic_trace_diagnostics", args = list(
    param_samps = samps,
    param_name = lab,
    chain = chain,
    stack = stack,
    group = "sample_information")
  )
})
dynamic_trace_diagnostic_accept_stat <- reactive({
  chain <- diagnostic_chain()
  samps <- accept_stat_pw()[, -1]
  lab <- "Mean Metropolis Acceptance"
  stack <- FALSE  
  do.call(".dynamic_trace_diagnostics", args = list(
    param_samps = samps,
    param_name = lab,
    chain = chain,
    stack = stack,
    group = "sample_information")
  )
})
lp_hist <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  sel <- selected_range()
  lp <- samps_post_warmup[if (!is.null(sel)) sel,, "lp__"]
  df <- as.data.frame(cbind(iterations = 1:nrow(lp), lp))
  .p_hist(df, lab = "Log Posterior", chain)
})
accept_stat_hist <- reactive({
  sel <- selected_range()
  df <- accept_stat_pw()[if (!is.null(sel)) sel, ]
  chain <- diagnostic_chain()
  .p_hist(df, lab = "Mean Metrop. Acceptance", chain) + xlim(0,1)
})
accept_stat_vs_lp <- reactive({
  sel <- selected_range()
  metrop <- accept_stat_pw()[if (!is.null(sel)) sel,-1L] # drop iterations column
  lp <- samps_post_warmup[if (!is.null(sel)) sel,,"lp__"]
  chain <- input$diagnostic_chain
  divergent <- ndivergent_pw()[if (!is.null(sel)) sel,-1L]
  td <- treedepth_pw()[if (!is.null(sel)) sel,-1L]
  hit_max_td <- apply(td, 2L, function(y) as.numeric(y == MISC$max_td))
  .sampler_param_vs_param(p = lp, sp = metrop,
                          divergent = divergent, 
                          hit_max_td = as.data.frame(hit_max_td),
                          p_lab = "Log Posterior",
                          sp_lab = "Mean Metropolis Acceptance", 
                          chain = chain)
})


# treedepth ---------------------------------------------------------------
dynamic_trace_diagnostic_treedepth <- reactive({
  chain <- diagnostic_chain()
  samps <- treedepth_pw()[, -1]
  max_td <- MISC$max_td
  lab <- "Treedepth"
  stack <- FALSE  
  graph <- do.call(".dynamic_trace_diagnostics", args = list(
    param_samps = samps,
    param_name = lab,
    chain = chain,
    stack = stack,
    group = "treedepth_information")
  )
  `%>%` <- dygraphs::`%>%`
  graph %>% 
    dygraphs::dyLimit(limit = max_td, label = "max_treedepth", labelLoc = "right",
                      color = "black", strokePattern = "solid") %>%
    dygraphs::dyAxis("y", valueRange = c(0, max_td * 8/7), 
                     pixelsPerLabel = 20, drawGrid = FALSE)
})
treedepth_ndivergent_hist <- reactive({
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_td <- treedepth_pw()[if (!is.null(sel)) sel, ]
  df_nd <- ndivergent_pw()[if (!is.null(sel)) sel, ]
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = "All")
})
treedepth_ndivergent0_hist <- reactive({
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_td <- treedepth_pw()[if (!is.null(sel)) sel, ]
  df_nd <- ndivergent_pw()[if (!is.null(sel)) sel, ]
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = 0)
})
treedepth_ndivergent1_hist <- reactive({
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_td <- treedepth_pw()[if (!is.null(sel)) sel, ]
  df_nd <- ndivergent_pw()[if (!is.null(sel)) sel, ]
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = 1)
})
treedepth_vs_lp <- reactive({
  chain <- diagnostic_chain()
  sel <- selected_range()
  treedepth <- treedepth_pw()[if (!is.null(sel)) sel, -1L] # drop iterations column
  lp <- samps_post_warmup[if (!is.null(sel)) sel,,"lp__"]
  .sampler_param_vs_param(p = lp, sp = treedepth, 
                          p_lab = "Log Posterior",
                          sp_lab = "Tree Depth", 
                          chain = chain, violin = TRUE)
})
treedepth_vs_accept_stat <- reactive({
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_td <- treedepth_pw()[if (!is.null(sel)) sel, -1L] # drop iterations column
  df_as <- accept_stat_pw()[if (!is.null(sel)) sel, -1L] 
  .sampler_param_vs_sampler_param_violin(df_td, df_as, 
                                         lab_x = "Treedepth",
                                         lab_y = "Mean Metrop. Acceptance",
                                         chain = chain)
})


# N divergent -------------------------------------------------------------
dynamic_trace_diagnostic_ndivergent <- reactive({
  chain <- diagnostic_chain()
  samps <- ndivergent_pw()[, -1]
  lab <- "N Divergent"
  stack <- FALSE  
  graph <- do.call(".dynamic_trace_diagnostics", args = list(
    param_samps = samps,
    param_name = lab,
    chain = chain,
    stack = stack,
    group = "ndivergent_information")
  )
  `%>%` <- dygraphs::`%>%`
  graph %>% 
    dygraphs::dyAxis("y", valueRange = c(0, 1.1), pixelsPerLabel = 1e4, 
                     drawGrid = FALSE)
})
ndivergent_vs_lp <- reactive({
  chain <- diagnostic_chain()
  sel <- selected_range()
  ndivergent <- ndivergent_pw()[if (!is.null(sel)) sel, -1L] # drop iterations column
  lp <- samps_post_warmup[if (!is.null(sel)) sel,,"lp__"]
  .sampler_param_vs_param(p = lp, sp = ndivergent, 
                          p_lab = "Log Posterior",
                          sp_lab = "N Divergent", 
                          chain = chain, violin = TRUE)
})
ndivergent_vs_accept_stat <- reactive({
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_nd <- ndivergent_pw()[if (!is.null(sel)) sel, -1L] # drop iterations column
  df_as <- accept_stat_pw()[if (!is.null(sel)) sel, -1L] 
  .sampler_param_vs_sampler_param_violin(df_nd, df_as, 
                                         lab_x = "N Divergent",
                                         lab_y = "Mean Metrop. Acceptance",
                                         chain = chain)
})


# model parameter ---------------------------------------------------------
dynamic_trace_diagnostic_parameter <- reactive({
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  transform_x <- diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[,, param]
  if (transform_x != "x") samps <- t_x(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  stack <- FALSE  
  do.call(".dynamic_trace_diagnostics", args = list(
    param_samps = samps,
    param_name = paste("Parameter:", lab),
    chain = chain,
    stack = stack
  )
  )
})
param_vs_lp <- reactive({
  param <- diagnostic_param()
  chain <- diagnostic_chain()
  sel <- selected_range()
  lp <- samps_post_warmup[if (!is.null(sel)) sel,, "lp__"]
  transform_x <- diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[if (!is.null(sel)) sel,, param]
  if (transform_x != "x") samps <- t_x(samps)
  samps <- as.data.frame(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  divergent <- ndivergent_pw()[if (!is.null(sel)) sel, -1L]
  td <- treedepth_pw()[if (!is.null(sel)) sel, -1L]
  hit_max_td <- apply(td, 2L, function(y) as.numeric(y == MISC$max_td))
  .sampler_param_vs_param(p = lp, sp = samps, 
                          divergent = divergent, 
                          hit_max_td = as.data.frame(hit_max_td),
                          p_lab = "Log Posterior",
                          sp_lab = lab, 
                          chain = chain, violin = FALSE)
})
param_vs_accept_stat <- reactive({
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  sel <- selected_range()
  metrop <- accept_stat_pw()[if (!is.null(sel)) sel, -1L] # drop iterations column
  transform_x <- diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[if (!is.null(sel)) sel,, param]
  if (transform_x != "x") samps <- t_x(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  divergent <- ndivergent_pw()[if (!is.null(sel)) sel, -1L]
  td <- treedepth_pw()[if (!is.null(sel)) sel, -1L]
  hit_max_td <- apply(td, 2L, function(y) as.numeric(y == MISC$max_td))
  graph <- .sampler_param_vs_param(p = samps, sp = metrop,
                                   divergent = divergent, 
                                   hit_max_td = as.data.frame(hit_max_td),
                                   chain = chain, p_lab = lab,
                                   sp_lab = "Mean Metropolis Acceptance")
  graph + coord_flip()
})
param_vs_stepsize <- reactive({
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  sel <- selected_range()
  stepsize <- stepsize_pw()[if (!is.null(sel)) sel, -1L] # drop iterations column
  transform_x <- diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[if (!is.null(sel)) sel,, param]
  if (transform_x != "x") samps <- t_x(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  .sampler_param_vs_param(p = samps, sp = stepsize, 
                          p_lab = lab, sp_lab = "Sampled Step Size", 
                          chain = chain, violin = TRUE)
})
param_vs_treedepth <- reactive({
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  sel <- selected_range()
  treedepth <- treedepth_pw()[if (!is.null(sel)) sel, -1L] # drop iterations column
  transform_x <- diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[if (!is.null(sel)) sel,, param]
  if (transform_x != "x") samps <- t_x(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  .sampler_param_vs_param(p = samps, sp = treedepth, p_lab = lab,
                          sp_lab = "Treedepth", chain = chain, violin = TRUE)
})
p_hist <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  sel <- selected_range()
  transform_x <- diagnostic_param_transform() # diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[if (!is.null(sel)) sel,, param]
  if (transform_x != "x") samps <- t_x(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  df <- as.data.frame(cbind(iterations = 1:nrow(samps), samps))
  .p_hist(df, lab = lab, chain = chain)
})

# outputs ---------------------------------------------------
output$dynamic_trace_diagnostic_parameter_out <- dygraphs::renderDygraph({
  dynamic_trace_diagnostic_parameter()
})
output$dynamic_trace_diagnostic_lp_out <- dygraphs::renderDygraph({
  dynamic_trace_diagnostic_lp()
})
output$dynamic_trace_diagnostic_accept_stat_out <- dygraphs::renderDygraph({
  dynamic_trace_diagnostic_accept_stat()
})
output$dynamic_trace_diagnostic_treedepth_out <- dygraphs::renderDygraph({
  dynamic_trace_diagnostic_treedepth()
})
output$dynamic_trace_diagnostic_stepsize_out <- dygraphs::renderDygraph({
  dynamic_trace_diagnostic_stepsize()
})
output$dynamic_trace_diagnostic_ndivergent_out <- dygraphs::renderDygraph({
  dynamic_trace_diagnostic_ndivergent()
})

hmc_plots <- c("accept_stat_trace", "accept_stat_hist","accept_stat_vs_lp",
               "lp_trace", "lp_hist", "ndivergent_trace", "treedepth_trace",
               "treedepth_ndivergent_hist","treedepth_ndivergent0_hist", 
               "treedepth_ndivergent1_hist", "treedepth_vs_lp", "ndivergent_vs_lp", 
               "treedepth_vs_accept_stat", "ndivergent_vs_accept_stat", 
               "stepsize_vs_lp", "stepsize_vs_accept_stat", "stepsize_trace", 
               "param_vs_lp", "param_vs_accept_stat", "param_vs_stepsize", 
               "param_vs_treedepth", "p_trace", "p_hist")
for (i in seq_along(hmc_plots)) {
  local({
    fn <- hmc_plots[i] 
    output[[paste0(fn,"_out")]] <- renderPlot({
      x <- suppressMessages(do.call(fn, list()))
      suppress_and_print(x)
    })
  })
}

output$diagnostic_chain_text <- renderText({
  chain <- diagnostic_chain()
  if (chain == 0) return("All chains")
  paste("Chain", chain)
})
