# From: https://gist.github.com/jcheng5/6141ea7066e62cafb31c
# Returns a reactive that debounces the given expression by the given time in
# milliseconds.
#
# This is not a true debounce in that it will not prevent \code{expr} from being
# called many times (in fact it may be called more times than usual), but
# rather, the reactive invalidation signal that is produced by expr is debounced
# instead. This means that this function should be used when \code{expr} is
# cheap but the things it will trigger (outputs and reactives that use
# \code{expr}) are expensive.
debounce <- function(expr, millis, env = parent.frame(), quoted = FALSE,
                     domain = getDefaultReactiveDomain()) {
  
  force(millis)
  
  f <- exprToFunction(expr, env, quoted)
  label <- sprintf("debounce(%s)", paste(deparse(body(f)), collapse = "\n"))
  
  v <- reactiveValues(
    trigger = NULL,
    when = NULL # the deadline for the timer to fire; NULL if not scheduled
  )  
  
  # Responsible for tracking when f() changes.
  observeEvent(f(), {
    # The value changed. Start or reset the timer.
    v$when <- Sys.time() + millis/1000
  }, ignoreNULL = FALSE)
  
  # This observer is the timer. It rests until v$when elapses, then touches
  # v$trigger.
  observe({
    if (is.null(v$when))
      return()
    
    now <- Sys.time()
    if (now >= v$when) {
      v$trigger <- runif(1)
      v$when <- NULL
    } else {
      invalidateLater((v$when - now) * 1000, domain)
    }
  })
  
  # This is the actual reactive that is returned to the user. It returns the
  # value of f(), but only invalidates/updates when v$trigger is touched.
  eventReactive(v$trigger, {
    f()
  }, ignoreNULL = FALSE)
}


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
stepsize_vs_lp <- reactive({
  chain <- diagnostic_chain()
  stepsize <- stepsize_pw()[,-1L] # drop iterations column
  lp <- samps_post_warmup[,,"lp__"]
  .sampler_param_vs_param(p = lp, sp = stepsize, 
                          p_lab = "Log Posterior",
                          sp_lab = "Sampled Step Size", 
                          chain = chain, violin = TRUE)
})
stepsize_vs_accept_stat <- reactive({
  df_ss <- stepsize_pw()[,-1L] # drop iterations column
  df_as <- accept_stat_pw()[,-1L] 
  chain <- diagnostic_chain()
  .sampler_param_vs_sampler_param_violin(round(df_ss, 4), df_as, 
                                         lab_x = "Sampled Step Size",
                                         lab_y = "Mean Metrop. Acceptance",
                                         chain = chain)
})
dynamic_trace_diagnostic_stepsize <- reactive({
  chain <- diagnostic_chain()
  samps <- stepsize_pw()[, -1]
  lab <- "Sampled Step Size"
  stack <- FALSE  
  do.call(".dynamic_trace_diagnostics", args = list(
    param_samps = samps,
    param_name = lab,
    chain = chain,
    stack = stack,
    group = "stepsize_information")
  )
})
# stepsize_trace <- reactive({
#   df <- stepsize_pw()
#   chain <- diagnostic_chain()
#   .stepsize_trace(df, chain)
#   # .p_trace(df, lab = "Sampled Step Size", chain)
# })



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

# lp_trace <- reactive({
#   sp_nuts_check()
#   chain <- diagnostic_chain()
#   lp <- samps_post_warmup[,,"lp__"]
#   df <- as.data.frame(cbind(iterations = (warmup_val+1):(warmup_val+nrow(lp)), lp))
#   .p_trace(df, lab = "Log Posterior", chain)
# })
# accept_stat_trace <- reactive({
#   df <- accept_stat_pw()
#   chain <- diagnostic_chain()
#   .p_trace(df, lab = "Mean Metrop. Acceptance", chain)
# }) 

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
                      color = "#eeba30", strokePattern = "solid") %>%
    # dygraphs::dyOptions(stepPlot = TRUE) %>%
    dygraphs::dyAxis("y", valueRange = c(0, max_td * 8/7), 
                     pixelsPerLabel = 20, drawGrid = FALSE)
})
# treedepth_trace <- reactive({
#   df <- treedepth_pw()
#   chain <- diagnostic_chain()
#   max_td <- MISC$max_td
#   .p_trace(df, lab = "Treedepth", chain) + 
#     geom_hline(yintercept = max_td, size = 1)
# })
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
    # dygraphs::dyOptions(stepPlot = TRUE) %>%
    dygraphs::dyAxis("y", valueRange = c(0, 1.2), pixelsPerLabel = 1e4, 
                     drawGrid = FALSE)
})
# ndivergent_trace <- reactive({
#   df <- ndivergent_pw()
#   chain <- diagnostic_chain()
#   .ndivergent_trace(df, chain)
# })
ndivergent_vs_lp <- reactive({
  ndivergent <- ndivergent_pw()[,-1L] # drop iterations column
  lp <- samps_post_warmup[,,"lp__"]
  chain <- diagnostic_chain()
  .sampler_param_vs_param(p = lp, sp = ndivergent, 
                          p_lab = "Log Posterior",
                          sp_lab = "N Divergent", 
                          chain = chain, violin = TRUE)
})
ndivergent_vs_accept_stat <- reactive({
  df_nd <- ndivergent_pw()[,-1L] # drop iterations column
  df_as <- accept_stat_pw()[,-1L] 
  chain <- diagnostic_chain()
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
# p_trace <- reactive({
#   chain <- diagnostic_chain()
#   param <- diagnostic_param()
#   transform_x <- diagnostic_param_transform()
#   t_x <- eval(parse(text = paste("function(x)", transform_x)))
#   samps <- samps_post_warmup[,, param]
#   if (transform_x != "x") samps <- t_x(samps)
#   lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
#   df <- as.data.frame(cbind(iterations = (warmup_val+1):(warmup_val+nrow(samps)), samps))
#   .p_trace(df, lab = lab, chain = chain)
# })

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

# dynamic trace outputs ---------------------------------------------------
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
