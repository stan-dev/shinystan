lp_name <- "log-posterior"
lp_lab <- "Log Posterior"
metrop_lab <- "Mean Metrop. Acceptance"
stepsize_lab <- "Sampled Step Size"
treedepth_lab <- "Treedepth"
ndivergent_lab <- "Divergent"

sp_nuts_check <- reactive({
  validate(
    need(STAN_ALGORITHM == "NUTS", message = "Only available for algorithm = NUTS"),
    need(input$diagnostic_chain, message = "Loading...")
  )
})
lp_check <- reactive({
  validate(
    need(lp_name %in% dimnames(SAMPS_post_warmup)[[3]], 
         message = "Plot not displayed\n(Draws for 'lp__' or 'log-posterior' not found)")
  )
})
diagnostic_chain <- reactive({
  validate(need(input$diagnostic_chain, message = "Waiting for chain (0 for all)"))
  input$diagnostic_chain
})
diagnostic_param <- reactive({
  validate(need(input$diagnostic_param, message = "Waiting for parameter"))
  input$diagnostic_param
})


diagnostic_param_transform <-
  eventReactive(input$diagnostic_param_transform_go > 0,
                input$diagnostic_param_transform)

selected_range <- debounce({
  panel <- input$diagnostics_navlist
  nm <- switch(
    panel,
    "By model parameter" = "parameter",
    "Sample information" = "lp",
    "Treedepth information" = "treedepth",
    "Step size information" = "stepsize",
    "Divergence information" = "divergent"
  )
  input_nm <- paste0("dynamic_trace_diagnostic_", nm, "_out_date_window")
  validate(need(input[[input_nm]], "Updating selected range"))
  sel <- input[[input_nm]]
  high <- as.integer(strsplit(sel[[2]], "[-]")[[1]][1])
  low <- as.integer(if (is.nan(sel[[1]])) "1" else strsplit(sel[[1]], "[-]")[[1]][1])
  low:high
}, millis = 125)

# stepsize ----------------------------------------------------------------
dynamic_trace_diagnostic_stepsize <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  samps <- .stepsize_pw[,-1]
  lab <- "Sampled Step Size"
  stack <- FALSE
  `%>%` <- dygraphs::`%>%`
  graph <- do.call(
    ".dynamic_trace_diagnostics",
    args = list(
      param_samps = samps,
      param_name = lab,
      chain = chain,
      stack = stack,
      group = "stepsize_information"
    )
  )
  graph %>% dygraphs::dyAxis("y", pixelsPerLabel = 40)
})
stepsize_vs_lp <- reactive({
  sp_nuts_check()
  lp_check()
  chain <- diagnostic_chain()
  sel <- selected_range()
  stepsize <- .stepsize_pw[if (!is.null(sel)) sel,-1L, drop = FALSE] # drop iterations column
  lp <- SAMPS_post_warmup[if (!is.null(sel)) sel, , lp_name]
  .sampler_param_vs_param(
    p = lp,
    sp = stepsize,
    p_lab = lp_lab,
    sp_lab = stepsize_lab,
    chain = chain,
    violin = TRUE
  )
})
stepsize_vs_accept_stat <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_ss <- .stepsize_pw[if (!is.null(sel)) sel,-1L, drop = FALSE] # drop iterations column
  df_as <- .accept_stat_pw[if (!is.null(sel)) sel,-1L, drop = FALSE]
  .sampler_param_vs_sampler_param_violin(
    round(df_ss, 4),
    df_as,
    lab_x = stepsize_lab,
    lab_y = metrop_lab,
    chain = chain
  )
})


# sample (accept_stat, lp) ------------------------------------------------
dynamic_trace_diagnostic_lp <- reactive({
  sp_nuts_check()
  lp_check()
  chain <- diagnostic_chain()
  samps <- SAMPS_post_warmup[, , lp_name]
  lab <- "Log Posterior"
  stack <- FALSE
  do.call(
    ".dynamic_trace_diagnostics",
    args = list(
      param_samps = samps,
      param_name = lab,
      chain = chain,
      stack = stack,
      group = "sample_information"
    )
  )
})
dynamic_trace_diagnostic_accept_stat <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  samps <- .accept_stat_pw[,-1]
  stack <- FALSE
  do.call(
    ".dynamic_trace_diagnostics",
    args = list(
      param_samps = samps,
      param_name = metrop_lab,
      chain = chain,
      stack = stack,
      group = "sample_information"
    )
  )
})
lp_hist <- reactive({
  sp_nuts_check()
  lp_check()
  chain <- diagnostic_chain()
  sel <- selected_range()
  lp <- SAMPS_post_warmup[if (!is.null(sel)) sel,, lp_name]
  df <- as.data.frame(cbind(iterations = 1:NROW(lp), lp))
  .p_hist(df, lab = lp_lab, chain)
})
accept_stat_hist <- reactive({
  sp_nuts_check()
  sel <- selected_range()
  df <- .accept_stat_pw[if (!is.null(sel)) sel,, drop=FALSE]
  chain <- diagnostic_chain()
  .p_hist(df, lab = metrop_lab, chain) + xlim(0,1)
})
accept_stat_vs_lp <- reactive({
  sp_nuts_check()
  lp_check()
  sel <- selected_range()
  metrop <- .accept_stat_pw[if (!is.null(sel)) sel, -1L, drop = FALSE] # drop iterations column
  lp <- SAMPS_post_warmup[if (!is.null(sel)) sel, , lp_name]
  chain <- input$diagnostic_chain
  divergent <- .ndivergent_pw[if (!is.null(sel)) sel, -1L, drop = FALSE]
  td <- .treedepth_pw[if (!is.null(sel)) sel, -1L, drop = FALSE]
  hit_max_td <- apply(td, 2L, function(y) as.numeric(y == MISC$max_td))
  .sampler_param_vs_param(
    p = lp,
    sp = metrop,
    divergent = divergent,
    hit_max_td = as.data.frame(hit_max_td),
    p_lab = lp_lab,
    sp_lab = metrop_lab,
    chain = chain
  )
})


# treedepth ---------------------------------------------------------------
dynamic_trace_diagnostic_treedepth <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  samps <- .treedepth_pw[, -1]
  max_td <- MISC$max_td
  lab <- treedepth_lab
  stack <- FALSE
  graph <- do.call(
    ".dynamic_trace_diagnostics",
    args = list(
      param_samps = samps,
      param_name = lab,
      chain = chain,
      stack = stack,
      group = "treedepth_information"
    )
  )
  
  `%>%` <- dygraphs::`%>%`
  graph %>%
    dygraphs::dyLimit(
      limit = max_td,
      label = "max_treedepth",
      color = "black",
      labelLoc = "right",
      strokePattern = "solid"
    ) %>%
    dygraphs::dyAxis(
      "y",
      valueRange = c(0, max_td * 8 / 7),
      pixelsPerLabel = 20,
      drawGrid = FALSE
    )
})
treedepth_ndivergent_hist <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_td <- .treedepth_pw[if (!is.null(sel)) sel, , drop=FALSE]
  df_nd <- .ndivergent_pw[if (!is.null(sel)) sel, , drop=FALSE]
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = "All")
})
treedepth_ndivergent0_hist <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_td <- .treedepth_pw[if (!is.null(sel)) sel, , drop=FALSE]
  df_nd <- .ndivergent_pw[if (!is.null(sel)) sel, , drop=FALSE]
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = 0)
})
treedepth_ndivergent1_hist <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_td <- .treedepth_pw[if (!is.null(sel)) sel, , drop=FALSE]
  df_nd <- .ndivergent_pw[if (!is.null(sel)) sel, , drop=FALSE]
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = 1)
})
treedepth_vs_lp <- reactive({
  sp_nuts_check()
  lp_check()
  chain <- diagnostic_chain()
  sel <- selected_range()
  treedepth <- .treedepth_pw[if (!is.null(sel)) sel,-1L, drop = FALSE] # drop iterations column
  lp <- SAMPS_post_warmup[if (!is.null(sel)) sel, , lp_name]
  .sampler_param_vs_param(
    p = lp,
    sp = treedepth,
    p_lab = lp_lab,
    sp_lab = treedepth_lab,
    chain = chain,
    violin = TRUE
  )
})
treedepth_vs_accept_stat <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_td <- .treedepth_pw[if (!is.null(sel)) sel,-1L, drop = FALSE] # drop iterations column
  df_as <- .accept_stat_pw[if (!is.null(sel)) sel,-1L, drop = FALSE]
  .sampler_param_vs_sampler_param_violin(
    df_td,
    df_as,
    lab_x = treedepth_lab,
    lab_y = metrop_lab,
    chain = chain
  )
})


# N divergent -------------------------------------------------------------
dynamic_trace_diagnostic_ndivergent <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  samps <- .ndivergent_pw[,-1]
  stack <- FALSE
  graph <- do.call(
    ".dynamic_trace_diagnostics",
    args = list(
      param_samps = samps,
      param_name = ndivergent_lab,
      chain = chain,
      stack = stack,
      group = "ndivergent_information"
    )
  )
  
  `%>%` <- dygraphs::`%>%`
  graph %>% dygraphs::dyAxis(
    "y",
    valueRange = c(0, 1.1),
    pixelsPerLabel = 1e4,
    drawGrid = FALSE
  )
})
ndivergent_vs_lp <- reactive({
  sp_nuts_check()
  lp_check()
  chain <- diagnostic_chain()
  sel <- selected_range()
  ndivergent <- .ndivergent_pw[if (!is.null(sel)) sel,-1L, drop = FALSE] # drop iterations column
  lp <- SAMPS_post_warmup[if (!is.null(sel)) sel, , lp_name]
  .sampler_param_vs_param(
    p = lp,
    sp = ndivergent,
    p_lab = lp_lab,
    sp_lab = ndivergent_lab,
    chain = chain,
    violin = TRUE
  )
})
ndivergent_vs_accept_stat <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  sel <- selected_range()
  df_nd <- .ndivergent_pw[if (!is.null(sel)) sel,-1L, drop = FALSE] # drop iterations column
  df_as <- .accept_stat_pw[if (!is.null(sel)) sel,-1L, drop = FALSE]
  .sampler_param_vs_sampler_param_violin(
    df_nd,
    df_as,
    lab_x = ndivergent_lab,
    lab_y = metrop_lab,
    chain = chain
  )
})



# energy ------------------------------------------------------------------
energy_hist <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  np <- bayesplot::nuts_params(SAMPLER_PARAMS_post_warmup, pars = "energy__")
  if (chain != 0)
    np <- subset(np, Chain == chain)
  
  schm <- unlist(bayesplot::color_scheme_get("brightblue"))
  schm["light"] <- base_fill
  schm["mid"] <- overlay_fill
  schm["light_highlight"] <- vline_base_clr
  schm["mid_highlight"] <- pt_outline_clr
  bayesplot::color_scheme_set(unname(schm))
  
  bayesplot::mcmc_nuts_energy(np, merge_chains = isTRUE(chain != 0)) + 
    ggplot2::facet_wrap(~ Chain, labeller = "label_both") + 
    thm_no_yaxs + 
    bayesplot::facet_bg(FALSE) + 
    bayesplot::facet_text(size = rel(1)) + 
    bayesplot::legend_move("right") + 
    theme(legend.text.align = 0, legend.text = element_text(size = rel(1.5)))
})


# model parameter ---------------------------------------------------------
dynamic_trace_diagnostic_parameter <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  transform_x <- diagnostic_param_transform()
  samps <- SAMPS_post_warmup[, , param]
  lab <- param
  if (transform_x != "identity") {
    t_x <- get(transform_x)
    samps <- t_x(samps)
    lab <- paste0(transform_x, "(", param, ")")
  }
  stack <- FALSE
  do.call(
    ".dynamic_trace_diagnostics",
    args = list(
      param_samps = samps,
      param_name = paste("Parameter:", lab),
      chain = chain,
      stack = stack
    )
  )
})
param_vs_lp <- reactive({
  sp_nuts_check()
  lp_check()
  param <- diagnostic_param()
  chain <- diagnostic_chain()
  sel <- selected_range()
  lp <- SAMPS_post_warmup[if (!is.null(sel)) sel, , lp_name]
  transform_x <- diagnostic_param_transform()
  samps <- SAMPS_post_warmup[if (!is.null(sel)) sel, , param, drop = FALSE]
  divergent <- .ndivergent_pw[if (!is.null(sel)) sel,-1L, drop = FALSE]
  td <- .treedepth_pw[if (!is.null(sel)) sel,-1L, drop = FALSE]
  hit_max_td <- apply(td, 2L, function(y) as.numeric(y == MISC$max_td))
  lab <- param
  if (transform_x != "identity") {
    t_x <- get(transform_x)
    samps <- t_x(samps)
    lab <- paste0(transform_x, "(", param, ")")
  }
  samps <- as.data.frame(samps)
  .sampler_param_vs_param(
    p = lp,
    sp = samps,
    divergent = divergent,
    hit_max_td = as.data.frame(hit_max_td),
    p_lab = lp_lab,
    sp_lab = lab,
    chain = chain,
    violin = FALSE
  )
})
param_vs_accept_stat <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  sel <- selected_range()
  metrop <- .accept_stat_pw[if (!is.null(sel)) sel, -1L, drop=FALSE] # drop iterations column
  transform_x <- diagnostic_param_transform()
  samps <- SAMPS_post_warmup[if (!is.null(sel)) sel,, param]
  divergent <- .ndivergent_pw[if (!is.null(sel)) sel, -1L, drop=FALSE]
  td <- .treedepth_pw[if (!is.null(sel)) sel, -1L, drop=FALSE]
  hit_max_td <- apply(td, 2L, function(y) as.numeric(y == MISC$max_td))
  lab <- param
  if (transform_x != "identity") {
    t_x <- get(transform_x)
    samps <- t_x(samps)
    lab <- paste0(transform_x, "(", param, ")")
  }
  graph <- .sampler_param_vs_param(
    p = samps,
    sp = metrop,
    divergent = divergent,
    hit_max_td = as.data.frame(hit_max_td),
    chain = chain,
    p_lab = lab,
    sp_lab = metrop_lab
  )
  graph + coord_flip()
})
param_vs_stepsize <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  sel <- selected_range()
  stepsize <- .stepsize_pw[if (!is.null(sel)) sel, -1L, drop=FALSE] # drop iterations column
  transform_x <- diagnostic_param_transform()
  samps <- SAMPS_post_warmup[if (!is.null(sel)) sel, , param]
  lab <- param
  if (transform_x != "identity") {
    t_x <- get(transform_x)
    samps <- t_x(samps)
    lab <- paste0(transform_x, "(", param, ")")
  }
  .sampler_param_vs_param(
    p = samps,
    sp = stepsize,
    p_lab = lab,
    sp_lab = stepsize_lab,
    chain = chain,
    violin = TRUE
  )
})
param_vs_treedepth <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  sel <- selected_range()
  treedepth <- .treedepth_pw[if (!is.null(sel)) sel, -1L, drop=FALSE] # drop iterations column
  transform_x <- diagnostic_param_transform()
  samps <- SAMPS_post_warmup[if (!is.null(sel)) sel, , param]
  lab <- param
  if (transform_x != "identity") {
    t_x <- get(transform_x)
    samps <- t_x(samps)
    lab <- paste0(transform_x, "(", param, ")")
  }
  .sampler_param_vs_param(
    p = samps,
    sp = treedepth,
    p_lab = lab,
    sp_lab = treedepth_lab,
    chain = chain,
    violin = TRUE
  )
})
p_hist <- reactive({
  sp_nuts_check()
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  sel <- selected_range()
  transform_x <- diagnostic_param_transform()
  samps <- SAMPS_post_warmup[if (!is.null(sel)) sel,, param]
  lab <- param
  if (transform_x != "identity") {
    t_x <- get(transform_x)
    samps <- t_x(samps)
    lab <- paste0(transform_x, "(", param, ")")
  }
  df <- as.data.frame(cbind(iterations = 1:NROW(samps), samps))
  .p_hist(df, lab = lab, chain = chain)
})

# outputs ---------------------------------------------------
trace_nms <- c("parameter", "lp", "accept_stat", 
               "treedepth", "stepsize", "ndivergent")
hmc_plots <- c("accept_stat_trace", "accept_stat_hist","accept_stat_vs_lp",
               "lp_trace", "lp_hist", "ndivergent_trace", "treedepth_trace",
               "treedepth_ndivergent_hist","treedepth_ndivergent0_hist", 
               "treedepth_ndivergent1_hist", "treedepth_vs_lp", "ndivergent_vs_lp", 
               "treedepth_vs_accept_stat", "ndivergent_vs_accept_stat", 
               "stepsize_vs_lp", "stepsize_vs_accept_stat", "stepsize_trace", 
               "param_vs_lp", "param_vs_accept_stat", "param_vs_stepsize", 
               "param_vs_treedepth", "p_trace", "p_hist", 
               "energy_hist")
for (j in seq_along(trace_nms)) {
  local({
    fn <- paste0("dynamic_trace_diagnostic_", trace_nms[j]) 
    output[[paste0(fn,"_out")]] <- dygraphs::renderDygraph(do.call(fn, list()))
  })
}
for (i in seq_along(hmc_plots)) {
  local({
    fn <- hmc_plots[i]
    output[[paste0(fn, "_out")]] <- renderPlot({
      x <- suppressMessages(do.call(fn, list()))
      suppress_and_print(x)
    })
  })
}
output$diagnostic_chain_text <- renderText({
  chain <- diagnostic_chain()
  if (chain == 0)
    return("All chains")
  paste("Chain", chain)
})

output$diagnostics_warnings_text <- renderText({
  sp_nuts_check()
  divs <- sum(.ndivergent_pw[, -1])
  hits <- sum(.treedepth_pw[, -1] == MISC$max_td)
  d <- divs > 0
  h <- hits > 0
  if (d && h) {
    msg <- paste(
      "WARNINGS -- Diverging error:", divs, "iterations.",
      "Maximum treedepth reached:", hits, "iterations."
    )
  } else if (d && !h) {
    msg <- paste("WARNINGS -- Diverging error:", divs, "iterations.")
  } else if (!d && h) {
    msg <- paste("WARNINGS -- Maximum treedepth reached:", hits, "iterations.")
  } else {
    msg <- NULL
  }
  msg
})
