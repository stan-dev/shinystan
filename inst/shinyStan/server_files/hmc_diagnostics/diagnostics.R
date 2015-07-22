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

stepsize_vs_lp <- reactive({
  stepsize <- stepsize_pw()[,-1L] # drop iterations column
  lp <- samps_post_warmup[,,"lp__"]
  chain <- diagnostic_chain()
  .sampler_param_vs_param(p = lp, sp = stepsize, 
                          p_lab = "Log Posterior",
                          sp_lab = "Sampled Step Size", 
                          chain = chain, violin = TRUE)
})
stepsize_vs_accept_stat <- reactive({
  df_ss <- stepsize_pw()[,-1L] # drop iterations column
  df_as <- accept_stat_pw()[,-1L] 
  chain <- diagnostic_chain()
  .stepsize_vs_accept_stat(df_ss, df_as, chain)
})
stepsize_trace <- reactive({
  df <- stepsize_pw()
  chain <- diagnostic_chain()
  .stepsize_trace(df, chain)
})

lp_hist <- reactive({
  validate(need(input$diagnostic_chain, message = "Loading..."))
  chain <- diagnostic_chain()
  lp <- samps_post_warmup[,,"lp__"]
  df <- as.data.frame(cbind(iterations = 1:nrow(lp), lp))
  .p_hist(df, lab = "Log Posterior", chain)
})
lp_trace <- reactive({
  lp <- samps_post_warmup[,,"lp__"]
  df <- as.data.frame(cbind(iterations = (warmup_val+1):(warmup_val+nrow(lp)), lp))
  chain <- diagnostic_chain()
  .p_trace(df, lab = "Log Posterior", chain)
})
accept_stat_hist <- reactive({
  df <- accept_stat_pw()
  chain <- diagnostic_chain()
  .accept_stat_hist(df, chain)
})
accept_stat_trace <- reactive({
  df <- accept_stat_pw()
  chain <- diagnostic_chain()
  .accept_stat_trace(df, chain)
}) 
accept_stat_vs_lp <- reactive({
  metrop <- accept_stat_pw()[,-1L] # drop iterations column
  lp <- samps_post_warmup[,,"lp__"]
  chain <- input$diagnostic_chain
  divergent <- ndivergent_pw()[,-1L]
  td <- treedepth_pw()[,-1L]
  hit_max_td <- apply(td, 2L, function(y) as.numeric(y == MISC$max_td))
  .sampler_param_vs_param(p = lp, sp = metrop,
                          divergent = divergent, 
                          hit_max_td = as.data.frame(hit_max_td),
                          p_lab = "Log Posterior",
                          sp_lab = "Mean Metropolis Acceptance", 
                          chain = chain)
})
ndivergent_trace <- reactive({
  df <- ndivergent_pw()
  chain <- diagnostic_chain()
  .ndivergent_trace(df, chain)
})
treedepth_trace <- reactive({
  df <- treedepth_pw()
  chain <- diagnostic_chain()
  max_td <- MISC$max_td
  .treedepth_trace(df, max_td, chain)
})
treedepth_ndivergent_hist <- reactive({
  df_td <- treedepth_pw()
  df_nd <- ndivergent_pw()
  chain <- diagnostic_chain()
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = "All")
})
treedepth_ndivergent0_hist <- reactive({
  df_td <- treedepth_pw()
  df_nd <- ndivergent_pw()
  chain <- diagnostic_chain()
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = 0)
})
treedepth_ndivergent1_hist <- reactive({
  df_td <- treedepth_pw()
  df_nd <- ndivergent_pw()
  chain <- diagnostic_chain()
  .treedepth_ndivergent_hist(df_td, df_nd, chain = chain, divergent = 1)
})
treedepth_vs_lp <- reactive({
  treedepth <- treedepth_pw()[,-1L] # drop iterations column
  lp <- samps_post_warmup[,,"lp__"]
  chain <- diagnostic_chain()
  .sampler_param_vs_param(p = lp, sp = treedepth, 
                          p_lab = "Log Posterior",
                          sp_lab = "Tree Depth", 
                          chain = chain, violin = TRUE)
})
ndivergent_vs_lp <- reactive({
  ndivergent <- ndivergent_pw()[,-1L] # drop iterations column
  lp <- samps_post_warmup[,,"lp__"]
  chain <- diagnostic_chain()
  .sampler_param_vs_param(p = lp, sp = ndivergent, 
                          p_lab = "Log Posterior",
                          sp_lab = "N Divergent", 
                          chain = chain, violin = TRUE)
})
treedepth_vs_accept_stat <- reactive({
  df_td <- treedepth_pw()[,-1L] # drop iterations column
  df_as <- accept_stat_pw()[,-1L] 
  chain <- diagnostic_chain()
  .treedepth_vs_accept_stat(df_td, df_as, chain)
})
ndivergent_vs_accept_stat <- reactive({
  df_nd <- ndivergent_pw()[,-1L] # drop iterations column
  df_as <- accept_stat_pw()[,-1L] 
  chain <- diagnostic_chain()
  .ndivergent_vs_accept_stat(df_nd, df_as, chain)
})

param_vs_lp <- reactive({
  param <- diagnostic_param()
  chain <- diagnostic_chain()
  lp <- samps_post_warmup[,, "lp__"]
  transform_x <- diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[,, param]
  if (transform_x != "x") samps <- t_x(samps)
  samps <- as.data.frame(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  divergent <- ndivergent_pw()[,-1L]
  td <- treedepth_pw()[,-1L]
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
  metrop <- accept_stat_pw()[,-1L] # drop iterations column
  transform_x <- diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[,, param]
  if (transform_x != "x") samps <- t_x(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  divergent <- ndivergent_pw()[,-1L]
  td <- treedepth_pw()[,-1L]
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
  stepsize <- stepsize_pw()[,-1L] # drop iterations column
  transform_x <- diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[,, param]
  if (transform_x != "x") samps <- t_x(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  .sampler_param_vs_param(p = samps, sp = stepsize, 
                          p_lab = lab, sp_lab = "Sampled Step Size", 
                          chain = chain, violin = TRUE)
})
param_vs_treedepth <- reactive({
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  treedepth <- treedepth_pw()[,-1L] # drop iterations column
  transform_x <- diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[,, param]
  if (transform_x != "x") samps <- t_x(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  .sampler_param_vs_param(p = samps, sp = treedepth, p_lab = param,
                          sp_lab = lab, chain = chain, violin = TRUE)
})
p_hist <- reactive({
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  transform_x <- diagnostic_param_transform() # diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[,, param]
  if (transform_x != "x") samps <- t_x(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  df <- as.data.frame(cbind(iterations = 1:nrow(samps), samps))
  .p_hist(df, lab = lab, chain = chain)
})
p_trace <- reactive({
  chain <- diagnostic_chain()
  param <- diagnostic_param()
  transform_x <- diagnostic_param_transform()
  t_x <- eval(parse(text = paste("function(x)", transform_x)))
  samps <- samps_post_warmup[,, param]
  if (transform_x != "x") samps <- t_x(samps)
  lab <- if (transform_x != "x") gsub("x", param, transform_x) else param
  df <- as.data.frame(cbind(iterations = (warmup_val+1):(warmup_val+nrow(samps)), samps))
  .p_trace(df, lab = lab, chain = chain)
})
