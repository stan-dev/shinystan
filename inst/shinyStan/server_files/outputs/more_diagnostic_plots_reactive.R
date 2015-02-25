# more diagnostic plots
n_eff_plot <- reactive({
  do.call(".rhat_neff_mcse_hist", args = list(
    fit_summary = fit_summary,
    samps = samps_post_warmup,
    which = "n_eff"
  ))
})

rhat_plot <- reactive({
  do.call(".rhat_neff_mcse_hist", args = list(
    fit_summary = fit_summary,
    samps = samps_post_warmup,
    which = "rhat"
  ))
})

mcse_over_sd_plot <- reactive({
  do.call(".rhat_neff_mcse_hist", args = list(
    fit_summary = fit_summary,
    samps = samps_post_warmup,
    which = "mcse"
  ))
})
