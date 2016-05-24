vb_check <- function() {
  validate(
    need(
      STAN_METHOD != "variational", 
      message = "Not available for variational inference"
    )
  )
}

n_eff_plot <- reactive({
  vb_check()
  dat <- SUMMARY[, "n_eff"]
  N <- prod(dim(SAMPS_post_warmup)[1:2])
  dat <- data.frame(parameter = names(dat), x = dat / N)
  do.call(".rhat_neff_mcse_hist",
          args = list(
            dat = dat,
            N = nrow(SAMPS_post_warmup),
            which = "n_eff"
          ))
})
rhat_plot <- reactive({
  vb_check()
  dat <- SUMMARY[, "Rhat"]
  dat <- data.frame(parameter = names(dat), x = dat)
  do.call(".rhat_neff_mcse_hist", args = list(dat = dat, which = "rhat"))
})
mcse_over_sd_plot <- reactive({
  vb_check()
  dat <- SUMMARY[, c("se_mean", "sd")]
  dat <- dat[, 1] / dat[, 2]
  dat <- data.frame(parameter = names(dat), x = dat)
  do.call(".rhat_neff_mcse_hist", args = list(dat = dat, which = "mcse"))
})
n_eff_warnings <- reactive({
  vb_check()
  paste(
    .n_eff_warnings(
      SUMMARY,
      threshold = input$n_eff_threshold,
      N_total = length(SAMPS_post_warmup[, , 1L])
    ),
    collapse = "\n"
  )
})
rhat_warnings <- reactive({
  vb_check()
  paste(.rhat_warnings(SUMMARY, threshold = input$rhat_threshold), collapse = "\n")
})
mcse_over_sd_warnings <- reactive({
  vb_check()
  paste(.mcse_over_sd_warnings(SUMMARY, threshold = input$mcse_threshold), collapse = "\n")
})

output$n_eff_warnings_title <- renderText({
  paste0(
    "The following parameters have an effective sample size less than ", 
    input$n_eff_threshold,
    "% of the total sample size: "
  )
})
output$rhat_warnings_title <- renderText({
  paste0(
    "The following parameters have an Rhat value above ", 
    input$rhat_threshold,
    ": "
  )
})
output$mcse_over_sd_warnings_title <- renderText({
  paste0(
    "The following parameters have a Monte Carlo standard error greater than ", 
    input$mcse_threshold,
    "% of the posterior standard deviation:"
  )
})

rhat_neff_mcse <- c("rhat", "n_eff", "mcse_over_sd")
for (i in seq_along(rhat_neff_mcse)) {
  local({
    fn <- paste0(rhat_neff_mcse[i], "_plot")
    output[[paste0(fn, "_out")]] <- renderPlot({
      x <- do.call(fn, list())
      suppress_and_print(x)
    }, bg = "transparent")
  })
  local({
    fn <- paste0(rhat_neff_mcse[i], "_warnings")
    output[[fn]] <- renderText(do.call(fn, list()))
  })
}
