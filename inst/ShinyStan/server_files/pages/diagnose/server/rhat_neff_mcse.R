# This file is part of shinystan
# Copyright (C) Jonah Gabry
#
# shinystan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinystan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.

# rhat, n_eff, mcse -------------------------------------------------------
vb_check <- function() {
  validate(need(stan_method != "variational", 
                message = "Not available for variational inference"))
}

n_eff_plot <- reactive({
  vb_check()
  dat <- fit_summary[,"n_eff"]
  N <- prod(dim(samps_post_warmup)[1:2])
  dat <- data.frame(parameter = names(dat), x = dat / N)
  do.call(".rhat_neff_mcse_hist", args = list(
    dat = dat,
    N = nrow(samps_post_warmup),
    which = "n_eff"
  ))
})
rhat_plot <- reactive({
  vb_check()
  dat <- fit_summary[,"Rhat"]
  dat <- data.frame(parameter = names(dat), x = dat)
  do.call(".rhat_neff_mcse_hist", args = list(
    dat = dat,
    which = "rhat"
  ))
})
mcse_over_sd_plot <- reactive({
  vb_check()
  dat <- fit_summary[, c("se_mean", "sd")]
  dat <- dat[,1] / dat[,2]
  dat <- data.frame(parameter = names(dat), x = dat)
  do.call(".rhat_neff_mcse_hist", args = list(
    dat = dat,
    which = "mcse"
  ))
})
n_eff_warnings <- reactive({
  vb_check()
  paste(.n_eff_warnings(fit_summary, threshold = input$n_eff_threshold), 
        collapse = "\n")
})
rhat_warnings <- reactive({
  vb_check()
  paste(.rhat_warnings(fit_summary, threshold = input$rhat_threshold), 
        collapse = "\n")
})
mcse_over_sd_warnings <- reactive({
  vb_check()
  paste(.mcse_over_sd_warnings(fit_summary, threshold = input$mcse_threshold), 
        collapse = "\n")
})

output$n_eff_warnings_title <- renderText({
  paste0("The following parameters have an effective sample size less than ", 
         input$n_eff_threshold,"% of the total sample size: ")
})
output$rhat_warnings_title <- renderText({
  paste0("The following parameters have an Rhat value above ", 
         input$rhat_threshold,": ")
})
output$mcse_over_sd_warnings_title <- renderText({
  paste0("The following parameters have a Monte Carlo standard error greater than ", 
         input$mcse_threshold ,"% of the posterior standard deviation:")
})

rhat_neff_mcse <- c("rhat", "n_eff", "mcse_over_sd")
for (i in seq_along(rhat_neff_mcse)) {
  local({
    fn <- paste0(rhat_neff_mcse[i], "_plot" )
    output[[paste0(fn,"_out")]] <- renderPlot({
      x <- do.call(fn, list())
      suppress_and_print(x)
    }, bg = "transparent")
  })
  local({
    fn <- paste0(rhat_neff_mcse[i], "_warnings")
    output[[fn]] <- renderText(do.call(fn, list()))
  })
}
