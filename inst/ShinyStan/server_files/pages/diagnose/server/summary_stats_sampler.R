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

# summary statistics for sampler parameters -------------------------------
summary_stats_sampler <- reactive({
  validate(need(stan_algorithm %in% c("NUTS", "HMC"), 
                message = "Only available for algorithm = NUTS"),
           need(input$sampler_warmup, message = "Loading..."))
  sp <- if (input$sampler_warmup == "include") 
    sampler_params else sampler_params_post_warmup
  
  do.call(".sampler_summary", args = list(
    sampler_params  = sp,
    warmup_val      = warmup_val,
    report          = input$sampler_report,
    digits          = input$sampler_digits
  ))
})

output$sampler_summary <- DT::renderDataTable({
  DT::datatable({
    summary_stats_sampler()
  }, options = list(
    # rownames = FALSE,
    processing = TRUE,
    deferRender = TRUE,
    scrollX = TRUE,
    scrollY = "200px",
    scrollCollapse = TRUE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE
  )
  )
})
