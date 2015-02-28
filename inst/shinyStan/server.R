# shinyStan
# Copyright (c) 2015 Jonah Gabry & shinyStan team
# All rights reserved.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

pkgs <- c("shiny", "shinyBS")
shinyStan:::Librarian(pkgs)

# options(shiny.trace=TRUE)
# load the helper functions
source("helper_functions/shinyStan_helpers.R", local=TRUE)

# extract the content of the shinystan_object slots
source("server_files/utilities/extract_shinystan_object.R", local=TRUE)

# Begin shinyServer -------------------------------------------------------
# _________________________________________________________________________
shinyServer(function(input, output, session) {

  # Stop the app when button is clicked
  observe({
    if(input$nav == "quit") {
      stopApp()
    }
  })

  # load utility functions & all files for dynamic UIs
  output_files <- list.files(path = "server_files/outputs", full.names = TRUE)
  utility_files <- list.files(path = "server_files/utilities", full.names = TRUE)
  ui_files <- list.files(path = "server_files/dynamic_ui", full.names = TRUE)
  help_files <- list.files(path = "server_files/help_and_glossary", full.names = TRUE)

  for (f in c(ui_files, utility_files, output_files, help_files)) {
    source(f, local = TRUE)
  }

  #### tooltips & popovers ####  
  tooltip_ids <- c("download_multiview", "dynamic_trace_stack", "download_all_summary", "tex_options")
  tooltip_msgs <- c("Will be a list object with one element per plot.", 
                    "If 'Stacked' is selected, the chains will be stacked on top of one another rather than drawing them independently. The first series specified in the input data will wind up on top of the chart and the last will be on bottom. Note that the y-axis values no longer correspond to the true values when this option is enabled.",
                    "Save as data.frame (.RData)", "Print latex table to R console")
  for (id in seq_along(tooltip_ids)) {
    addTooltip(session, id = tooltip_ids[id], trigger = "hover", placement = "right",
               title = tooltip_msgs[id])
  }
  
  popover_ids <- c(paste0("tex_", c("booktabs", "long")))
  popover_msgs <- c("From print.xtable {xtable}: If TRUE, the toprule, midrule and bottomrule tags from the LaTex 'booktabs' package are used rather than hline for the horizontal line tags.",
                     "For tables longer than a single page. If TRUE, will use LaTeX package 'longtable'.")
  for (id in seq_along(popover_ids)) {
    addPopover(session, id = popover_ids[id], trigger = "hover", placement = "right",
               title = popover_msgs[id])
  }

  #### DATATABLE: summary stats (all parameters) ####
  output$all_summary_out <- renderDataTable({
    summary_stats()
  }, options = list(
    search = list(regex = TRUE), # allow regular expression when searching for parameter names
    processing = TRUE,
    pagingType = "full", # show only first, previous, next, last buttons (no page numbers)
    pageLength = 10,
    lengthMenu = list(c(5, 10, 20, 50, -1), c('5', '10', '20', '50', 'All')),
    orderClasses = TRUE,
    scrollY = 400,
    scrollX = TRUE,
    scrollCollapse = FALSE,
    columnDefs = list(list(width="100px", targets=list(0))),
    initComplete = I( # change background color of table header
      'function(settings, json) {
      $(this.api().table().header()).css({"background-color": "#346fa1", "color": "#fff"});
      }'),
    rowCallback = I(
      'function(row, data) {
        // Bold cells in the first column
          $("td:eq(0)", row).css("font-weight", "bold");
      }')
  ))
  # download the table
  output$download_all_summary <- downloadHandler(
    filename = paste0('shinystan_summary_stats.RData'),
    content = function(file) {
      shinystan_summary_stats <- summary_stats()
      save(shinystan_summary_stats, file = file)
    }
  )
  # latex the table
  # Stop the app when button is clicked
#   observe({
#     latex <- input$tex_go
#     if (latex > 0) summary_stats_latex()
#   })
  observeEvent(input$tex_go, handlerExpr = {
    summary_stats_latex()
  })
  #### DATATABLE: summary stats (sampler) ####
  output$sampler_summary <- renderDataTable({
    summary_stats_sampler()
  }, options = list(
    processing = TRUE,
    scrollX = TRUE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    orderClasses = TRUE,
    initComplete = I( # change background color of table header
      'function(settings, json) {
      $(this.api().table().header()).css({"background-color": "#346fa1", "color": "#fff"});
      }'),
    rowCallback = I(
      'function(row, data) {
        // Bold cells in the first column
          $("td:eq(0)", row).css("font-weight", "bold");
      }')
  ))
  #### PLOT: multiple parameters ####
  output$plot_param_vertical_out <- renderPlot({
    plot_param_vertical()
  }, height = calc_height_param_plot)
  # download the plot
  output$download_multiparam_plot <- downloadHandler(
    filename = 'shinystan_param_plot.RData',
    content = function(file) {
      shinystan_param_plot <- plot_param_vertical()
      save(shinystan_param_plot, file = file)
    }
  )
  #### PLOT: n_eff / total sample size ####
  output$n_eff_plot_out <- renderPlot({
    x <- n_eff_plot()
    suppressMessages(suppressWarnings(print(x)))
  })
  #### PLOT: ratio of mcmc se to posterior sd  ####
  output$mcse_over_sd_plot_out <- renderPlot({
    x <- mcse_over_sd_plot()
    suppressMessages(suppressWarnings(print(x)))
  })
  #### PLOT: rhat ####
  output$rhat_plot_out <- renderPlot({
    x <- rhat_plot()
    suppressMessages(suppressWarnings(print(x)))
  })
  #### TEXT: n_eff warnings ####
  output$n_eff_warnings_title <- renderText({
    paste0("The following parameters have an effective sample size less than ", input$n_eff_threshold,"% of the total number of samples: ")
  })
  output$n_eff_warnings <- renderText({
    n_eff_warnings()
  })
  #### TEXT: rhat warnings ####
  output$rhat_warnings_title <- renderText({
    paste0("The following parameters have an Rhat value above ", input$rhat_threshold,": ")
  })
  output$rhat_warnings <- renderText({
    rhat_warnings()
  })
  #### TEXT: mcmc se to posterior sd warnings ####
  output$mcse_over_sd_warnings_title <- renderText({
    paste0("The following parameters have a Monte Carlo standard error greater than ", input$mcse_threshold ,"% of the posterior standard deviation:")
  })
  output$mcse_over_sd_warnings <- renderText({
    mcse_over_sd_warnings()
  })

  #### PLOT: autocorrelation ####
  output$autocorr_plot_out <- renderPlot({
    autocorr_plot()
  })
  # download the plot
  output$download_autocorr <- downloadHandler(
    filename = paste0('shinystan_autocorr.RData'),
    content = function(file) {
      shinystan_autocorr <- autocorr_plot
      save(shinystan_autocorr, file = file)
    }
  )
  #### PLOT: trace plots (multiple parameters) ####
  output$multi_trace_plot_out <- renderPlot({
    x <- multi_trace_plot()
    suppressWarnings(print(x)) # this avoids warnings about removing rows when using tracezoom feature
  }, height = calc_height_trace_plot)
  # download the plot
  output$download_multi_trace <- downloadHandler(
    filename = paste0('shinystan_multi_trace.RData'),
    content = function(file) {
      shinystan_multi_trace <- multi_trace_plot()
      save(shinystan_multi_trace, file = file)
    }
  )

  #### TEXT: parameter name ####
  output$param_name <- renderText({
    input$param
  })
  #### TABLE: summary stats (single parameter) ####
  output$parameter_summary_out <- renderDataTable({
    as.data.frame(round(parameter_summary(), 2))
  }, options = list(
    paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE,
    initComplete = I( # change background color of table header
      'function(settings, json) {
      $(this.api().table().header()).css({"background-color": "white", "color": "black"});
      }')
  ))
  #### PLOT: Multiview ####
  output$multiview_param_name <- renderUI(strong(style = "font-size: 250%; color: #f9dd67;", input$param))
  output$multiview_trace <- renderPlot(trace_plot_multiview())
  output$multiview_density <- renderPlot(density_plot_multiview())
  output$multiview_autocorr <- renderPlot(autocorr_plot_multiview())
  # download multiview plot
  output$download_multiview <- downloadHandler(
    filename = 'shinystan_multiview.RData',
    content = function(file) {
      param_name <- input$param
      shinystan_multiview <- list()
      shinystan_multiview[[paste0("trace_", param_name)]] <- trace_plot_multiview()
      shinystan_multiview[[paste0("density", param_name)]] <- density_plot_multiview()
      shinystan_multiview[[paste0("ac_", param_name)]] <- autocorr_plot_multiview()
      save(shinystan_multiview, file = file)
    }
  )

  ### PLOT: histogram ####
  output$hist_plot_out <- renderPlot({
    x <- hist_plot()
    suppressMessages(suppressWarnings(print(x)))
  })
  # download plot
  output$download_histogram <- downloadHandler(
    filename = 'shinystan_histogram.RData',
    content = function(file) {
      shinystan_histogram <- hist_plot()
      save(shinystan_histogram, file = file)
    }
  )
  #### PLOT: dynamic trace plot ####
  output$dynamic_trace_plot_out <- dygraphs::renderDygraph({
    dynamic_trace_plot()
  })
  ### PLOT: density ####
  output$density_plot_out <- renderPlot({
    density_plot()
  })
  # download plot
  output$download_density <- downloadHandler(
    filename = 'shinystan_density.RData',
    content = function(file) {
      shinystan_density <- density_plot()
      save(shinystan_density, file = file)
    }
  )
  #### PLOT: trivariate 3D ####
  output$trivariate_plot_out <- threejs::renderScatterplotThree({
    trivariate_plot()
  })
  #### PLOT: bivariate ####
  output$bivariate_plot_out <- renderPlot({
    bivariate_plot()
  })
  output$download_bivariate <- downloadHandler(
    filename = 'shinystan_bivariate.RData',
    content = function(file) {
      shinystan_bivariate <- bivariate_plot()
      save(shinystan_bivariate, file = file)
    }
  )

  #### TEXT: User's model notes ####
#   observe({
#     input$save_user_model_info
#     isolate({
#       if (input$user_model_info != "")
#         shinystan_object@user_model_info <<- input$user_model_info
#     })
#   })
  observeEvent(input$save_user_model_info, handlerExpr = {
    if (input$user_model_info != "")
      shinystan_object@user_model_info <<- input$user_model_info
  })
  output$user_text_saved <- renderText({
    input$save_user_model_info # take dependency on action button
    if (input$save_user_model_info != 0) {
      print(paste("Saved:  ", format(Sys.time(), "%a %b %d %Y %X")))
    }
  })


}) # End shinyServer
