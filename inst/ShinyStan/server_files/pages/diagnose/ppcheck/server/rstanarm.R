output$pp_rep_vs_obs_out_rstanarm <- renderPlot({
  overlay <- input$pp_rep_vs_obs_overlay_rstanarm == "density"
  if (overlay)
    print(PPC_plots[["pp_check_dens"]])
  else
    suppress_and_print(PPC_plots[["pp_check_hist"]])
}, bg = "transparent")

output$pp_hists_test_statistics_mean_out_rstanarm <- renderPlot({
  suppress_and_print(PPC_plots[["pp_check_stat_mean"]])
}, bg = "transparent")

output$pp_hists_test_statistics_sd_out_rstanarm <- renderPlot({
  suppress_and_print(PPC_plots[["pp_check_stat_sd"]])
}, bg = "transparent")

output$pp_hists_test_statistics_min_out_rstanarm <- renderPlot({
  suppress_and_print(PPC_plots[["pp_check_stat_min"]])
}, bg = "transparent")

output$pp_hists_test_statistics_max_out_rstanarm <- renderPlot({
  suppress_and_print(PPC_plots[["pp_check_stat_max"]])
}, bg = "transparent")

output$pp_y_vs_avg_rep_out_rstanarm <- renderPlot({
  print(PPC_plots[["pp_check_scatter"]])
}, bg = "transparent")

output$pp_hist_resids_out_rstanarm <- renderPlot({
  suppress_and_print(PPC_plots[["pp_check_resid"]])
}, bg = "transparent")
