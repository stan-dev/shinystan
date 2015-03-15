output$ui_ppcheck_navlist <- renderUI({
  navlistPanel(id = "pp_navlist", widths = c(4,8), well = FALSE,  
               "Data",
               tabPanel("Select data",
                        uiOutput("ui_pp_data")
               ),
               "Plots",
               tabPanel("Distribution of observed data vs replications",
                        uiOutput("ui_pp_hists_rep_vs_obs")
               ),
               tabPanel("Distributions of test statistics",
                        uiOutput("ui_pp_hists_test_statistics")
               ),
               tabPanel("Scatters",
                        uiOutput("ui_pp_scatters")
               ),
               tabPanel("Histogram of residuals",
                        uiOutput("ui_pp_hist_resids")
               ),
               "About",
               tabPanel("About graphical posterior predictive checking",
                        uiOutput("ui_pp_about")
               ),
               tabPanel("Tutorial",
                        includeMarkdown("markdown/pp_check_tutorial.md")
               )
               
  )
})
