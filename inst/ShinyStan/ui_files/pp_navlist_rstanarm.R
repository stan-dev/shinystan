navlistPanel(
  id = "pp_navlist",
  widths = c(3, 9),
  well = TRUE,
  tabPanel(
    "Distribution of observed data vs replications",
    div(
      br(),
      h4(withMathJax(plot_descriptions["plot_hists_rep_vs_obs"])),
      br(),
      # actionButton("resample_go", label = "Show different replications", icon = icon("refresh")),
      fluidRow(
        column(
          width = 5,
          radioButtons(
            "pp_rep_vs_obs_overlay_rstanarm",
            label = "",
            choices = list(Histograms = "histograms", "Overlaid Densities" = "density"),
            inline = TRUE
          )
        )
      ), 
      plotOutput("pp_rep_vs_obs_out_rstanarm"),
      br()
    )
  ),
  tabPanel(
    "Distributions of test statistics",
    div(
      br(),
      h4(withMathJax(plot_descriptions["plot_test_statistics"])),
      helpText(
        "The blue lines show \\(T(y)\\), the value of the statistic computed from the observed data."
      ),
      fluidRow(
        column(
          width = 6,
          plotOutput("pp_hists_test_statistics_mean_out_rstanarm", height = "200px")
        ), 
        column(
          width = 6,
          plotOutput("pp_hists_test_statistics_sd_out_rstanarm", height = "200px")
        )
      ), 
      br(),
      fluidRow(
        column(
          width = 6,
          plotOutput("pp_hists_test_statistics_min_out_rstanarm", height = "200px")
        ),
        column(
          width = 6,
          plotOutput("pp_hists_test_statistics_max_out_rstanarm", height = "200px")
        )
      ), 
      br()
    )
  ),
  tabPanel(
    "Scatterplots",
    div(
      br(),
      h4(withMathJax(plot_descriptions["plot_obs_vs_avg_y_rep"])),
      plotOutput("pp_y_vs_avg_rep_out_rstanarm"),
      br()
    )
  ), 
  tabPanel(
    "Histograms of residuals",
    div(
      br(),
      h4(withMathJax(plot_descriptions["plot_hist_resids"])),
      # br(),
      # actionButton("resample_resids_go", label = "Show a different replication", icon = icon("refresh")),
      # br(),br(),
      plotOutput("pp_hist_resids_out_rstanarm")
    )
  ), 
  "About",
  tabPanel(
    "About graphical posterior predictive checking",
    source(file.path("ui_files", "pp_about.R"), local = TRUE)$value
  )
)
