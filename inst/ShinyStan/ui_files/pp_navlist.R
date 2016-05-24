navlistPanel(
  id = "pp_navlist",
  widths = c(3, 9),
  well = TRUE,
  tabPanel(
    "Select data",
    div(
      br(),
      uiOutput("ui_pp_get_y"),
      br(),
      uiOutput("ui_pp_get_yrep")
    )
  ), 
  "Plots",
  tabPanel(
    "Distribution of observed data vs replications",
    div(
      br(),
      h4(withMathJax(plot_descriptions["plot_hists_rep_vs_obs"])),
      br(),
      actionButton(
        "resample_hist_go",
        label = "Show different replications",
        icon = icon("refresh")
      ),
      fluidRow(
        column(
          width = 5,
          radioButtons(
            "pp_hists_rep_vs_obs_type",
            label = "",
            choices = list(Histograms = "histogram", Densities = "density"),
            inline = TRUE
          )
        ),
        column(
          width = 4,
          conditionalPanel(
            condition = "input.pp_hists_rep_vs_obs_type == 'density'",
            radioButtons(
              "pp_hists_rep_vs_obs_overlay",
              label = "",
              choices = list(Separate = FALSE, Overlay = TRUE),
              selected = FALSE,
              inline = TRUE
            )
          )
        )
      ),
      plotOutput("pp_hists_rep_vs_obs_out", width = "90%"),
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
      radioButtons(
        "pp_hists_test_statistics_type",
        label = "",
        choices = list(Histograms = "histogram", Densities = "density"),
        inline = TRUE
      ),
      fluidRow(
        column(
          width = 6,
          plotOutput("pp_hists_test_statistics_mean_out", height = "200px")
        ),
        column(
          width = 6,
          plotOutput("pp_hists_test_statistics_sd_out", height = "200px")
        )
      ),
      br(),
      fluidRow(
        column(
          width = 6,
          plotOutput("pp_hists_test_statistics_min_out", height = "200px")
        ),
        column(
          width = 6,
          plotOutput("pp_hists_test_statistics_max_out", height = "200px")
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
      checkboxInput("pp_zoom_to_zero", "Zoom to include (0,0)", value = FALSE),
      plotOutput("pp_y_vs_avg_rep_out", height = "250px", width = "80%"),
      # h5(withMathJax(plot_descriptions["plot_avg_rep_vs_avg_resid_rep"])),
      # plotOutput("pp_avg_rep_vs_avg_resid_rep_out", height = "250px", width = "80%"),
      br()
    )
  ), 
  tabPanel(
    "Histograms of residuals",
    div(
      br(),
      h4(withMathJax(plot_descriptions["plot_hist_resids"])),
      br(),
      actionButton(
        "resample_resids_go",
        label = "Show a different replication",
        icon = icon("refresh")
      ),
      br(),br(),
      plotOutput("pp_hist_resids_out", height = "250px", width = "75%")
    )
  ),
  "About",
  tabPanel(
    "About graphical posterior predictive checking",
    source(file.path("ui_files", "pp_about.R"), local = TRUE)$value
  ),
  tabPanel(
    "Tutorial",
    includeMarkdown("markdown/pp_check_tutorial.md")
  )
)
