tabsetPanel(
  id = "diagnose_tabset",
  
  #### hmc/nuts plots ####
  tabPanel(
    title = "NUTS (plots)",
    source_ui("diagnostics_customize.R"),
    navlistPanel(
      id = "diagnostics_navlist",
      tabPanel(
        "By model parameter", 
        source_ui("diagnostics_by_parameter.R")
      ),
      tabPanel(
        "Sample information", 
        source_ui("diagnostics_sample.R")
      ),
      tabPanel(
        "Divergence information", 
        source_ui("diagnostics_ndivergent.R")
      ),
      tabPanel(
        "Energy information", 
        source_ui("diagnostics_energy.R")
      ),
      tabPanel(
        "Treedepth information", 
        source_ui("diagnostics_treedepth.R")
      ),
      tabPanel(
        "Step size information", 
        source_ui("diagnostics_stepsize.R")
      ),
      tabPanel(
        "Help", 
        source_ui("diagnostics_help.R")
      ),
      well = FALSE,
      widths = c(2, 10)
    )
  ),
  
  #### hmc/nuts stats ####
  tabPanel(
    title = "HMC/NUTS (stats)",
    h2("Summary of sampler parameters"),
    a_glossary("open_glossary_from_nuts_table"),
    br(),
    source_ui("sampler_stats_customize.R"),
    DT::dataTableOutput("sampler_summary"),
    br()
  ),
  
  #### rhat, n_eff, mcse ####
  tabPanel(
    title = "\\(\\hat{R}, n_{eff}, \\text{se}_{mean}\\)",
    source_ui("rhat_neff_mcse_layout.R")
  ),
  
  #### autocorrelation ####
  tabPanel(
    title = "Autocorrelation",
    source_ui("autocorr_customize.R"),
    wellPanel(fluidRow(
      column(
        width = 8,
        selectizeInput(
          "ac_params",
          width = "100%",
          label = h5("Select or enter parameter names"),
          choices = .param_list_with_groups,
          multiple = TRUE
        )
      ),
      column(
        width = 3, 
        offset = 1, 
        a_options("autocorr")
      )
    )),
    plotOutput("autocorr_plot_out")
  ),
  
  #### ppcheck ####
  tabPanel(
    title = "PPcheck",
    h2("Graphical posterior predictive checks"),
    h6("Experimental feature"),
    source_ui(if (.has_rstanarm_ppcs)
      "pp_navlist_rstanarm.R" else "pp_navlist.R"),
    br()
  )
)