diagnoseUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  
  # encapsulate everything in taglist, see https://shiny.rstudio.com/articles/modules.html
  tagList(
    if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS") uiOutput(ns("HMC")),
    if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm != "NUTS") uiOutput(ns("MCMC")),
    if(sso@misc$stan_method != "sampling") uiOutput(ns("VI"))
  )
  
}

diagnose <- function(input, output, session){
  
  if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS"){
    getParcoordPlot <- callModule(parallelCoordinates, "parallelCoordinates")
    getPairsPlot <- callModule(pairs, "pairs")
    getDivergentTransitionsPlot <- callModule(divergentTransitions, "divergentTransitions")
    getDivergentScatterPlot <- callModule(divergentScatter, "divergentScatter")
    getEnergyPlot <- callModule(energy, "energy")
    getTreedepthPlot <- callModule(treedepth, "treedepth")
    getStepSizePlot <- callModule(stepSize, "stepSize")
    getAcceptancePlot <- callModule(acceptance, "acceptance")
    
    getTracePlot <- callModule(tracePlot, "tracePlot")  
    getRhatNeffSEmeanPlots <- callModule(rhat_n_eff_se_mean, "rhat_n_eff_se_mean")
    callModule(autoCorrelation, "autoCorrelation")
    
    callModule(statsTableHMC, "statsTableHMC")
    callModule(rhat_n_eff_se_mean_stats, "rhat_n_eff_se_mean_stats")
    callModule(autoCorrelationStats, "autoCorrelationStats")
    
    getInputReactiveDiagnosePlots <- reactive({
      list(
      "parcoordPlot" = getParcoordPlot(),
      "pairsPlot" = getPairsPlot()
      # "pairsPlot" = NULL,
      # "parcoordPlot" = NULL
      )
    })
    
    getDiagnosePlots <- reactive({
      list("divergentScatterPlot" = getDivergentScatterPlot(),
           # "pairsPlot" = NULL,
           # "parcoordPlot" = NULL,
           "divergentTransitionsPlot" = getDivergentTransitionsPlot(),
           "energyPlot" = getEnergyPlot(),
           "treedepthPlot" = getTreedepthPlot(),
           "stepSizePlot" = getStepSizePlot(),
           "acceptancePlot" = getAcceptancePlot(),
           "tracePlot" = getTracePlot(),
           "rhatPlot" = getRhatNeffSEmeanPlots()["rhatPlot"],
           "n_effPlot" = getRhatNeffSEmeanPlots()["n_effPlot"],
           "se_meanPlot" = getRhatNeffSEmeanPlots()["se_meanPlot"])
    })
    
    
    callModule(report, "report", ggplotsList = getDiagnosePlots, 
               getParcoordPlot = reactive({
                 list("parcoordPlot" = getParcoordPlot())}),
               getPairsPlot = reactive({
                 list("pairsPlot" = getPairsPlot())
               }))
    }
  
  if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm != "NUTS"){
    getTracePlot <- callModule(tracePlot, "tracePlot")  
    getRhatNeffSEmeanPlots <- callModule(rhat_n_eff_se_mean, "rhat_n_eff_se_mean")
    callModule(autoCorrelation, "autoCorrelation")
    
    callModule(rhat_n_eff_se_mean_stats, "rhat_n_eff_se_mean_stats")
    callModule(autoCorrelationStats, "autoCorrelationStats")
  }
  

  output$HMC <- renderUI({
    validate(
      need(sso@misc$stan_method == "sampling", ""),
      need(sso@misc$stan_algorithm == "NUTS", ""))
    tagList(
      tags$head(
        tags$script("src"="func.js")),
      tabsetPanel(
    tabPanel(
      title = "Plots",
      id = session$ns("visualHMC"),
      navlistPanel(
        id = session$ns("HMC_navlist_vis"),
        "NUTS/HMC",
        tabPanel(
          title = "Divergent Scatter",
          id = session$ns("divergentScatterTab"),
          divergentScatterUI(session$ns("divergentScatter"))
        ),
        tabPanel(
          title = "Parallel Coordinates",
          id = session$ns("parallelCoordinatesTab"),
          parallelCoordinatesUI(session$ns("parallelCoordinates"))
        ),
        tabPanel(
          title = "Pairs",
          id = session$ns("pairsTab"),
          pairsUI(session$ns("pairs"))
        ),
        tabPanel(
          title = "Divergence Information",
          id = session$ns("divergentTransitionsTab"),
          divergentTransitionsUI(session$ns("divergentTransitions"))
        ),
        tabPanel(
          title = "Energy Information",
          id = session$ns("energyTab"),
          energyUI(session$ns("energy"))
        ),
        tabPanel(
          title = "Treedepth Information",
          id = session$ns("treedepthTab"),
          treedepthUI(session$ns("treedepth"))
        ),
        tabPanel(
          title = "Step Size Information",
          id = session$ns("stepSizeTab"),
          stepSizeUI(session$ns("stepSize"))
        ),
        tabPanel(
          title = "Acceptance Information",
          id = session$ns("acceptanceTab"),
          acceptanceUI(session$ns("acceptance"))
        ),
        "MCMC",
        tabPanel(
          title = "Trace Plots",
          id = session$ns("traceTab"),
          tracePlotUI(session$ns("tracePlot"))
        ),
        tabPanel(
          title = withMathJax("\\(\\hat{R}, \\text{ } n_{eff}, \\text{ se}_{mean}\\)"),
          id = session$ns("rhat_n_eff_se_meanTab"),
          value = "rhat_neff_se_mean_plot_tab",
          rhat_n_eff_se_meanUI(session$ns("rhat_n_eff_se_mean"))
        ),
        tabPanel(
          title = "Autocorrelation",
          id = session$ns("autocorrelationTab"),
          autoCorrelationUI(session$ns("autoCorrelation"))
        )
      )
    ),
    tabPanel(
      title = "Stats",
      id = session$ns("numericalHMC"),
      navlistPanel(
        id = session$ns("HMC_navlist_num"),
        "NUTS/HMC",
        tabPanel(
          title = "All NUTS/HMC stats",
          id = session$ns("HMCstatTab"),
          statsTableHMCUI(session$ns("statsTableHMC"))
        ),
        "MCMC",
        tabPanel(
          title = withMathJax("\\(\\hat{R}, \\text{ } n_{eff}, \\text{ se}_{mean}\\)"),
          id = session$ns("rhat_n_eff_se_meanTab"),
          rhat_n_eff_se_mean_statsUI(session$ns("rhat_n_eff_se_mean_stats"))
        ),
        tabPanel(
          title = "Autocorrelation",
          id = session$ns("autocorrelationTab"),
          autoCorrelationStatsUI(session$ns("autoCorrelationStats"))
        )
      )
    ),
    tabPanel(
      title = "Report",
      reportUI(session$ns("report")
        )
    )
    )
    )
  })
  
  output$MCMC <- renderUI({
    validate(
      need(sso@misc$stan_method == "sampling", ""), 
      need(sso@misc$stan_algorithm != "NUTS", ""))
    
    lagList(
    tabPanel(
      title = "Plots",
      id = session$ns("visualHMC"),
      navlistPanel(
        id = session$ns("MCMC_navlist_vis"),
        tabPanel(
          title = "Trace Plots",
          id = session$ns("traceTab"),
          tracePlotUI(session$ns("tracePlot"))
        ),
        tabPanel(
          title = withMathJax("\\(\\hat{R}, \\text{ } n_{eff}, \\text{ se}_{mean}\\)"),
          value = "rhat_neff_se_mean_plot_tab",
          id = session$ns("rhat_n_eff_se_meanTab"),
          rhat_n_eff_se_meanUI(session$ns("rhat_n_eff_se_mean"))
        ),
        tabPanel(
          title = "Autocorrelation",
          id = session$ns("autocorrelationTab"),
          autoCorrelationUI(session$ns("autoCorrelation"))
        )
      )
    ),
    tabPanel(
      title = "Stats",
      id = session$ns("numericalHMC"),
      navlistPanel(
        id = session$ns("MCMC_navlist_num"),
        tabPanel(
          title = withMathJax("\\(\\hat{R}, \\text{ } n_{eff}, \\text{ se}_{mean}\\)"),
          id = session$ns("rhat_n_eff_se_meanTab"),
          rhat_n_eff_se_mean_statsUI(session$ns("rhat_n_eff_se_mean_stats"))
        ),
        tabPanel(
          title = "Autocorrelation",
          id = session$ns("autocorrelationTab"),
          autoCorrelationStatsUI(session$ns("autoCorrelationStats"))
        )
      )
    )
    )
    
  })
  
  output$VI <- renderUI({
    validate(need(sso@misc$stan_method == "variational", ""))
    h4("Currently no diagnostics available for Variational Inference")
  })
  
  
  return(reactive({
    list("divergentScatterPlot" = getDivergentScatterPlot(),
         # "parcoordPlot" = getParcoordPlot(),
         # "pairsPlot" = getPairsPlot(),
         "pairsPlot" = NULL,
         "parcoordPlot" = NULL,
         "divergentTransitionsPlot" = getDivergentTransitionsPlot(),
         "energyPlot" = getEnergyPlot(),
         "treedepthPlot" = getTreedepthPlot(),
         "stepSizePlot" = getStepSizePlot(),
         "acceptancePlot" = getAcceptancePlot(),
         "tracePlot" = getTracePlot(),
         "rhatPlot" = getRhatNeffSEmeanPlots()["rhatPlot"],
         "n_effPlot" = getRhatNeffSEmeanPlots()["n_effPlot"],
         "se_meanPlot" = getRhatNeffSEmeanPlots()["se_meanPlot"])
    }))
  
}
