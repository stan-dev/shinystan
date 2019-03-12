server <- function(input, output, session) {
  # home tab
  source("MODULES/HOME/homepage.r", local = TRUE)
  source("MODULES/HOME/warnings.r", local = TRUE)
  
  # diagnoses tab
  source("MODULES/DIAGNOSE/diagnoseHomepage.r", local = TRUE)
  
  source("MODULES/DIAGNOSE/divergentScatter.r", local = TRUE)
  source("MODULES/DIAGNOSE/divergentTransitions.r", local = TRUE)
  source("MODULES/DIAGNOSE/energy.r", local = TRUE)
  source("MODULES/DIAGNOSE/treedepth.r", local = TRUE)
  source("MODULES/DIAGNOSE/stepSize.r", local = TRUE)
  source("MODULES/DIAGNOSE/parallelCoordinates.r", local = TRUE)
  source("MODULES/DIAGNOSE/pairs.r", local = TRUE)
  source("MODULES/DIAGNOSE/acceptance.r", local = TRUE)
  
  source("MODULES/DIAGNOSE/tracePlot.r", local = TRUE)
  source("MODULES/DIAGNOSE/rhat_n_eff_se_mean.r", local = TRUE)
  source("MODULES/DIAGNOSE/autoCorrelation.r", local = TRUE)
  
  source("MODULES/DIAGNOSE/statsTableHMC.r", local = TRUE)
  source("MODULES/DIAGNOSE/rhat_n_eff_se_mean_stats.r", local = TRUE)
  source("MODULES/DIAGNOSE/autoCorrelationStats.r", local = TRUE)
  
  # estimate tab
  source("MODULES/ESTIMATE/estimateHomepage.r", local = TRUE)
  
  source("MODULES/ESTIMATE/visualEstimate.r", local = TRUE)
  source("MODULES/ESTIMATE/scatterPlot.r", local = TRUE)
  source("MODULES/ESTIMATE/densityPlot.r", local = TRUE)
  source("MODULES/ESTIMATE/histogramPlot.r", local = TRUE)
  source("MODULES/ESTIMATE/intervalsPlot.r", local = TRUE)
  source("MODULES/ESTIMATE/areasPlot.r", local = TRUE)
  
  source("MODULES/ESTIMATE/numericalEstimate.r", local = TRUE)
  source("MODULES/ESTIMATE/summaryTable.r", local = TRUE)
  
  # more tab
  source("MODULES/MORE/about.r", local = TRUE)
  source("MODULES/MORE/modelCode.r", local = TRUE)
  source("MODULES/MORE/help.r", local = TRUE)
  source("MODULES/MORE/glossary.r", local = TRUE)
  # source("MODULES/MORE/test.r", local = TRUE)
  # report
  source("MODULES/REPORT/report2.r", local = TRUE)
  
  # internal functions or events that are required for general use.
  # save and close button
  observeEvent(
    input$save_and_close_button, 
    stopApp()
  )
  
  # this is used to reference the HTML links to the correct page on the homepage
  # module. Need to find a way to actually incorporate this in the module and not
  # in the main server file.
  toc_entries <- c("Estimate", "Diagnose", "Explore", "Model Code")
  observe({
    local({
      lapply(toc_entries, function(x) {
        id <- paste0("toc_", if (x == "Model Code") "more" else tolower(x))
        shinyjs::onclick(id, updateTabsetPanel(session, "nav", selected = x))
     })
    })
  })

  # calling modules
  
  # home tab
  callModule(homepage, "homepage")
  # diagnose tab
  getDiagnosePlots <- callModule(diagnose, "diagnoseHomepage")
  # estimate tab
  callModule(estimate, "estimateHomepage")
  # about tab
  callModule(about, "about")
  callModule(modelCode, "modelCode")
  callModule(help, "help")
  callModule(glossary, "glossary")
  # callModule(report, "report", ggplotsList = getDiagnosePlots[getDiagnosePlots != "pairsPlot"],
  #            getPairsPlot = getDiagnosePlots[getDiagnosePlots == "pairsPlot"], 
  #            getParcoordPlot = getDiagnosePlots[getDiagnosePlots == "parcoordPlot"])
  
}

