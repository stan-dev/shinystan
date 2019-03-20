server <- function(input, output, session) {
  # utilities
  source("MODULES/UTILS/plotOptions.r", local = TRUE)
  source("MODULES/UTILS/report.r", local = TRUE)
  
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
  
  # internal functions or events that are required for general use.
  # save and close button
  observeEvent(
    input$save_and_close_button, 
    stopApp()
  )
  
  # transformation options
  transformation_choices <-
    c(
      "abs", "atanh",
      cauchit = "pcauchy", "cloglog",
      "exp", "expm1",
      "identity", "inverse", inv_logit = "plogis",
      "log", "log10", "log2", "log1p", logit = "qlogis",
      probit = "pnorm",
      "square", "sqrt"
    )
  
  inverse <- function(x) 1/x
  cloglog <- function(x) log(-log1p(-x))
  square <- function(x) x^2
  
  # to reduce code in modules for selecting theme's in Options.
  select_theme <- function(name){
    switch(name,
           "bayesplot default" = "theme_default()", 
           "classic" = "theme_classic()",
           "dark" = "theme_dark()")
  }
  
  
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
  callModule(diagnose, "diagnoseHomepage")
  # estimate tab
  callModule(estimate, "estimateHomepage")
  # about tab
  callModule(about, "about")
  callModule(modelCode, "modelCode")
  callModule(help, "help")
  callModule(glossary, "glossary")
  
}

