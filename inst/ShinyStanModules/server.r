server <- function(input, output, session) {
  
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

