visualEstimateUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  
    navlistPanel(
      id = ns("HMC_navlist"),
      tabPanel(
        title = "Intervals",
        id = ns("intervalsTab"),
        intervalsPlotUI(ns("intervalsPlot"))
      ),
      tabPanel(
        title = "Areas",
        id = ns("areasTab"),
        areasPlotUI(ns("areasPlot"))
        ),  #reminder, inlcude areas ridges as option
      tabPanel(
        title = "Scatter",
        id = ns("scatterTab"),
        scatterPlotUI(ns("scatterPlot"))
      ),
      tabPanel(
        title = "Density",
        id = ns("densityTab"),
        densityPlotUI(ns("densityPlot"))
      ),
      tabPanel(
        title = "Histogram",
        id = ns("histogramTab"),
        histogramPlotUI(ns("histogramPlot"))
      )
    )
}




visualEstimate <- function(input, output, session){
  
  callModule(scatterPlot, "scatterPlot")
  callModule(densityPlot, "densityPlot")
  callModule(histogramPlot, "histogramPlot")
  callModule(intervalsPlot, "intervalsPlot")
  callModule(areasPlot, "areasPlot")
 
  return(reactive({
    list("areas" = NULL,
         "density" = NULL)
    }))
  
}
