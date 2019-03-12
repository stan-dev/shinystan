numericalEstimateUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  
  tabPanel(
    title = "Stats",
    id = ns("numericalEstimate"),
    navlistPanel(
      id = ns("HMC_navlist"),
      tabPanel(
        title = "Posterior Summary Statistics",
        id = ns("summaryStats"),
        summaryTableUI(ns("summaryTable"))
      )
    )
  )
}




numericalEstimate <- function(input, output, session){
  callModule(summaryTable, "summaryTable")
  
}
