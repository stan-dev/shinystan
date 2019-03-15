numericalEstimateUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  
    navlistPanel(
      id = ns("HMC_navlist"),
      tabPanel(
        title = "Posterior Summary Statistics",
        id = ns("summaryStats"),
        summaryTableUI(ns("summaryTable"))
      )
    )
  
}




numericalEstimate <- function(input, output, session){
  callModule(summaryTable, "summaryTable")
  
}
