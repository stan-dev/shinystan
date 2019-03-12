estimateUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  
  # encapsulate everything in taglist, see https://shiny.rstudio.com/articles/modules.html
  tagList(
    uiOutput(ns("estimateHomepage"))
  )
  
}

estimate <- function(input, output, session){
  
  callModule(visualEstimate, "visualEstimate")
  callModule(numericalEstimate, "numericalEstimate")
  
  output$estimateHomepage <- renderUI({
    tagList(
      tabsetPanel(
        id = session$ns("diagnose_tabset"),
        visualEstimateUI(session$ns("visualEstimate")),
        numericalEstimateUI(session$ns("numericalEstimate"))
      )
    )
  })
}
