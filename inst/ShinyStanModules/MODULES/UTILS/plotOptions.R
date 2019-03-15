plotOptionsUI <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),  # Set up shinyjs
    checkboxInput(ns("showOptions"), "Show/Hide Options"),
    hidden(
      uiOutput(ns("optionsPanel"))
    )
  )
}

plotOptions <- function(input, output, session){
  
  observe({
    toggle("optionsPanel", condition = input$showOptions)
    
  })
  output$optionsPanel <- renderUI({
    tagList(
      
      wellPanel(
        h5("testss"),
        selectInput(session$ns("theme"), label = "Select Theme", 
                    choices = c("bayesplot default",
                                "classic",
                                "dark"),
                    selected = "bayesplot default")
        )
    )
  })
  
  plotTheme <- reactive({
    ifelse(!is.null(input$theme), input$theme, "bayesplot default")
    
  })
  
  return(reactive({plotTheme()}))
  
  # observe(print(plotTheme()))
  
}