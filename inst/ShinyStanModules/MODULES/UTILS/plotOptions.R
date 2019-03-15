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
      
      
        splitLayout(align = "left",
        selectInput(session$ns("theme"), label = "Select Theme", 
                    choices = c("bayesplot default",
                                "classic",
                                "dark"),
                    selected = "bayesplot default"),
        selectInput(session$ns("color"), label = "Select Colors",
                    choices = c("blue", "brightblue", "gray",
                                "darkgray", "green", "pink", "purple",
                                "red", "teal", "yellow", "mix-blue-pink", 
                                "mix-blue-red"))
          
        )
        
    )
  })
  
  plotTheme <- reactive({
    out <- list()
    out$theme <- ifelse(!is.null(input$theme), input$theme, "bayesplot default")
    out$color <- ifelse(!is.null(input$color), input$color, "blue")
    out
  })
  
  return(reactive({plotTheme()}))
  
  # observe(print(plotTheme()))
  
}