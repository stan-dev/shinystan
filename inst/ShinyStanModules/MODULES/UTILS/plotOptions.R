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

plotOptions <- function(input, output, session, ...){
  
  input_names <- names(list(...))
  # print(input_names)
  
  observe({
    toggle("optionsPanel", condition = input$showOptions)
    
  })
  output$optionsPanel <- renderUI({
    tagList(
      
      column(width = 6, 
             splitLayout(align = "left",
                         selectInput(session$ns("theme"), label = "Select Theme", 
                                     choices = c("bayesplot default",
                                                 "classic",
                                                 "dark"),
                                     selected = input$theme),
                         selectInput(session$ns("color"), label = "Select Colors",
                                     choices = c("blue", "brightblue", "gray",
                                                 "darkgray", "green", "pink", "purple",
                                                 "red", "teal", "yellow", "mix-blue-pink", 
                                                 "mix-blue-red"),
                                     selected = input$color)
             )
      ),
      column(width = 6,
             splitLayout(align = "left",
               if("divOptions" %in% input_names == TRUE){
                 selectInput(session$ns("divColor"), label = "Select Divergent Color",
                             choices = c("red", "blue", "gray",
                                         "darkgray", "green", "pink", "purple",
                                         "yellow"),
                             selected = input$divColor)
               },
               if("histOptions" %in% input_names == TRUE){
                 NULL
               }
             ))
      
    )
  })
  
  plotTheme <- reactive({
    out <- list()
    out$theme <- ifelse(!is.null(input$theme), input$theme, "bayesplot default")
    out$color <- ifelse(!is.null(input$color), input$color, "blue")
    out$divColor <- ifelse(!is.null(input$divColor), input$divColor, "red")
    out
  })
  
  return(reactive({plotTheme()}))
  
  # observe(print(plotTheme()))
  
}