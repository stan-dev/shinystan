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
    # to fix overflow problem with selectinputs in verticalLayout.
    tagList(tags$head(tags$style(HTML("
    .shiny-split-layout > div {
      overflow: visible;
    }
  "))),
      column(width = 6, 
             if("estimatePlots" %in% input_names == TRUE){
               splitLayout(align = "left",
                           div(style = "width: 90%;",
                           verticalLayout(
                             selectInput(session$ns("theme"), label = h5("Select Theme"), 
                                         choices = c("bayesplot default",
                                                     "classic",
                                                     "dark"),
                                         selected = input$theme),
                             if("intervalOptions" %in% input_names == TRUE) {
                               sliderInput(
                                 inputId = session$ns("param_plot_ci_level"),
                                 label = h5("Posterior Interval (inner)"),
                                 width = "90%",
                                 ticks = FALSE,
                                 min = 50,
                                 max = outer_ci(),
                                 value = inner_ci(),
                                 step = 5,
                                 post = "%"
                               )
                             }
                           )),
                           div(style = "width: 90%;",
                           verticalLayout(
                             selectInput(session$ns("color"), label = h5("Select Colors"),
                                         choices = c("blue", "brightblue", "gray",
                                                     "darkgray", "green", "pink", "purple",
                                                     "red", "teal", "yellow", "mix-blue-pink", 
                                                     "mix-blue-red"),
                                         selected = input$color),
                             if("intervalOptions" %in% input_names == TRUE) {
                                 sliderInput(
                                   inputId = session$ns("param_plot_ci_level_outer"),
                                   label = h5("Posterior Interval (outer)"),
                                   width = "90%",
                                   ticks = FALSE,
                                   min = inner_ci(),
                                   max = 95,
                                   value = outer_ci(),
                                   step = 5,
                                   post = "%"
                                 )
                             })
               ))
             } else {
               splitLayout(align = "left",
                           selectInput(session$ns("theme"), label = h5("Select Theme"), 
                                       choices = c("bayesplot default",
                                                   "classic",
                                                   "dark"),
                                       selected = input$theme),
                           verticalLayout(
                           selectInput(session$ns("color"), label = h5("Select Colors"),
                                       choices = c("blue", "brightblue", "gray",
                                                   "darkgray", "green", "pink", "purple",
                                                   "red", "teal", "yellow", "mix-blue-pink", 
                                                   "mix-blue-red"),
                                       selected = input$color),
                           if("alphaOptions" %in% input_names == TRUE){
                             div(style = "width: 90%;",
                                 sliderInput(
                                   inputId = session$ns("alpha"),
                                   label = h5("Opacity"),
                                   width = "90%",
                                   ticks = FALSE,
                                   min = 0,
                                   max = 1,
                                   value = opacity(),
                                   step = 0.1
                                 ))
                           } 
                           )
               )
             }
      ),
      column(width = 6,
             splitLayout(align = "left",
                         div(style = "width: 90%;",
                         verticalLayout(
                         if("divOptions" %in% input_names == TRUE){
                           selectInput(session$ns("divColor"), label = h5("Select Divergent Color"),
                                       choices = c("red", "blue", "gray",
                                                   "darkgray", "green", "pink", "purple",
                                                   "yellow"),
                                       selected = input$divColor)
                         },
                         if("alphaDivOptions" %in% input_names == TRUE) {
                           div(style = "width: 90%;",
                               sliderInput(
                                 inputId = session$ns("alphaDiv"),
                                 label = h5("Opacity Divergences"),
                                 width = "90%",
                                 ticks = FALSE,
                                 min = 0.1,
                                 max = 1,
                                 value = opacityDiv(),
                                 step = 0.1
                               ))
                         },
                         if("intervalOptions" %in% input_names == TRUE){
                           radioButtons(
                             inputId = session$ns("param_plot_point_est"),
                             label = h5("Point Estimate"),
                             choices = c("Median", "Mean", "None"),
                             selected = input$param_plot_point_est,
                             inline = TRUE
                           )
                         }
                         )),
                         if("areasOptions" %in% input_names == TRUE){
                             radioButtons(
                               inputId = session$ns("areas_ridges"),
                               label = h5("Plot Type"),
                               choices = c("Areas", "Ridges"),
                               selected = input$areas_ridges,
                               inline = TRUE
                             )
                           }
                         ))
      
    )
  })
  
  inner_ci <- reactive(input$param_plot_ci_level)
  outer_ci <- reactive({
    if(is.null(input$param_plot_ci_level_outer)) 95 else input$param_plot_ci_level_outer
  })
  opacity <- reactive({
    if(is.null(input$alpha)) 0.8 else input$alpha
  })
  opacityDiv <- reactive({
    if(is.null(input$alphaDiv)) 0.8 else input$alphaDiv
  })
  
  
  
  plotTheme <- reactive({
    out <- list()
    out$theme <- ifelse(!is.null(input$theme), input$theme, "bayesplot default")
    out$color <- ifelse(!is.null(input$color), input$color, "blue")
    out$divColor <- ifelse(!is.null(input$divColor), input$divColor, "red")
    out$inner_ci <- ifelse(!is.null(input$param_plot_ci_level), input$param_plot_ci_level, 50)
    out$outer_ci <- ifelse(!is.null(input$param_plot_ci_level_outer), input$param_plot_ci_level_outer, 95)
    out$point_est <- ifelse(!is.null(input$param_plot_point_est), input$param_plot_point_est, "Median")
    out$areas_ridges <- ifelse(!is.null(input$areas_ridges), input$areas_ridges, "Areas")
    out$alpha <- ifelse(!is.null(input$alpha), input$alpha, 0.8)
    out$alphaDiv <- ifelse(!is.null(input$alphaDiv), input$alphaDiv, 0.8)
    out
  })
  
  return(reactive({plotTheme()}))
  
  # observe(print(plotTheme()))
  
}