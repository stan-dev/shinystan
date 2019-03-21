scatterPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 6, 
               verticalLayout(
                 selectizeInput(
                   inputId = ns("diagnostic_param"),
                   label = h5("Parameter"),
                   multiple = TRUE,
                   choices = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names,
                   selected = c(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[1],shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[which(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names == "log-posterior")]),
                   options = list(maxItems = 2)
                 )
               )
        ),
        column(width = 4,
               tags$head(tags$style(HTML("
                                         .shiny-split-layout > div {
                                         overflow: visible;
                                         }
                                         "))), # overflow of splitlayout didn't work, so this is a fix. 
               splitLayout(
                 div(style = "width: 90%;",
                     selectInput(
                       inputId = ns("transformation"),
                       label = h5("Transform X"),
                       choices = transformation_choices,
                       selected = "identity"
                     )),
                 div(style = "width: 90%;",
                     selectInput(
                       inputId = ns("transformation2"),
                       label = h5("Transform Y") ,
                       choices = transformation_choices,
                       selected = "identity"
                     )
                 )
               )
        ),
        column(width = 2, align = "right"
        )
      ),
      fluidRow(
        align = "right",
        plotOptionsUI(ns("options"))
      )
    ),
    plotOutput(ns("plot1")),
    hr(), 
    checkboxInput(ns("report"), "Include in report?")
  )
}


scatterPlot <- function(input, output, session){
  
  
  visualOptions <- callModule(plotOptions, "options")
  
  param <- reactive(input$diagnostic_param)
  include <- reactive(input$report)
  
  transform1 <- reactive({input$transformation})
  transform2 <- reactive({input$transformation2})
  
  transform <- reactive({
    validate(
      need(is.null(transform1()) == FALSE, "")
    )
    out <- list(transform1(), transform2())
    names(out) <- c(param())
    out
  })
  
  plotOut <- function(parameters, chain, transformations){
    
    validate(
      need(length(parameters) == 2, "Select two parameters.")
    )
    mcmc_scatter(
      shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ],
      pars = parameters,
      transformations = transformations
    ) 
  }
  
  output$plot1 <- renderPlot({
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
    out <- plotOut(parameters = param(),
                   transformations = transform())
    bayesplot_theme_set(save_old_theme)
    out
  })
  
  return(reactive({
    if(include() == TRUE){
      # customized plot options return without setting the options for the other plots
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- plotOut(parameters = param(), 
                     transformations = transform())
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
  }))
  
}