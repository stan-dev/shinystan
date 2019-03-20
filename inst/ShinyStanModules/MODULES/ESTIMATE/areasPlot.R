areasPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 6,
               selectizeInput(
                 inputId = ns("diagnostic_param"),
                 label = h5("Parameter"),
                 multiple = TRUE,
                 choices = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names,
                 selected = if(length(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names) > 9) shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[1:10] else shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names
               )
        ), 
        column(width = 4),
        column(width = 2, align = "right")
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

areasPlot <- function(input, output, session){
  
  visualOptions <- callModule(plotOptions, "options", estimatePlots = TRUE,
                              intervalOptions = TRUE, areasOptions = TRUE)
  
  param <- reactive(input$diagnostic_param)
  include <- reactive(input$report)
  
  plotOut <- function(parameters, plotType){
    
    validate(
      need(length(parameters) > 0, "Select at least one parameter.")
    )
    if(plotType == "Areas"){
      out <- mcmc_areas(
        shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ],
        pars = parameters,
        point_est = tolower(visualOptions()$point_est),
        prob = visualOptions()$inner_ci / 100,
        prob_outer = visualOptions()$outer_ci / 100
      )
    }
    if(plotType == "Ridges"){
      out <-   mcmc_areas_ridges(
        shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ],
        pars = parameters,
        prob = visualOptions()$inner_ci / 100,
        prob_outer = visualOptions()$outer_ci / 100
      )
    }
    out
  }
  
  output$plot1 <- renderPlot({
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
    out <- plotOut(parameters = param(), plotType = visualOptions()$areas_ridges)
    bayesplot_theme_set(save_old_theme)
    out
  })
  
  
  return(reactive({
    if(include() == TRUE){
      # customized plot options return without setting the options for the other plots
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- plotOut(parameters = param(), plotType = visualOptions()$areas_ridges)
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
  }))
  
  
}