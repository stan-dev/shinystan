densityPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 6, 
               verticalLayout(
                 selectizeInput(
                   inputId = ns("diagnostic_param"),
                   label = h5("Parameters"),
                   multiple = TRUE,
                   choices = .make_param_list_with_groups(shinystan:::.sso_env$.SHINYSTAN_OBJECT),
                   selected = c(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[1])
                 )
               )
        ),
        column(width = 4),
        column(width = 2, align = "right"
        )
               ),
      fluidRow(
        align = "right",
        plotOptionsUI(ns("options"))
      )
      ),
    plotOutput(ns("plot1")),
    checkboxInput(ns("showCaption"), "Show/Hide Caption"),
    hidden(
      uiOutput(ns("caption"))
    ),
    hr(), 
    checkboxInput(ns("report"), "Include in report?")
  )
}



densityPlot <- function(input, output, session){
  
  visualOptions <- callModule(plotOptions, "options")
  
  param <- reactive(unique(.update_params_with_groups(params = input$diagnostic_param,
                                                      all_param_names = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names)))
  
  include <- reactive(input$report)
  
  observe({
    toggle("caption", condition = input$showCaption)
  })
  
  plotOut <- function(parameters, chain){
    
    validate(
      need(length(param()) > 0, "Select at least one parameter.")
    )
    mcmc_dens(
      shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ],
      pars = parameters
    )
  }
  
  output$plot1 <- renderPlot({
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
    out <- plotOut(parameters = param())
    bayesplot_theme_set(save_old_theme)
    out
  })
  
  captionOut <- function(){
    HTML(paste0("Density plots..",
                " ",
                " ",
                " ",
                " ",
                " ",
                " "))
  }
  output$caption <- renderUI({
    captionOut()
  })
  
  return(reactive({
    if(include() == TRUE){
      # customized plot options return without setting the options for the other plots
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- list(plot = plotOut(parameters = param()),
                  caption = captionOut())
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
  }))
  
}