parallelCoordinatesUI <- function(id){
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
                   selected = if(length(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names) > 3) shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[1:4] else shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names
                 )
               )
        ),
        column(width = 4,
               br(),br(),
               actionButton(ns("generatePlot"), "Generate Parcoord Plot", class = "generatePlot")
        ),
        column(width = 2, align = "right",
                 div(style = "width: 100px;",
                     numericInput(
                       ns("diagnostic_chain"),
                       label = h5(textOutput(ns("diagnostic_chain_text"))),
                       value = 0,
                       min = 0,
                       # don't allow changing chains if only 1 chain
                       max = ifelse(shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain == 1, 0, shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain)
                     )
                 )
        )
      ),
      fluidRow(
        align = "right",
        plotOptionsUI(ns("options"))
      )
    ),
    uiOutput(ns("plotUI"))
  )
}


parallelCoordinates <- function(input, output, session){
  
  visualOptions <- callModule(plotOptions, "options", divOptions = TRUE)
  chain <- reactive(input$diagnostic_chain)
  param <- reactive(input$diagnostic_param)
  
  # structure needed to get FALSE for report inclusion if variable is logical(0)
  include <- reactive(input$report)
  include_report <- reactive({
    ifelse(!is.null(include()), include(), FALSE)
  })
  
  visualOptions_reactive <- eventReactive(input$generatePlot, {
    visualOptions()
  })
  
  param_reactive <- eventReactive(input$generatePlot, {
    param()
  })
  
  chain_reactive <- eventReactive(input$generatePlot, {
    chain()
  })
  
  
  output$diagnostic_chain_text <- renderText({
    if (chain() == 0)
      return("All chains")
    paste("Chain", chain())
  })
  
  plotOut <- function(parameters, chain, div_color = "red"){
    
    validate(
      need(length(parameters) > 1, "Select at least two parameters.")
    )
    
      if(chain != 0) {
        mcmc_parcoord(
          x = shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, chain, ],
          pars = parameters,
          np = nuts_params(list(shinystan:::.sso_env$.SHINYSTAN_OBJECT@sampler_params[[chain]]) %>%
                             lapply(., as.data.frame) %>%
                             lapply(., filter, row_number() > shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) %>%
                             lapply(., as.matrix)),
          np_style = scatter_style_np(div_color = div_color, div_size = .25)
        )
      } else {
        mcmc_parcoord(
          x = shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ],
          pars = parameters,
          np = nuts_params(shinystan:::.sso_env$.SHINYSTAN_OBJECT@sampler_params %>%
                             lapply(., as.data.frame) %>%
                             lapply(., filter, row_number() > shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) %>%
                             lapply(., as.matrix)),
          np_style = scatter_style_np(div_color = div_color, div_size = .25)
        )
      }
    
  }
  
  observeEvent(input$generatePlot, {
    output$plot1 <- renderPlot({
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions_reactive()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_reactive()$theme)))) 
      out <- plotOut(parameters = param_reactive(), chain = chain_reactive(), 
                     div_color = visualOptions_reactive()$divColor)
      bayesplot_theme_set(save_old_theme)
      out
    })
  })
  
  observeEvent(input$generatePlot, {
    output$plotUI <-   renderUI({
      tagList(
        plotOutput(session$ns("plot1")),
        hr(), 
        checkboxInput(session$ns("report"), "Include in report?", value = include())
      )
    })
  })
  
  return(reactive({
    if(include_report() == TRUE){
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions_reactive()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions_reactive()$theme)))) 
      out <- plotOut(parameters = param_reactive(), chain = chain_reactive(), 
                     div_color = visualOptions_reactive()$divColor)
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
    
  }))
  
}