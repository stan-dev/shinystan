parallelCoordinatesUI <- function(id){
  ns <- NS(id)
  
  tagList(
    wellPanel(
      fluidRow(
        column(width = 3, h5(textOutput(ns("diagnostic_chain_text")))),
        column(width = 4, h5("Parameter")),
        column(width = 4)
      ),
      fluidRow(
        column(
          width = 3, div(style = "width: 100px;",
                         numericInput(
                           ns("diagnostic_chain"),
                           label = NULL,
                           value = 0,
                           min = 0,
                           # don't allow changing chains if only 1 chain
                           max = ifelse(sso@n_chain == 1, 0, sso@n_chain)
                         )
          )),
        column(
          width = 4,
          selectizeInput(
            inputId = ns("diagnostic_param"),
            label = NULL,
            multiple = TRUE,
            choices = sso@param_names,
            selected = sso@param_names[1:2]
          )
        ),
        column(
          width = 4,
          actionButton(ns("generatePlot"), "Generate Parallel Coordinates Plot",
                       style="color: white; background-color: #222222; height: 30px; width: 200px; padding: 5px; 
                       margin: 3px; border-radius: 15px; font-size: 12px; font-weight: bold;")
          )
      )
    ),
    plotOutput(ns("plot1"))
  )
}


parallelCoordinates <- function(input, output, session){
  
  chain <- reactive(input$diagnostic_chain)
  param <- reactive(input$diagnostic_param)
  
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
  
  plotOut <- function(parameters, chain){
    
      color_scheme_set("darkgray")
      
      if(chain != 0) {
        mcmc_parcoord(
          x = sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, chain, ],
          pars = parameters,
          np = nuts_params(list(sso@sampler_params[[chain]]) %>%
                             lapply(., as.data.frame) %>%
                             lapply(., filter, row_number() > sso@n_warmup) %>%
                             lapply(., as.matrix))
        )
      } else {
        mcmc_parcoord(
          x = sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, , ],
          pars = parameters,
          np = nuts_params(sso@sampler_params %>%
                             lapply(., as.data.frame) %>%
                             lapply(., filter, row_number() > sso@n_warmup) %>%
                             lapply(., as.matrix))
        )
      }
    
  }
  
  
  observeEvent(input$generatePlot, {
    output$plot1 <- renderPlot({
      plotOut(parameters = param_reactive(), chain = chain_reactive())
    })
  })
  
  return(reactive({ 
    plotOut(parameters = param_reactive(), chain = chain_reactive())
  }))
  
}