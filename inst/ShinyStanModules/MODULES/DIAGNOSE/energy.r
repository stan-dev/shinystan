energyUI <- function(id){
  ns <- NS(id)
  
  tagList(
    wellPanel(
      fluidRow(
        column(width = 3, h5(textOutput(ns("diagnostic_chain_text")))), 
        column(width = 4),
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
          width = 4
        )
      )
    ),
    plotOutput(ns("plot1"))
  )
}



energy <- function(input, output, session){
    
  chain <- reactive(input$diagnostic_chain)
  
  output$diagnostic_chain_text <- renderText({
    if (chain() == 0)
      return("All chains")
    paste("Chain", chain())
  })
  
  plotOut <- function(chain){
    
    color_scheme_set("blue")
    mcmc_nuts_energy(
      if(chain != 0) {
        nuts_params(list(sso@sampler_params[[chain]]) %>%
                      lapply(., as.data.frame) %>%
                      lapply(., filter, row_number() > sso@n_warmup) %>%
                      lapply(., as.matrix))
      } else {
        nuts_params(sso@sampler_params %>%
                      lapply(., as.data.frame) %>%
                      lapply(., filter, row_number() > sso@n_warmup) %>%
                      lapply(., as.matrix)) 
        
      }
    )
  }
  
  output$plot1 <- renderPlot({
    plotOut(chain = chain())
  })
  
  return(reactive({
    plotOut(chain = chain())
  }))
  
}