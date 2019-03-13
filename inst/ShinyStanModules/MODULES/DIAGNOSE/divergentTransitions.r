divergentTransitionsUI <- function(id){
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
                           max = ifelse(shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain == 1, 0, shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain)
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


divergentTransitions <- function(input, output, session){
  
  
  
    chain <- reactive(input$diagnostic_chain)
    
    output$diagnostic_chain_text <- renderText({
      if (chain() == 0)
        return("All chains")
      paste("Chain", chain())
    })
    
  
  plotOut <- function(chain){
    color_scheme_set("blue")
    
    if(chain != 0) {
      mcmc_nuts_divergence(
        x = nuts_params(list(shinystan:::.sso_env$.SHINYSTAN_OBJECT@sampler_params[[chain]]) %>%
                          lapply(., as.data.frame) %>%
                          lapply(., filter, row_number() > shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) %>%
                          lapply(., as.matrix)),
        lp = data.frame(Iteration = rep(1:(shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup), 1),
                        Value = c(shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup + 1):shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, chain,"log-posterior"]),
                        Chain = rep(chain, each = (shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup))) 
      )
    } else {
      mcmc_nuts_divergence(
        x = nuts_params(shinystan:::.sso_env$.SHINYSTAN_OBJECT@sampler_params %>%
                          lapply(., as.data.frame) %>%
                          lapply(., filter, row_number() > shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) %>%
                          lapply(., as.matrix)),
        lp = data.frame(Iteration = rep(1:(shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup), shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain),
                        Value = c(shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup + 1):shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, ,"log-posterior"]),
                        Chain = rep(1:shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain, each = (shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup))) 
      )
    }
  }
  
  
  output$plot1 <- renderPlot({
    plotOut(chain = chain())
    
  })
  
  return(reactive({ 
    plotOut(chain = chain())
  }))
  
  
}