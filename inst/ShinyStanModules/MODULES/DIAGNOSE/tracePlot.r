tracePlotUI <- function(id){
  # for internal namespace structure
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
            selected = sso@param_names[1]
          )
        )
      )
    ),
    plotOutput(ns("plot1"))
  )
  
}




tracePlot <- function(input, output, session){
    
  chain <- reactive(input$diagnostic_chain)
  param <- reactive(input$diagnostic_param)
  
  output$diagnostic_chain_text <- renderText({
    if (chain() == 0)
      return("All chains")
    paste("Chain", chain())
  })
  
  plotOut <- function(parameters, chain) {
    color_scheme_set("mix-blue-pink")
    validate(
      need(length(parameters) > 0, "Select at least one parameter.")
    )
    if(sso@misc$stan_method == "sampling"){
      mcmc_trace( if(chain != 0) {
        sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, chain, ]
      } else {
        sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, , ]
      }, pars = parameters,
      np = if(chain != 0){
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
      )} else {
        mcmc_trace( if(chain != 0) {
          sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, chain, ]
        } else {
          sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, , ]
        }, pars = parameters) 
      }
    
  }
  
  output$plot1 <- renderPlot({
    plotOut(parameters = param(), chain = chain())
  })
  
  return(reactive({plotOut(parameters = param(), chain = chain())}))
  
}
