autoCorrelationUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 3, h5(textOutput(ns("diagnostic_chain_text")))),
        column(width = 4, h5("Parameter")),
        column(width = 4, h5("Lags"))
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
        ),
        column(
          width = 4, div(style = "width: 100px;",
                         numericInput(
                           ns("diagnostic_lags"),
                           label = NULL,
                           value = 20,
                           min = 1,
                           max = (sso@n_iter - sso@n_warmup - 2),
                           step = 1
                         )
          ))
      )
    ),
    plotOutput(ns("plot1"))
  )
  
}




autoCorrelation <- function(input, output, session){

  chain <- reactive(input$diagnostic_chain)
  param <- reactive(input$diagnostic_param)
  lags <- reactive(input$diagnostic_lags)
  
  output$diagnostic_chain_text <- renderText({
    validate(
      need(is.na(chain()) == FALSE, "Select chains")
    )
    if (chain() == 0)
      return("All chains")
    paste("Chain", chain())
  })
  
  output$plot1 <- renderPlot({
    
    color_scheme_set("blue")
    validate(
      need(length(param()) > 0, "Select at least one parameter."),
      need(is.na(chain()) == FALSE, "Select chains"),
      need(is.null(lags()) == FALSE & is.na(lags()) == FALSE, "Select lags"),
      need(lags() > 0 & lags() < (sso@n_iter - sso@n_warmup - 1), "Number of lags is inappropriate.")
    )
    mcmc_acf_bar( if(chain() != 0) {
      sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, chain(), ]
    } else {
      sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, , ]
    }, pars = param(),
    lags = lags()
    )
  })
  
}
