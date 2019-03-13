autoCorrelationUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 4, 
               verticalLayout(
                 selectizeInput(
                   inputId = ns("diagnostic_param"),
                   label = h5("Parameter"),
                   multiple = TRUE,
                   choices = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names,
                   selected = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[1]
                 )
               )
        ),
        column(width = 4,
               div(style = "width: 100px;",
                   numericInput(
                     ns("diagnostic_lags"),
                     label = h5("Lags"),
                     value = 20,
                     min = 1,
                     max = (shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup - 2),
                     step = 1
                   )
               )
        ),
        column(width = 4, align = "right",
               splitLayout(
                 radioButtons(
                   ns("report"),
                   label = h5("Report"),
                   choices = c("Omit", "Include"),
                   select = "Omit"
                 ),
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
        )
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
      need(lags() > 0 & lags() < (shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup - 1), "Number of lags is inappropriate.")
    )
    mcmc_acf_bar( if(chain() != 0) {
      shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, chain(), ]
    } else {
      shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ]
    }, pars = param(),
    lags = lags()
    )
  })
  
}
