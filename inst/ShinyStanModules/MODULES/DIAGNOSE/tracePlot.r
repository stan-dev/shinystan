tracePlotUI <- function(id){
  # for internal namespace structure
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
                   selected = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[1]
                 )
               )
        ),
        column(width = 4
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
    plotOutput(ns("plot1")),
    hr(), 
    checkboxInput(ns("report"), "Include in report?")
  )
}



tracePlot <- function(input, output, session){
    
  visualOptions <- callModule(plotOptions, "options", divOptions = TRUE)
  chain <- reactive(input$diagnostic_chain)
  param <- reactive(input$diagnostic_param)
  include <- reactive(input$report)
  
  output$diagnostic_chain_text <- renderText({
    if (chain() == 0)
      return("All chains")
    paste("Chain", chain())
  })
  
  plotOut <- function(parameters, chain, div_color = "red"){
    validate(
      need(length(parameters) > 0, "Select at least one parameter.")
    )
    if(shinystan:::.sso_env$.SHINYSTAN_OBJECT@misc$stan_method == "sampling"){
      mcmc_trace( if(chain != 0) {
        shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, chain, ]
      } else {
        shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ]
      }, pars = parameters,
      np = if(chain != 0){
        nuts_params(list(shinystan:::.sso_env$.SHINYSTAN_OBJECT@sampler_params[[chain]]) %>%
                      lapply(., as.data.frame) %>%
                      lapply(., filter, row_number() > shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) %>%
                      lapply(., as.matrix))
      } else {
        nuts_params(shinystan:::.sso_env$.SHINYSTAN_OBJECT@sampler_params %>%
                      lapply(., as.data.frame) %>%
                      lapply(., filter, row_number() > shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) %>%
                      lapply(., as.matrix))
      },
      np_style = scatter_style_np(div_color = div_color, div_size = .25)
      )} else {
        mcmc_trace( if(chain != 0) {
          shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, chain, ]
        } else {
          shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ]
        }, pars = parameters,
        np_style = scatter_style_np(div_color = div_color, div_size = .25)) 
      }
    
  }
  
  
  
  output$plot1 <- renderPlot({
    # change plot theme based on selection for this plot, thereafter change back.
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
    out <- plotOut(parameters = param(), chain = chain(), div_color = visualOptions()$divColor)
    bayesplot_theme_set(save_old_theme)
    out
  })
  
  return(reactive({
    if(include() == TRUE){
      # customized plot options return without setting the options for the other plots
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- plotOut(parameters = param(), chain = chain(), div_color = visualOptions()$divColor)
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
  }))
  
}
