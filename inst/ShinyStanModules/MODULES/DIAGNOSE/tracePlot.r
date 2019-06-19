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
                   choices = .make_param_list_with_groups(shinystan:::.sso_env$.SHINYSTAN_OBJECT),
                   selected = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[order(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "n_eff"])[1:2]]
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
    checkboxInput(ns("showCaption"), "Show/Hide Caption"),
    hidden(
      uiOutput(ns("caption"))
    ),
    hr(), 
    checkboxInput(ns("report"), "Include in report?")
  )
}



tracePlot <- function(input, output, session){
    
  visualOptions <- callModule(plotOptions, "options", divOptions = TRUE)
  chain <- reactive(input$diagnostic_chain)
  param <- reactive(unique(.update_params_with_groups(params = input$diagnostic_param,
                                                      all_param_names = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names)))
  include <- reactive(input$report)
  
  observe({
    toggle("caption", condition = input$showCaption)
  })
  
  
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
  
  captionOut <- function(parameters){
    HTML(paste0(if(length(parameters) == 1 & chain() != 0) {"This is an autocorrelation plot of <i>"} else {"These are autocorrelation plots of <i>"}, 
                paste(parameters[1:(length(parameters)-1)], collapse = ", "),
                if(length(parameters) > 1) {"</i> and <i>"}, 
                if(length(parameters) > 1) {parameters[length(parameters)]},"</i>",
                " for ", tolower(if (chain() == 0) {"All chains"} else {paste("Chain", chain())}), ".",
                " ",
                " ",
                " ",
                " ",
                " ",
                " "))
  }
  output$caption <- renderUI({
    captionOut(parameters = param())
  })
  
  return(reactive({
    if(include() == TRUE){
      # customized plot options return without setting the options for the other plots
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- list(plot = plotOut(parameters = param(), chain = chain(), div_color = visualOptions()$divColor), 
                  caption = captionOut(parameters = param()))
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
  }))
  
}
