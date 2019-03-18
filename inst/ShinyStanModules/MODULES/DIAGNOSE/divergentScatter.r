divergentScatterUI <- function(id){
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
                 selected = c(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[1],shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[which(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names == "log-posterior")]),
                 options = list(maxItems = 2)
               )
               )
        ),
        column(width = 4,
               tags$head(tags$style(HTML("
                                           .shiny-split-layout > div {
                                           overflow: visible;
                                           }
                                           "))), # overflow of splitlayout didn't work, so this is a fix. 
               splitLayout(
                 div(style = "width: 90%;",
                     selectInput(
                       inputId = ns("transformation"),
                       label = h5("Transform X"),
                       choices = transformation_choices,
                       selected = "identity"
                     )),
                 div(style = "width: 90%;",
                     selectInput(
                       inputId = ns("transformation2"),
                       label = h5("Transform Y") ,
                       choices = transformation_choices,
                       selected = "identity"
                     )
                 )
                 )
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


divergentScatter <- function(input, output, session){
 
  visualOptions <- callModule(plotOptions, "options", divOptions = TRUE)
  
  chain <- reactive(input$diagnostic_chain)
  param <- reactive(input$diagnostic_param)
  include <- reactive(input$report)
  
  transform1 <- reactive({input$transformation})
  transform2 <- reactive({input$transformation2})
  
  transform <- reactive({
    validate(
      need(is.null(transform1()) == FALSE, "")
    )
    out <- list(transform1(), transform2())
    names(out) <- c(param())
    out
  })
  
  
  output$diagnostic_chain_text <- renderText({
    if (chain() == 0)
      return("All chains")
    paste("Chain", chain())
  })
  
  
  plotOut <- function(parameters, chain, transformations, div_color = "red"){
    
    validate(
      need(length(parameters) == 2, "Select two parameters.")
    )
    mcmc_scatter(
      if(chain != 0) {
        shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, chain, ]
      } else {
        shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ]
      },
      pars = parameters,
      transformations = transformations,
      np = if(chain != 0) {
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
      np_style = scatter_style_np(div_color = div_color, div_alpha = 0.8)
    ) #+ labs(#title = "",
      #subtitle = "Generated via ShinyStan",
      # caption = paste0("Scatter plot of ", parameters[1]," and ", parameters[2],
      #                 " with highlighted divergent transitions."))
    
    
  }
  
  output$plot1 <- renderPlot({
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
    out <- plotOut(parameters = param(), chain = chain(),
                   transformations = transform(), div_color = visualOptions()$divColor)
    bayesplot_theme_set(save_old_theme)
    out
  })
  
  
  observe({
    toggle("caption", condition = input$showCaption)
  })
  
  output$caption <- renderUI({
    HTML(paste0("This is a plot of MCMC draws of <i>", param()[1], "</i> (x-axis) against <i>",
                param()[2], "</i> (y-axis). The ", visualOptions()$divColor, 
                " colored draws represent, if present, divergent transitions.",
                " Divergent transitions can indicate problems for the validity of the results.",
                " A good plot would show no divergent transitions. A bad plot would show ",
                "divergent transitions in a systematic patern. ",
                "For more information see ",
                tags$a('https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup'), "."))
  })
  
  
  
  
  return(reactive({
    if(include() == TRUE){
      # customized plot options return without setting the options for the other plots
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- plotOut(parameters = param(), chain = chain(),
                     transformations = transform(), div_color = visualOptions()$divColor)
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
  }))
  
}