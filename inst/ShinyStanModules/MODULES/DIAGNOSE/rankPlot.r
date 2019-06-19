rankPlotUI <- function(id){
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
        column(width = 4,
               radioButtons(ns("selectStat"), h5("Type"), 
                                choices = c("Histogram",
                                            "Overlay"),
                                inline = FALSE)
               ),
        column(width = 2
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



rankPlot <- function(input, output, session){
    
  visualOptions <- callModule(plotOptions, "options")
  param <- reactive(unique(.update_params_with_groups(params = input$diagnostic_param,
                                                      all_param_names = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names)))
  include <- reactive(input$report)
  
  observe({
    toggle("caption", condition = input$showCaption)
  })
  
 
  
  plotOut <- function(parameters){
    validate(
      need(length(parameters) > 0, "Select at least one parameter.")
    )
    if(input$selectStat == "Histogram"){
      mcmc_rank_hist( 
        shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ], 
        pars = parameters) 
    } else {
      mcmc_rank_overlay( 
        shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ], 
        pars = parameters) 
    }
  }  

  
  output$plot1 <- renderPlot({
    # change plot theme based on selection for this plot, thereafter change back.
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
    out <- plotOut(parameters = param())
    bayesplot_theme_set(save_old_theme)
    out
  })
  
  captionOut <- function(parameters){
    HTML(paste0(if(length(parameters) == 1) {"This is a rank plot of <i>"} else {"These are rank plots of <i>"}, 
                paste(parameters[1:(length(parameters)-1)], collapse = ", "),
                if(length(parameters) > 1) {"</i> and <i>"}, 
                if(length(parameters) > 1) {parameters[length(parameters)]},"</i>", ".",
                " Rank histograms visualize how the values from",
                " the chains mix together in terms of ranking.",
                " An ideal plot would show the rankings mixing",
                " or overlapping in a uniform distribution.",
                " See Vehtari et al. (2019) for details."))
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
      out <- list(plot = plotOut(parameters = param()), 
                  caption = captionOut(parameters = param()))
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
  }))
  
}
