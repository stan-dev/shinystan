areasPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 6,
               selectizeInput(
                 inputId = ns("diagnostic_param"),
                 label = h5("Parameter"),
                 multiple = TRUE,
                 choices = .make_param_list_with_groups(shinystan:::.sso_env$.SHINYSTAN_OBJECT),
                 selected = if(length(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names) > 9) shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[1:10] else shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names
               )
        ), 
        column(width = 4),
        column(width = 2, align = "right")
      ),
      fluidRow(
        align = "right",
        plotOptionsUI(ns("options"))
      )
    ),
    plotOutput(ns("plot1")),
    checkboxInput(ns("showCaption"), "Show Caption", value = TRUE),
    hidden(
      uiOutput(ns("caption"))
    ),
    hr(), 
    # checkboxInput(ns("report"), "Include in report?")
    downloadButton(ns('downloadPlot'), 'Download Plot', class = "downloadReport"),
    downloadButton(ns('downloadRDS'), 'Download RDS', class = "downloadReport")
  )
}

areasPlot <- function(input, output, session){
  
  visualOptions <- callModule(plotOptions, "options", estimatePlots = TRUE,
                              intervalOptions = TRUE, areasOptions = TRUE)
  
  param <- debounce(reactive(unique(.update_params_with_groups(params = input$diagnostic_param,
                                                               all_param_names = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names))),
                    500)
  
  include <- reactive(input$report)
  
  observe({
    toggle("caption", condition = input$showCaption)
  })
  
  plotOut <- function(parameters, plotType){
    
    validate(
      need(length(parameters) > 0, "Select at least one parameter.")
    )
    if(plotType == "Areas"){
      out <- mcmc_areas(
        shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ],
        pars = parameters,
        point_est = tolower(visualOptions()$point_est),
        prob = visualOptions()$inner_ci / 100,
        prob_outer = visualOptions()$outer_ci / 100,
        area_method = visualOptions()$areas_type
      )
    }
    if(plotType == "Ridges"){
      out <-   mcmc_areas_ridges(
        shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ],
        pars = parameters,
        prob = visualOptions()$inner_ci / 100,
        prob_outer = visualOptions()$outer_ci / 100
      )
    }
    out
  }
  
  output$plot1 <- renderPlot({
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
    out <- plotOut(parameters = param(), plotType = visualOptions()$areas_ridges)
    bayesplot_theme_set(save_old_theme)
    out
  })
  
  captionOut <- function(parameters){
    # HTML(paste0(if(length(parameters) == 1) {"This is an area plot of <i>"} else {"These are area plots of <i>"}, 
    #             paste(parameters[1:(length(parameters)-1)], collapse = ", "),
    #             if(length(parameters) > 1) {"</i> and <i>"}, 
    #             if(length(parameters) > 1) {parameters[length(parameters)]},"</i>", ".",
    #             " The outer edges denote the ", visualOptions()$outer_ci, "% credibility interval.", 
    #             " The inner edges denote the ", visualOptions()$inner_ci, "% credibility interval.", 
    #             if(visualOptions()$point_est != "None") {paste0(" The point estimate denotes the posterior ",
    #                                                             tolower(visualOptions()$point_est), ".")}
    # ))
    HTML(paste0("This is an area plot. The outer edges denote the ", 
                visualOptions()$outer_ci, "% posterior uncertainty interval (credible interval).",
                " The inner edges denote the ", visualOptions()$inner_ci, "% interval.", 
                if(visualOptions()$point_est != "None") {paste0(" The point estimate is the posterior ",
                                                                tolower(visualOptions()$point_est), ".")}))
  }
  
  output$caption <- renderUI({
    captionOut(parameters = param())
  })
  
  output$downloadPlot <- downloadHandler(
    filename = 'areasPlot.pdf',
    content = function(file) {
      # ggsave(file, gridExtra::arrangeGrob(grobs = downloadSelection()))
      pdf(file)
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- plotOut(parameters = param(), plotType = visualOptions()$areas_ridges)
      bayesplot_theme_set(save_old_theme)
      print(out)
      dev.off()
    })
  
  
  output$downloadRDS <- downloadHandler(
    filename = 'areasPlot.rds',
    content = function(file) {
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- plotOut(parameters = param(), plotType = visualOptions()$areas_ridges)
      bayesplot_theme_set(save_old_theme)
      saveRDS(out, file)
    })  
  
  return(reactive({
    if(include() == TRUE){
      # customized plot options return without setting the options for the other plots
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- list(plot = plotOut(parameters = param(), 
                                 plotType = visualOptions()$areas_ridges),
                  caption = captionOut(parameters = param()))
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
  }))
  
  
}