autoCorrelationUI <- function(id){
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
               splitLayout(
                 div(style = "width: 90%;",
                     radioButtons(inputId = ns("autoLags"),
                                  label = h5("Choose Lags"),
                                  choices = c("No", "Yes"),
                                  selected = "No"
                     )
                 ),
                 div(style = "width: 90%;",
                     numericInput(
                       inputId = ns("diagnostic_lags"),
                       label = h5("Lags"),
                       value = 20, 
                       min = 1,
                       max = (shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup - 2),
                       step = 1
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


autoCorrelation <- function(input, output, session){
  
  visualOptions <- callModule(plotOptions, "options")  
  chain <- reactive(input$diagnostic_chain)
  param <- debounce(reactive(unique(.update_params_with_groups(params = input$diagnostic_param,
                                                               all_param_names = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names))),
                    500)
  observeEvent(input$autoLags, {
    shinyjs::toggleState("diagnostic_lags")
  })
  # Geyer truncation rule, see: https://discourse.mc-stan.org/t/shinystan-3-0-alpha-test/12664/9
  lags <- reactive({
    if(input$autoLags == "No"){
      geyer_truncation <- shinystan:::.max_t_sso(shinystan:::.sso_env$.SHINYSTAN_OBJECT, param(), chains = chain())
      ifelse(geyer_truncation > 20, 
             yes = 20,
             no = geyer_truncation)
      
    } else {
      input$diagnostic_lags
    }
  })
  
  include <- reactive(input$report)
  
  observe({
    toggle("caption", condition = input$showCaption)
  })
  
  output$diagnostic_chain_text <- renderText({
    validate(
      need(is.na(chain()) == FALSE, "Select chains")
    )
    if (chain() == 0)
      return("All chains")
    paste("Chain", chain())
  })
  
  # create function to make the plot and call in renderplot and return
  # needed to return plots from module so that we can use them in report.
  plotOut <- function(chain, lags, parameters) {
    
    validate(
      need(length(parameters) > 0, "Select at least one parameter."),
      need(is.na(chain) == FALSE, "Select chains"),
      need(is.null(lags) == FALSE & is.na(lags) == FALSE, "Select lags"),
      need(lags > 0 & lags < (shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup - 1), "Number of lags is inappropriate.")
    )
    mcmc_acf_bar( if(chain != 0) {
      shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, chain, ]
    } else {
      shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(1 + shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) : shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, , ]
    }, pars = parameters,
    lags = lags
    )
  }
  
  
  output$plot1 <- renderPlot({
    # change plot theme based on selection for this plot, thereafter change back.
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
    out <- plotOut(chain = chain(), lags = lags(), parameters = param())
    bayesplot_theme_set(save_old_theme)
    out
  })
  
  captionOut <- function(parameters){
    HTML(paste0(if(length(parameters) == 1 & chain() != 0) {"This is an autocorrelation plot of <i>"} else {"These are autocorrelation plots of <i>"}, 
                paste(parameters[1:(length(parameters)-1)], collapse = ", "),
                if(length(parameters) > 1) {"</i> and <i>"}, 
                if(length(parameters) > 1) {parameters[length(parameters)]},"</i>",
                " for ", tolower(if (chain() == 0) {"All chains"} else {paste("Chain", chain())}), ".",
                " The autocorrelation expresses the dependence between the samples of a Monte Carlo simulation.",
                " With higher dependence between the draws, more samples are needed to obtain the same effective sample size.",
                " High autocorrelation can sometimes be remedied by reparametrization of the model."
    ))
  }
  output$caption <- renderUI({
    captionOut(parameters = param())
  })
  
  
  output$downloadPlot <- downloadHandler(
    filename = 'autoCorrelationPlot.pdf',
    content = function(file) {
      # ggsave(file, gridExtra::arrangeGrob(grobs = downloadSelection()))
      pdf(file)
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- plotOut(chain = chain(), lags = lags(), parameters = param())
      bayesplot_theme_set(save_old_theme)
      print(out)
      dev.off()
    })
  
  
  output$downloadRDS <- downloadHandler(
    filename = 'autoCorrelationPlot.rds',
    content = function(file) {
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- plotOut(chain = chain(), lags = lags(), parameters = param())
      bayesplot_theme_set(save_old_theme)
      saveRDS(out, file)
    })  
  
  
  return(reactive({
    if(include() == TRUE){
      # customized plot options return without setting the options for the other plots
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- list(plot = plotOut(chain = chain(), lags = lags(), parameters = param()), 
                  caption = captionOut(parameters = param()))
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
  }))
  
}
