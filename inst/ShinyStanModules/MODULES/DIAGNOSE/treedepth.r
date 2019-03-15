treedepthUI <- function(id){
  ns <- NS(id)
  
  tagList(
    wellPanel(
      fluidRow(
        column(width = 4), 
        column(width = 4),
        column(width = 4, align = "right",
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



treedepth <- function(input, output, session){
  
  visualOptions <- callModule(plotOptions, "options")
  chain <- reactive(input$diagnostic_chain)
  include <- reactive(input$report)
  
  output$diagnostic_chain_text <- renderText({
    if (chain() == 0)
      return("All chains")
    paste("Chain", chain())
  })
  
  plotOut <- function(chain) {
    
    # color_scheme_set("blue")
    
    if(chain != 0) {
      mcmc_nuts_treedepth(
        x = nuts_params(list(shinystan:::.sso_env$.SHINYSTAN_OBJECT@sampler_params[[chain]]) %>%
                          lapply(., as.data.frame) %>%
                          lapply(., filter, row_number() > shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) %>%
                          lapply(., as.matrix)),
        lp = data.frame(Iteration = rep(1:(shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup), 1),
                        Value = c(shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup + 1):shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, chain,"log-posterior"]),
                        Chain = rep(chain, each = (shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup))) 
      ) #+ theme_dark()#eval(parse(text = switch(1, "theme_default()", "theme_classic()")))
    } else {
      mcmc_nuts_treedepth(
        x = nuts_params(shinystan:::.sso_env$.SHINYSTAN_OBJECT@sampler_params %>%
                          lapply(., as.data.frame) %>%
                          lapply(., filter, row_number() > shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) %>%
                          lapply(., as.matrix)),
        lp = data.frame(Iteration = rep(1:(shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup), shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain),
                        Value = c(shinystan:::.sso_env$.SHINYSTAN_OBJECT@posterior_sample[(shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup + 1):shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter, ,"log-posterior"]),
                        Chain = rep(1:shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain, each = (shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup))) 
      ) #+ theme_dark() #eval(parse(text = switch(1, "theme_default()", "theme_classic()")))
    }
    
  }  
  
  output$plot1 <- renderPlot({
    # change plot theme based on selection for this plot, thereafter change back.
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions()$color)
    bayesplot_theme_set(eval(parse(text = 
                                     switch(visualOptions()$theme,
                                            "bayesplot default" = "theme_default()", 
                                            "classic" = "theme_classic()",
                                            "dark" = "theme_dark()"))))
    out <- plotOut(chain = chain()) 
    bayesplot_theme_set(save_old_theme)
    out
  })
  
  return(reactive({
    if(include() == TRUE){
      # customized plot options return without setting the options for the other plots
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = 
                                       switch(visualOptions()$theme,
                                              "bayesplot default" = "theme_default()", 
                                              "classic" = "theme_classic()",
                                              "dark" = "theme_dark()"))))
      out <- plotOut(chain = chain()) 
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
  }))
}