energyUI <- function(id){
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
    checkboxInput(ns("showCaption"), "Show Caption", value = TRUE),
    hidden(
      uiOutput(ns("caption"))
    ),
    hr(), 
    checkboxInput(ns("report"), "Include in report?")
  )
}




energy <- function(input, output, session){
  
  visualOptions <- callModule(plotOptions, "options")  
  chain <- reactive(input$diagnostic_chain)
  include <- reactive(input$report)
  
  observe({
    toggle("caption", condition = input$showCaption)
  })
  
  output$diagnostic_chain_text <- renderText({
    if (chain() == 0)
      return("All chains")
    paste("Chain", chain())
  })
  
  plotOut <- function(chain){
    
    mcmc_nuts_energy(
      if(chain != 0) {
        nuts_params(list(shinystan:::.sso_env$.SHINYSTAN_OBJECT@sampler_params[[chain]]) %>%
                      lapply(., as.data.frame) %>%
                      lapply(., filter, row_number() > shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) %>%
                      lapply(., as.matrix))
      } else {
        nuts_params(shinystan:::.sso_env$.SHINYSTAN_OBJECT@sampler_params %>%
                      lapply(., as.data.frame) %>%
                      lapply(., filter, row_number() > shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) %>%
                      lapply(., as.matrix)) 
        
      }
    )
  }
  
  output$plot1 <- renderPlot({
    # change plot theme based on selection for this plot, thereafter change back.
    save_old_theme <- bayesplot_theme_get()
    color_scheme_set(visualOptions()$color)
    bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
    out <- plotOut(chain = chain()) 
    bayesplot_theme_set(save_old_theme)
    suppressMessages(print(out)) # hide 'bins = 30' message ggplot
  })
  
  captionOut <- function(){
    
    HTML(paste0("These are plots of the overlaid histograms of the marginal energy distribution (",
                " \\(\\pi_E\\)", ")",
                " and the energy transition distribution (", 
                " \\(\\pi_{\\Delta E}\\)", 
                ") for ", 
                tolower(if (chain() == 0) {"All chains"} else {paste("Chain", chain())}), ".",
                " A good plot shows histograms that look well-matched, indicating that the ",
                " Hamiltonian Monte Carlo should perform robustly.",
                " The closer ", " \\(\\pi_{\\Delta E}\\)", " is to ", " \\(\\pi_E\\)", 
                " the faster the random walk explores the energies and the smaller the autocorrelations will be in the chain.",
                " If ", " \\(\\pi_{\\Delta E}\\)", " is narrower than ", " \\(\\pi_E\\)",
                " the random walk is less effective and autocorrelations will be larger. Additionally",
                " the chain may not be able to completely explore the tails of the target distribution.",
                " See Betancourt <a href = https://arxiv.org/abs/1701.02434>'A conceptual introduction to Hamiltonian Monte Carlo'</a>",
                " and Betancourt <a href = 'https://arxiv.org/abs/1604.00695'>'Diagnosing suboptimal cotangent disintegrations in Hamiltonian Monte Carlo'</a>",
                " for the general theory behind the energy plots.", withMathJax()
                ))
    
  
  }
  
  output$caption <- renderUI({
    captionOut()
  })
  
  
  return(reactive({
    if(include() == TRUE){
      # customized plot options return without setting the options for the other plots
      save_old_theme <- bayesplot_theme_get()
      color_scheme_set(visualOptions()$color)
      bayesplot_theme_set(eval(parse(text = select_theme(visualOptions()$theme)))) 
      out <- list(plot = plotOut(chain = chain()), 
                  caption = captionOut())
      bayesplot_theme_set(save_old_theme)
      out
    } else {
      NULL
    }
  }))
  
}