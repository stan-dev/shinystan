reportUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(div(
      img(
        src = "wide_ensemble.png",
        class = "wide-ensemble",
        width = "100%"
      )
    ),
    div(
      style = "margin-top: 25px",
      img(src = "stan_logo.png", class = "stan-logo"),
      div(id = "shinystan-title", "ShinyStan")
    )),
    fluidRow(
      align="center",
      br(), br(),
      wellPanel(id = "selectVariableTab",
                fluidRow(h4("Please note that you need to visit the tabs, and, if applicable, generate the plots before you can download the report."),
                         br()),
                fluidRow(
                  if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS") h4("NUTS/HMC Diagnostics Options"),
                  if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS") column(width = 4, 
                         radioButtons(ns("divergentScatter"), "Include Divergent Scatter Plot",
                                                 choices = c(TRUE, FALSE), selected = FALSE),
                         radioButtons(ns("divergentTransitions"), "Include Divergent Transitions Plot",
                                      choices = c(TRUE, FALSE), selected = FALSE),
                         radioButtons(ns("stepSize"), "Include Step Size Plot",
                                      choices = c(TRUE, FALSE), selected = FALSE)),
                  if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS") column(width = 4, 
                         radioButtons(ns("parcoord"), "Include Parallel Coordinates Plot",
                                                 choices = c(TRUE, FALSE), selected = FALSE),
                         radioButtons(ns("energy"), "Include Energy Plot",
                                      choices = c(TRUE, FALSE), selected = FALSE),
                         radioButtons(ns("acceptance"), "Include Acceptance Plot",
                                      choices = c(TRUE, FALSE), selected = FALSE)),
                  if(sso@misc$stan_method == "sampling" & sso@misc$stan_algorithm == "NUTS") column(width = 4, 
                         radioButtons(ns("pairs"), "Include Pairs Plot",
                                                 choices = c(TRUE, FALSE), selected = FALSE),
                         radioButtons(ns("treedepth"), "Include Treedepth Plot",
                                      choices = c(TRUE, FALSE), selected = FALSE))),
                fluidRow(
                if(sso@misc$stan_method == "sampling") h4("MCMC Diagnostics Options"),
                if(sso@misc$stan_method == "sampling") column(width = 4, 
                        radioButtons(ns("trace"), "Include Trace Plot",
                                     choices = c(TRUE, FALSE), selected = FALSE),
                        radioButtons(ns("se_mean"), "Include se_mean Plot",
                                     choices = c(TRUE, FALSE), selected = FALSE)),
                if(sso@misc$stan_method == "sampling") column(width = 4, 
                        radioButtons(ns("rhat"), "Include Rhat Plot",
                                     choices = c(TRUE, FALSE), selected = FALSE),
                        radioButtons(ns("autocorrelation"), "Include Autocorrelation Plot",
                                     choices = c(TRUE, FALSE), selected = FALSE)),
                if(sso@misc$stan_method == "sampling") column(width = 4, 
                        radioButtons(ns("n_eff"), "Include n_eff Plot",
                                     choices = c(TRUE, FALSE), selected = FALSE))
                )
      ),
      br(), br(),
      downloadButton(ns('downloadPlot'), 'Download Plots'),
      downloadButton(ns('downloadRDS'), 'Download RDS'))
  )
  
}

report <- function(input, output, session, ggplotsList,
                   getParcoordPlot, getPairsPlot, ...) {
  
  # input_names <- names(list(...))
  # print(input_names)
  
  output$downloadPlot <- downloadHandler(
    filename = 'test.pdf',
    content = function(file) {
      # ggsave(file, gridExtra::arrangeGrob(grobs = downloadSelection()))
      pdf(file)
      # if(is.na(match("divergentScatterPlot", input_names)) == FALSE) if(input$divergentScatter == TRUE) print(divergentScatterPlot())
      
      if(input$divergentScatter == TRUE) print(ggplotsList()["divergentScatterPlot"])
      if(input$pairs == TRUE) print(getPairsPlot()["pairsPlot"])
      if(input$parcoord == TRUE) print(getParcoordPlot()["parcoordPlot"])
      if(input$divergentTransitions == TRUE) print(ggplotsList()["divergentTransitionsPlot"])
      if(input$energy == TRUE) print(ggplotsList()["energyPlot"])
      if(input$treedepth == TRUE) print(ggplotsList()["treedepthPlot"])
      if(input$stepSize == TRUE) print(ggplotsList()["stepSizePlot"])
      if(input$acceptance == TRUE) print(ggplotsList()["acceptancePlot"])
      if(input$trace == TRUE) print(ggplotsList()["tracePlot"])
      if(input$rhat == TRUE) print(ggplotsList()["rhatPlot"])
      if(input$n_eff == TRUE) print(ggplotsList()["n_effPlot"])
      if(input$se_mean == TRUE) print(ggplotsList()["se_meanPlot"])
      # print(ggplotsList())
      dev.off()
    })  
  
  output$downloadRDS <- downloadHandler(
    filename = 'test.rds',
    content = function(file) {
      saveRDS(ggplotsList(), file)
    })  
  
  
  
}