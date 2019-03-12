densityPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 3),
        column(width = 4, h5("Parameter")),
        column(width = 4, h5("Transformation"))
      ),
      fluidRow(
        column(width = 3),
        column(
          width = 4,
          selectizeInput(
            inputId = ns("diagnostic_param"),
            label = NULL,
            multiple = TRUE,
            choices = sso@param_names,
            selected = c(sso@param_names[1])
          )
        ),
        column(
          width = 4,
          uiOutput(ns("transform"))
        )
      )
    ),
    plotOutput(ns("plot1"))
  )
}


densityPlot <- function(input, output, session){
  
  
  output$transform <- renderUI({
    
    inverse <- function(x) 1/x
    cloglog <- function(x) log(-log1p(-x))
    square <- function(x) x^2
    
    transformation_choices <- c(
        "abs", "atanh",
        cauchit = "pcauchy", "cloglog",
        "exp", "expm1",
        "identity", "inverse", inv_logit = "plogis",
        "log", "log10", "log2", "log1p", logit = "qlogis",
        probit = "pnorm", "square", "sqrt"
      )
    
    selectInput(
      inputId = session$ns("transformation"),
      label = NULL,
      choices = transformation_choices,
      selected = "identity"
    )
    
  })
  
  param <- reactive(input$diagnostic_param)
  transform <- reactive({
    validate(
      need(is.null(input$transformation) == FALSE, "")
    )
    input$transformation
  })
  
  output$plot1 <- renderPlot({
    
    color_scheme_set("blue")
    validate(
      need(length(param()) > 0, "Select at least one parameter.")
    )
    mcmc_dens(
      sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, , ],
      pars = param(),
      transformations = transform()
    )
  })
}