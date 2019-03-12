divergentScatterUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 3, h5(textOutput(ns("diagnostic_chain_text")))),
        column(width = 4, h5("Parameter")),
        column(width = 4, h5("Transformation"))
      ),
      fluidRow(
        column(
          width = 3, div(style = "width: 100px;",
                         numericInput(
                           ns("diagnostic_chain"),
                           label = NULL,
                           value = 0,
                           min = 0,
                           # don't allow changing chains if only 1 chain
                           max = ifelse(sso@n_chain == 1, 0, sso@n_chain)
                         )
          )),
        column(
          width = 4,
          selectizeInput(
            inputId = ns("diagnostic_param"),
            label = NULL,
            multiple = TRUE,
            choices = sso@param_names,
            selected = c(sso@param_names[1],sso@param_names[which(sso@param_names == "log-posterior")]),
            options = list(maxItems = 2)
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


divergentScatter <- function(input, output, session){
  
  chain <- reactive(input$diagnostic_chain)
  param <- reactive(input$diagnostic_param)
  
  output$transform <- renderUI({
    
    inverse <- function(x) 1/x
    cloglog <- function(x) log(-log1p(-x))
    square <- function(x) x^2
    
    validate(
      need(length(param()) == 2, "Select two parameters.")
    )
    transformation_choices <- c(
      "abs", "atanh",
      cauchit = "pcauchy", "cloglog",
      "exp", "expm1",
      "identity", "inverse", inv_logit = "plogis",
      "log", "log10", "log2", "log1p", logit = "qlogis",
      probit = "pnorm", "square", "sqrt"
    )
    fluidRow(
      selectInput(
        inputId = session$ns("transformation"),
        label = NULL,
        choices = transformation_choices,
        selected = "identity"
      ),
      selectInput(
        inputId = session$ns("transformation2"),
        label = NULL,
        choices = transformation_choices,
        selected = "identity"
      )
    )
  })
  
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
  
  
  plotOut <- function(parameters, chain, transformations){
    
    color_scheme_set("darkgray")
    validate(
      need(length(parameters) == 2, "Select two parameters.")
    )
    mcmc_scatter(
      if(chain != 0) {
        sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, chain, ]
      } else {
        sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, , ]
      },
      pars = parameters,
      transformations = transformations,
      np = if(chain != 0) {
        nuts_params(list(sso@sampler_params[[chain]]) %>%
                      lapply(., as.data.frame) %>%
                      lapply(., filter, row_number() > sso@n_warmup) %>%
                      lapply(., as.matrix))
      } else {
        nuts_params(sso@sampler_params %>%
                      lapply(., as.data.frame) %>%
                      lapply(., filter, row_number() > sso@n_warmup) %>%
                      lapply(., as.matrix)) 
        
      },
      np_style = scatter_style_np(div_color = "green", div_alpha = 0.8)
    ) + labs(#title = "",
             #subtitle = "Generated via ShinyStan",
             caption = paste0("Scatter plot of ", parameters[1]," and ", parameters[2],
                              " with highlighted divergent transitions."))
    
    
  }
  
  output$plot1 <- renderPlot({
    plotOut(parameters = param(), chain = chain(),
            transformations = transform())
  })
  
  return(reactive({plotOut(parameters = param(), chain = chain(),
                           transformations = transform())}))
  
  }