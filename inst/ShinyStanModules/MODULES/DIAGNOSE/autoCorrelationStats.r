autoCorrelationStatsUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 3, h5(textOutput(ns("diagnostic_chain_text")))),
        column(width = 4, h5("Parameter")),
        column(width = 4, h5("Decimals"))
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
            selected = sso@param_names[1]
          )
        ),
        column(
          width = 4,
          numericInput(
            ns("sampler_digits"),
            label = NULL,
            value = 4,
            min = 0,
            max = 10,
            step = 1
          )
        )
      )
    ),
    DT::dataTableOutput(ns("sampler_summary"))
  )
  
}




autoCorrelationStats <- function(input, output, session){

  chain <- reactive(input$diagnostic_chain)
  param <- reactive(input$diagnostic_param)
  
  output$diagnostic_chain_text <- renderText({
    if (chain() == 0)
      return("All chains")
    paste("Chain", chain())
  })
  
  
  
  autocorrelationTable <- reactive({
    
    out <-  mcmc_acf( if(chain() != 0) {
      sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, chain(), ]
    } else {
      sso@posterior_sample[(1 + sso@n_warmup) : sso@n_iter, , ]
    }, pars = param()
    )
    
    outXT <- NULL
    for(i in 1:length(param())){

    if(chain() == 0){
    
    out2 <- out$data %>% xtabs(AC ~ Chain + Lag + Parameter, .)
    out2 <- matrix(out2[, , param()[i]], nrow = sso@n_chain)
    rownames(out2) <- paste(param()[i], 'chain', 1:sso@n_chain) 
    outXT <- rbind(outXT, out2)
       
    } else {
      
    out2 <- out$data %>% xtabs(AC ~ Chain + Lag + Parameter, .)
    out2 <- matrix(out2[, , param()[i]], nrow = 1)
    rownames(out2) <- paste(param()[i], 'chain', chain())
    outXT <- rbind(outXT, out2)
    }
    }

    outXT <- formatC(round(outXT, input$sampler_digits),
                   format = 'f', digits = input$sampler_digits)
    colnames(outXT) <- paste("Lag", 0:20)
    outXT
  })
  
  
  output$sampler_summary <- DT::renderDataTable({
    validate(
      need(length(param()) > 0, "Select at least one parameter.")
    )
    DT::datatable({
      autocorrelationTable() 
    }, options = list(
      processing = TRUE,
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = "200px",
      scrollCollapse = TRUE,
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    ))
  })
  
  
}
