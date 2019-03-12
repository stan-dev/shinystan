summaryTableUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 3),
        column(width = 4, h5("Parameter")),
        column(width = 4, h5("Decimals"))
      ),
      fluidRow(
        column(
          width = 3),
        column(
          width = 4,
          selectizeInput(
            inputId = ns("diagnostic_param"),
            label = NULL,
            multiple = TRUE,
            choices = sso@param_names,
            selected = if(length(sso@param_names) > 9) sso@param_names[1:10] else sso@param_names
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
    DT::dataTableOutput(ns("summaryTable"))
  )
}


summaryTable <- function(input, output, session){
  
  param <- reactive(input$diagnostic_param)
  digits <- reactive(input$sampler_digits)
  
  summaryStats <- reactive({
    
    remove.colums <- if(sso@misc$stan_method == "sampling"){
      c(which(colnames(sso@summary) == "Rhat"), which(colnames(sso@summary) == "n_eff"))
    } else {
      c(which(colnames(sso@summary) == "Rhat"), which(colnames(sso@summary) == "n_eff"),
        which(colnames(sso@summary) == "se_mean"))
    }
    
    if(length(param()) == 1){
      
      out <- sso@summary[param(), -remove.colums]
      out <- matrix(out, nrow = 1)
      rownames(out) <- param()
      colnames(out) <- colnames(sso@summary)[-remove.colums]
      out <- formatC(round(out, digits()), format = 'f', digits = digits())
      out
      
    } else {
      out <- sso@summary[param(), -remove.colums]
      out <- formatC(round(out, digits()), format = 'f', digits = digits())
      out
    }
  })
  
  output$summaryTable <- DT::renderDataTable({
    validate(
      need(length(param()) > 0, "Select at least one parameter.")
    )
    DT::datatable({
      summaryStats() 
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