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
            choices = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names,
            selected = if(length(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names) > 9) shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[1:10] else shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names
          )
        ),
        column(
          width = 4,
          div(style = "width: 100px;",
              numericInput(
                ns("sampler_digits"),
                label = NULL,
                value = 2,
                min = 0,
                max = 10,
                step = 1
              )
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
    
    remove.colums <- if(shinystan:::.sso_env$.SHINYSTAN_OBJECT@misc$stan_method == "sampling"){
      c(which(colnames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary) == "Rhat"), which(colnames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary) == "n_eff"))
    } else {
      c(which(colnames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary) == "Rhat"), which(colnames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary) == "n_eff"),
        which(colnames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary) == "se_mean"))
    }
    
    if(length(param()) == 1){
      
      out <- shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[param(), -remove.colums]
      out <- matrix(out, nrow = 1)
      rownames(out) <- param()
      colnames(out) <- colnames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary)[-remove.colums]
      out <- formatC(round(out, digits()), format = 'f', digits = digits())
      out
      
    } else {
      out <- shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[param(), -remove.colums]
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