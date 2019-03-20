rhat_n_eff_se_mean_statsUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 6,
               selectizeInput(
                 inputId = ns("diagnostic_param"),
                 label = h5("Parameter"),
                 multiple = TRUE,
                 choices = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names,
                 selected = if(length(shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names) > 9) shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[1:10] else shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names
               )
        ), 
        column(width = 4),
        column(width = 2, align = "right",
               div(style = "width: 100px;",
                   numericInput(
                     ns("sampler_digits"),
                     label = h5("Decimals"),
                     value = 2,
                     min = 0,
                     max = 10,
                     step = 1
                   )
               )
        )
      )
    ),
    DT::dataTableOutput(ns("sampler_summary"))
  )
  
}




rhat_n_eff_se_mean_stats <- function(input, output, session){
  
  param <- reactive(input$diagnostic_param)
  digits <- reactive(input$sampler_digits)
  
  MCMCtable <- reactive({
    out <- shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, c("Rhat", "n_eff", "se_mean", "sd")]
    out[, 2] <- out[, 2] / ((shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup) * shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_chain)
    out[, 3] <- out[, 3] / out[, 4]
    out <- out[param(), 1:3]
    if(length(param()) == 1) out <- matrix(out, nrow = 1); rownames(out) <- param()
    colnames(out) <- c("Rhat", "n_eff / N", "se_mean / sd")
    out <- formatC(round(out, input$sampler_digits),
                   format = 'f', digits = digits())
    out
  })
  
  
  output$sampler_summary <- DT::renderDataTable({
    validate(
      need(length(param()) > 0, "Select at least one parameter.")
    )
    DT::datatable({
      MCMCtable() 
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
