summaryTableUI <- function(id){
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
                 choices = .make_param_list_with_groups(shinystan:::.sso_env$.SHINYSTAN_OBJECT),
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
      ),
      fluidRow(
        column(width = 6, align = "left",
               checkboxGroupInput(
                 ns("tex_columns"),
                 label = h5("Columns"),
                 choices = if(shinystan:::.sso_env$.SHINYSTAN_OBJECT@misc$stan_method == "sampling"){
                   c("Posterior mean" = "mean",
                     "Monte Carlo error" = "se_mean",
                     "Posterior standard deviation" = "sd",
                     "Quantile: 2.5%" = "2.5%",
                     "Quantile: 25%" = "25%",
                     "Quantile: 50%" = "50%",
                     "Quantile: 75%" = "75%",
                     "Quantile: 97.5%" = "97.5%",
                     "Effective sample size" = "n_eff",
                     "Rhat"
                   )
                 } else {
                   c("Posterior mean" = "mean",
                     "Posterior standard deviation" = "sd",
                     "Quantile: 2.5%" = "2.5%",
                     "Quantile: 25%" = "25%",
                     "Quantile: 50%" = "50%",
                     "Quantile: 75%" = "75%",
                     "Quantile: 97.5%" = "97.5%"
                   )
                 },
                 selected = c("mean", "sd", "2.5%", "50%", "97.5%"),
                 inline = TRUE
               )     
        ), 
        column(width = 4),
        column(width = 2, align = "right")
      )
    ),
    DT::dataTableOutput(ns("summaryTable"))
  )
}


summaryTable <- function(input, output, session){
  
  param <- reactive(unique(.update_params_with_groups(params = input$diagnostic_param,
                                                      all_param_names = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names)))
  digits <- reactive(input$sampler_digits)
  selectedSummaries <- reactive(input$tex_columns)
  
  summaryStats <- reactive({
    
    select.columns <- c(which(colnames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary) %in% input$tex_columns))
    
    out <- shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[param(), select.columns]
    out <- matrix(out, nrow = length(param()))
    rownames(out) <- param()
    colnames(out) <- colnames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary)[select.columns]
    out <- formatC(round(out, digits()), format = 'f', digits = digits())
    out
    
  })
  
  output$summaryTable <- DT::renderDataTable({
    validate(
      need(length(param()) > 0, "Select at least one parameter."),
      need(length(input$tex_columns) > 0, "Select at least one summary.")
    )
    DT::datatable({
      summaryStats() 
    }, options = list(
      processing = TRUE,
      deferRender = TRUE,
      scrollX = TRUE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      paging = FALSE,
      searching = TRUE,
      info = FALSE
    ))
  })
  
  
}