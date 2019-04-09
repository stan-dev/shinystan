summaryTableLatexUI <- function(id){
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
        column(width = 4,
               textInput(ns("tex_caption"), label = h5("Caption"))),
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
                     "Monte Carlo error" = "se_mean",
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
        column(width = 2, align = "right",
               checkboxGroupInput(
                 ns("tex_pkgs"),
                 h5("Packages"),
                 choices = c("Booktabs", "Longtable"),
                 selected = NULL,
                 inline = FALSE
               ))
      )
    ),
    verbatimTextOutput(ns("summaryLatexTable"))
  )
}


summaryTableLatex <- function(input, output, session){
  
  param <- reactive(input$diagnostic_param)
  digits <- reactive(input$sampler_digits)
  selectedSummaries <- reactive(input$tex_columns)
  

  summary_stats_latex <- reactive({
    
    xt <- xtable::xtable(summaryStats(), 
                         caption = input$tex_caption)
    
    xtable::print.xtable(
      xt,
      booktabs = "Booktabs" %in% input$tex_pkgs,
      tabular.environment = ifelse("Longtable" %in% input$tex_pkgs, "longtable", "tabular"),
      include.rownames = TRUE
    )
  })
  
  output$summaryLatexTable <- renderPrint({
    validate(
      need(length(param()) > 0, "Select at least one parameter."),
      need(length(input$tex_columns) > 0, "Select at least one summary.")
    )
    
    summary_stats_latex()
  })
  
  
  summaryStats <- reactive({
    
    select.columns <- c(which(colnames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary) %in% input$tex_columns))
    
    out <- shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[param(), select.columns]
    out <- matrix(out, nrow = length(param()))
    rownames(out) <- param()
    colnames(out) <- colnames(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary)[select.columns]
    out <- formatC(round(out, digits()), format = 'f', digits = digits())
    out
    
  })
  
  
}