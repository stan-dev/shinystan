reportUI <- function(id) {
  # for internal namespace structure
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 2, align = "left",
               radioButtons(inputId = ns("format"),
                            label = h5("Choose Format"),
                            choices = c("html", "pdf", "word", "rmd"),
                            selected = "html"
               )
        ),
        column(width = 4,
               splitLayout(
                 div(style = "width: 90%;",
                     radioButtons(inputId = ns("autoParam"),
                                  label = h5("Choose Parameters"),
                                  choices = c("No", "Yes"),
                                  selected = "No"
                     )
                 ),
                 div(style = "width: 90%;",
                     numericInput(
                       inputId = ns("nParams"),
                       label = h5("How many?"),
                       value = 3, 
                       min = 1,
                       max = (shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_iter - shinystan:::.sso_env$.SHINYSTAN_OBJECT@n_warmup - 2),
                       step = 1
                     )
                 )
               )
        ),
        column(width = 6, 
               verticalLayout(
                 selectizeInput(
                   inputId = ns("named_param"),
                   label = h5("Parameter"),
                   multiple = TRUE,
                   # choices = .make_param_list(shinystan:::.sso_env$.SHINYSTAN_OBJECT),
                   choices = unlist(.make_param_list(shinystan:::.sso_env$.SHINYSTAN_OBJECT))[-which(unlist(.make_param_list(shinystan:::.sso_env$.SHINYSTAN_OBJECT)) == "log-posterior")], 
                   selected = shinystan:::.sso_env$.SHINYSTAN_OBJECT@param_names[order(shinystan:::.sso_env$.SHINYSTAN_OBJECT@summary[, "n_eff"])[1:2]]
                 )
               )
        )
      )
    ),
    fluidRow(
      align="center",
      br(), br(),
      # downloadButton(ns('downloadPlot'), 'Download Plots', class = "downloadReport"),
      downloadButton(ns("downloadPDFreport"), "Download Report", class = "downloadReport"),
      # downloadButton(ns('downloadRDS'), 'Download RDS', class = "downloadReport"))
    )
  )
}

report <- function(input, output, session, ggplotsList, reportType, ...) {
  
  nParam <- reactive({input$nParams})
  namedParam <- reactive({input$named_param})
  formatType <- reactive({
    if(input$format == "html") return("html_document")
    if(input$format == "pdf") return("pdf_document")
    if(input$format == "word") return("word_document")
    if(input$format == "rmd") return("rmd_document")
  })
  chooseParam <- reactive({input$autoParam})
  pars <- reactive({
    if(chooseParam() == "Yes") return(namedParam()) else return(NULL)
  })
  
  
  observeEvent(input$autoParam, {
    if(input$autoParam == "No") {
      shinyjs::enable(id = "nParams")
    } else {
      shinyjs::disable(id = "nParams")
    }
    if(input$autoParam == "Yes") {
      shinyjs::enable(id = "named_param")
    } else {
      shinyjs::disable(id = "named_param")
    }
  })
  
  output$downloadPDFreport <- downloadHandler(
    filename = "shinystanReport.html",
    # print(chooseParam()),
    # print(namedParam()),
    # print(formatType()),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      if(formatType() == "rmd_document") {
        ifelse(reportType == "diagnose", 
               file.show("reports/generate_report_diagnostics.Rmd"),
               file.show("reports/generate_report_estimates.Rmd"))
        
      } else {
        
        shinystan::generate_report(sso = shinystan:::.sso_env$.SHINYSTAN_OBJECT,
                                   n_param = nParam(),
                                   pars = pars(),
                                   output_format = formatType(),
                                   report_type = reportType
        )
      }
    }
    
  )
  # filename = "shinystanReport.pdf",
  # content = function(file) {
  #   # Copy the report file to a temporary directory before processing it, in
  #   # case we don't have write permissions to the current working dir (which
  #   # can happen when deployed).
  #   tempDirectory <- tempdir()
  #   if(reportType == "diagnoseReport") tempReport <- file.path(tempDirectory, "diagnostics_report.Rmd")
  #   if(reportType == "estimateReport") tempReport <- file.path(tempDirectory, "estimate_report.Rmd")
  #   tempStanFig <- file.path(tempDirectory, "stan_logo.png")
  #   tempStanFig2 <- file.path(tempDirectory, "wide_ensemble.png")
  #   
  #   
  #   if(reportType == "diagnoseReport") file.copy("reports/diagnostics_report.Rmd", tempReport, overwrite = TRUE)
  #   if(reportType == "estimateReport") file.copy("reports/estimate_report.Rmd", tempReport, overwrite = TRUE)
  #   file.copy('www/stan_logo.png', tempStanFig, overwrite = TRUE)   
  #   file.copy('www/wide_ensemble.png', tempStanFig2, overwrite = TRUE)   
  #   # Set up parameters to pass to Rmd document
  #   params <- ggplotsList()
  #   
  #   # Knit the document, passing in the `params` list, and eval it in a
  #   # child of the global environment (this isolates the code in the document
  #   # from the code in this app).
  #   rmarkdown::render(tempReport, output_file = file,
  #                     params = params,
  #                     envir = new.env(parent = globalenv()),
  #                     quiet = TRUE
  #   )
  #   }
  # )
  
  
}