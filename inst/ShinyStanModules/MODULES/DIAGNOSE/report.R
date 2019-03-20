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
      br(), br(),
      downloadButton(ns('downloadPlot'), 'Download Plots', class = "downloadReport"),
      downloadButton(ns("downloadPDFreport"), "Download Report", class = "downloadReport"),
      downloadButton(ns('downloadRDS'), 'Download RDS', class = "downloadReport"))
  )
  )
  
}

report <- function(input, output, session, ggplotsList, ...) {
  
  # input_names <- names(list(...))
  # print(input_names)
  
  output$downloadPlot <- downloadHandler(
    filename = 'test.pdf',
    content = function(file) {
      # ggsave(file, gridExtra::arrangeGrob(grobs = downloadSelection()))
      pdf(file)
      lapply(ggplotsList(), print)
      dev.off()
    })
  
  output$downloadPDFreport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "diagnostics_report.Rmd")
      file.copy("reports/diagnostics_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- ggplotsList()
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$downloadRDS <- downloadHandler(
    filename = 'test.rds',
    content = function(file) {
      saveRDS(ggplotsList(), file)
    })  
  
  
  
}