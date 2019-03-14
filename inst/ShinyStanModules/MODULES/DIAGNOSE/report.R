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
  
  output$downloadRDS <- downloadHandler(
    filename = 'test.rds',
    content = function(file) {
      saveRDS(ggplotsList(), file)
    })  
  
  
  
}