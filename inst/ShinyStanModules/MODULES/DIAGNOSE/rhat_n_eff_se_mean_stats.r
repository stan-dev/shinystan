rhat_n_eff_se_mean_statsUI <- function(id){
  # for internal namespace structure
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(width = 3),
        column(width = 4),
        column(width = 4, h5("Decimals"))
      ),
      fluidRow(
        column(width = 3),
        column(width = 4),
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




rhat_n_eff_se_mean_stats <- function(input, output, session){
  
  MCMCtable <- reactive({
    out <- sso@summary[, c("Rhat", "n_eff", "se_mean", "sd")]
    out[, 2] <- out[, 2] / ((sso@n_iter - sso@n_warmup) * sso@n_chain)
    out[, 3] <- out[, 3] / out[, 4]
    out <- out[, 1:3]
    # colnames(out) <-  c(withMathJax("\\(\\hat{R}\\)"), withMathJax("\\(n_{eff} / N\\)"), 
    #              withMathJax("\\(mcse / sd\\)"))
    colnames(out) <- c("Rhat", "n_eff / N", "se_mean / sd")
    out <- formatC(round(out, input$sampler_digits),
                   format = 'f', digits = input$sampler_digits)
    out
  })
  
  
  output$sampler_summary <- DT::renderDataTable({
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
