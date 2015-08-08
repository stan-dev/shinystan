output$save_pdf_multiparam = downloadHandler(
  filename = "shinstan-parameters-plot.pdf",
  content = function(file) {
    ggsave(file, plot = multiparam_plot(), device = pdf)
  })
output$save_pdf_bivariate = downloadHandler(
  filename = "shinstan-scatter.pdf",
  content = function(file) {
    ggsave(file, plot = bivariate_plot(), device = pdf)
  })
output$save_pdf_density = downloadHandler(
  filename = "shinstan-density.pdf",
  content = function(file) {
    ggsave(file, plot = density_plot(), device = pdf)
  })
output$save_pdf_histogram = downloadHandler(
  filename = "shinstan-histogram.pdf",
  content = function(file) {
    ggsave(file, plot = hist_plot(), device = pdf)
  })