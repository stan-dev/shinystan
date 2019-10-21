generate_report <- function (sso, n_param = 3, output_format = "html_document", view = TRUE) {
  if(class(sso) != "shinystan") stop("Object is not of class 'shinystan'.")
  path <- rmarkdown::render(input = system.file("ShinyStanModules/reports/report_function.Rmd",
                                                package = "shinystan"), 
                            output_format = output_format, output_dir = getwd())
  message("File saved to ", path)
  if (view) {
    system2("open", shQuote(path))
  }
}




