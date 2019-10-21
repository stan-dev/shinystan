# shinystan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinystan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.


#' Generate a report with diagnostics for a shinystan object
#' 
#' The report displays diagnostics information about the worst parameters in 
#' your model, based on effective sample size.
#' 
#' @export
#' @template args-sso
#' @param n_param On how many parameters do you want the report to be? 
#' @param output_format What type of report would you like? The options are
#'   'html_document', 'pdf_document' and 'word_document'.
#' @param view Do you want to open the report after it is generated?
#'   
#' @return A report is generated and the path where it is stored is printed.
#' 
#' @examples
#'  \dontrun{
#' # Using example shinystan object 'eight_schools'
#' generate_report(eight_schools)
#' }
#' 

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




