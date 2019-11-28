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
#' @param n_param On how many parameters do you want the report to be? To
#'    print the report for all parameters put 'n_param = Inf` or
#'    n_param = "all"`.
#' @param pars An optional character vector of parameter names. If no names are 
#'    specified the n_param count will be used. Arguments passed to pars take
#'    precedence over the n_param argument.
#' @param output_format What type of report would you like? The options are
#'   'html_document', 'pdf_document' and 'word_document'.
#' @param view Do you want to open the report after it is generated?
#' @param report_type What type of report would you like? The options are
#'   'diagnose', 'estimate' and 'both'. The default is 'diagnose'.
#'   
#' @return A report is generated and the path where it is stored is printed.
#' 
#' @examples
#' \dontrun{
#' # Using example shinystan object 'eight_schools'
#' generate_report(eight_schools, report_type = "both")
#' 
#' ######################
#' ### stanfit object ###
#' ######################
#' library("rstan")
#' fit <- stan_demo("eight_schools")
#' sso <- as.shinystan(fit, model_name = "example")
#' generate_report(sso, pars = c("tau", "mu", "theta[1]"), 
#'                 output_format = "word_document")
#' # to change figure visualization use bayesplot::bayesplot_theme_set()
#' bayesplot::bayesplot_theme_set(theme_dark())
#' generate_report(sso, pars = c("tau", "mu", "theta[1]"))
#' 
#'  
#' ######################
#' ### stanreg object  ##
#' ######################
#' 
#' library("rstanarm")
#' example("example_model")
#' sso <- as.shinystan(example_model)
#' generate_report(sso)
#' 
#' 
#' ######################
#' ### brms object    ###
#' ######################
#' 
#' library(brms)
#' bprior1 <- prior(student_t(5,0,10), class = b) +
#'   prior(cauchy(0,2), class = sd)
#'   fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
#'               data = epilepsy, family = poisson(), prior = bprior1)
#' sso <- as.shinystan(fit1$fit)
#' generate_report(sso, n_param = 5)
#' 
#' 
#' }
#' 

generate_report <- function (sso, n_param = 3, pars = NULL, output_format = "html_document", 
                             view = TRUE, report_type = "diagnose") {
  if(class(sso) != "shinystan") stop("Object is not of class 'shinystan'.")
  if(sso@stan_used == FALSE) stop("Currently only available for stan related objects.")
  if(sso@stan_algorithm == "variational" & report_type == "diagnose"){
     stop("Currently no diagnostics available for variational inference.")
  } 
  if(is.null(pars) == FALSE & class(pars) != "character") stop("pars should be a character vector.")
  if(is.null(pars) == FALSE & all(pars %in% sso@param_names) == FALSE) stop("Invalid parameters in pars.")
  if(report_type %in% c("estimate", "diagnose", "both") == FALSE) stop("Invalid input for report_type.")
  
  if(report_type == "diagnose"){
    path <- rmarkdown::render(input = system.file("ShinyStanModules/reports/report_function.Rmd",
                                                  package = "shinystan"), 
                              output_format = output_format, 
                              output_file = "ShinyStan_diagnostics_report")  
  }
  if(report_type == "estimate") {
    path <- rmarkdown::render(input = system.file("ShinyStanModules/reports/report_function_estimates.Rmd",
                                                  package = "shinystan"), 
                              output_format = output_format, 
                              output_file = "ShinyStan_estimates_report")
  }
  if(report_type == "both") {
    path1 <- rmarkdown::render(input = system.file("ShinyStanModules/reports/report_function.Rmd",
                                                  package = "shinystan"), 
                              output_format = output_format, 
                              output_file = "ShinyStan_diagnostics_report")  
    path2 <- rmarkdown::render(input = system.file("ShinyStanModules/reports/report_function_estimates.Rmd",
                                                  package = "shinystan"), 
                              output_format = output_format, 
                              output_file = "ShinyStan_estimates_report")
  }
  
  if(report_type == "diagnose" | report_type == "estimate"){
    message("File saved to ", path)
    if (view) {
      system2("open", shQuote(path))
  }
  }
  
  if(report_type == "both"){
    message("Files saved to ", path1, " and ", path2)
    if (view) {
      system2("open", shQuote(path1))
      system2("open", shQuote(path2))
    }
  }
  
}




