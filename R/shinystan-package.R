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


#' \pkg{shinystan} R package ('ShinyStan' graphical user interface)
#'
#' @docType package
#' @name shinystan-package
#'
#' @description
#' \if{html}{
#'   \figure{stanlogo.png}{options: width="50" alt="mc-stan.org"}
#'   \emph{Stan Development Team}
#' }
#'
#' Applied Bayesian data analysis is primarily implemented through the Markov
#' chain Monte Carlo (MCMC) algorithms offered by various software packages.
#' When analyzing a posterior sample obtained by one of these algorithms the
#' first step is to check for signs that the chains have converged to the target
#' distribution and and also for signs that the algorithm might require tuning
#' or might be ill-suited for the given model. There may also be theoretical
#' problems or practical inefficiencies with the specification of the model. The
#' ShinyStan app provides interactive plots and tables helpful for analyzing a
#' posterior sample, with particular attention to identifying potential problems
#' with the performance of the MCMC algorithm or the specification of the model.
#' ShinyStan is powered by the Shiny web application framework by RStudio
#' (\url{https://shiny.rstudio.com/}) and works with the output of MCMC programs
#' written in any programming language (and has extended functionality for
#' models fit using the \pkg{rstan} package and the No-U-Turn sampler).
#'
#' @section ShinyStan has extended functionality for Stan models:
#'
#' Stan (\url{https://mc-stan.org/}) models can be run in \R using the
#' \pkg{\link[rstan]{rstan}} package. Other packages like
#' \pkg{\link[rstanarm]{rstanarm}} and \pkg{brms} provide
#' higher-level interfaces to Stan that use \pkg{rstan} internally.
#'
#' @section Saving and sharing:
#'
#'   The \pkg{shinystan} package allows you to store the basic components of an
#'   entire project (code, posterior samples, graphs, tables, notes) in a single
#'   object, a \code{\link[=as.shinystan]{shinystan object}} (sso, for short).
#'   Users can save many of the plots as \pkg{ggplot2} objects for further
#'   customization and easy integration in reports or post-processing for
#'   publication.
#'
#'   The \code{\link{deploy_shinystan}} function lets you easily deploy your own
#'   ShinyStan apps online for any of your models using the shinyapps.io 
#'   service from 'RStudio'. Each of your apps (each of your models) will have a
#'   unique url and will be compatible with most web browsers.
#'
#' @section License:
#'
#'  The \pkg{shinystan} package is open source licensed under the GNU Public
#'  License, version 3 (GPLv3).
#'
#' @section Demo:
#'
#'  Check out the demo using \code{\link{launch_shinystan_demo}} or try it with
#'  one of your own models using \code{\link{launch_shinystan}}.
#'
#' @section Resources:
#' \itemize{
#'  \item Web page with online documentation
#'    (\url{https://mc-stan.org/shinystan/})
#'  \item Stan Forums on Discourse (\url{https://discourse.mc-stan.org})
#'  \item GitHub issue tracker (\url{https://github.com/stan-dev/shinystan/issues})
#' }
#'
#' @template seealso-as.shinystan
#' @template seealso-demo
#' @template seealso-launch
#' 
#' @template reference-muth
#' @template reference-bayesvis
#'
#' @import ggplot2
#' @import methods
#' @import shiny
#' @import dygraphs
#' @import bayesplot
#' @import threejs
#' @import utils
#' @import xts
#' @importFrom stats acf quantile time var na.omit
#' @importFrom shinythemes shinytheme
#' @importFrom colourpicker colourInput
#' @importFrom shinyjs useShinyjs
#' @importFrom DT datatable
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom stringi stri_sort
#' @importFrom markdown markdownToHTML
#' @importFrom reshape2 melt
#' @importFrom xtable xtable print.xtable
#'
NULL
