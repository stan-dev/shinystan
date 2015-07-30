# This file is part of shinyStan
# Copyright (C) 2015 Jonah Sol Gabry & Stan Development Team
#
# shinyStan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinyStan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.


output$ui_about <- renderUI({
tags$div(
  h4("shinyStan"),
  htmlOutput("ui_credits"),
  br(),
  actionLink("citation_modal", "Citing shinyStan"),
  uiOutput("ui_cite"),
  br(),
  h4("Stan & RStan"),
  a("Stan Development Team", href="http://mc-stan.org/team.html"),
  hr(),
  h4("About shinyStan"),
  p("Most applied Bayesian data analysis requires employing a Markov chain 
Monte Carlo (MCMC) algorithm to obtain samples from the posterior 
    distributions of the quantities of interest. Diagnosing convergence, 
    checking the fit of the model, and producing graphical and numerical 
    summaries of the parameters of interest is an important but often 
    laborious process that slows the down the creative and exploratory 
    process of model building. The", strong("shinyStan"), "package and Shiny app is 
    designed to facilitate this process in two primary ways:"),
  
  h5(style = "color: #428bca; ", "1) Providing interactive visual model exploration"),
  
  p(strong("shinyStan"), "provides immediate, informative, customizable visual and 
    numerical summaries of model parameters and convergence diagnostics for 
    MCMC simulations. Although", strong("shinyStan"), "has some special features only 
    available for users of the", strong("RStan"), "package (the R interface to the 
    Stan programming language for Bayesian statistical inference), it can 
    also easily be used to explore the output from any other program (e.g. 
    Jags, Bugs, SAS) or any user-written MCMC algorithm."),
  
  h5(style = "color: #428bca; ", "2) Making saving and sharing more convenient"),
  p(strong("shinyStan"), "allows you to store the basic components of an entire 
    project (code, posterior samples, graphs, tables, notes) in a single 
    object. Users can also export graphics into their R sessions as ggplot2 
    objects for further customization and easy integration in reports or 
    post-processing for publication.")
  )

})