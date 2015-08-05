# This file is part of shinystan
# Copyright (C) Jonah Gabry
#
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


# output$ui_cite <- renderUI({
#   div(style = "text-align: left;", 
#     shinyjs::hidden(wellPanel(id = "citation_div", pre(id = "citation_text", 
# "@Misc{shinystan-software:2015,
# title = {{shinystan}: {R} Package for Interactive Exploration of {MCMC} samples, Version 2.0.0},
# author = {Gabry, Jonah and Stan Development Team},
# year = {2015},
# abstract = {The shinystan R package provides the ShinyStan app for exploring Markov chain Monte Carlo output through interactive visualizations and tables.},
# url = {https://mc-stan.org}
# }")))
#   )
# })
output$ui_credits <- renderUI({
  # jonah_and_stan <- "Jonah Gabry and Stan Development Team"
  michael <- "Michael Andreae"
  yuanjun <- "Yuanjun Gao"
  dongying <- "Dongying Song"
  HTML(paste(michael, yuanjun, dongying, sep = '<br/>'))
})
output$ui_about <- renderUI({
  div(style = "text-align: center;",
    strong(style = "font-size: 16px;", "Jonah Gabry and Stan Development Team"),
    div(class = "aoptions",
      actionLink(inputId = "shinystan_citation_show", 
                 label = "Show Citation")
    ),
    p(style = "font-size: 12px; color: gray;", 
      a(style = "font-size: 15px;", "Stan Development Team", href="http://mc-stan.org/team/"), "(mc-stan.org)"),
    div(style = "text-align: left;", 
        shinyjs::hidden(wellPanel(id = "citation_div", pre(id = "citation_text", 
                                                           "@Misc{shinystan-software:2015,
title = {{shinystan}: {R} Package for Interactive Exploration of {MCMC} samples, Version 2.0.0},
author = {Gabry, Jonah and Stan Development Team},
year = {2015},
abstract = {The shinystan R package provides the ShinyStan app for exploring Markov chain Monte Carlo output through interactive visualizations and tables.},
url = {https://mc-stan.org}
}")))
    ),
    br(),
    h6("Additional contributors:"),
    helpText(style = "font-size: 12px;", htmlOutput("ui_credits")),
    br(),
    h6("Logo:"),
    helpText(style = "font-size: 12px;", "Michael Betancourt", br(), "with special thanks to Stephanie Mannhein for critical refinements. (CC-BY ND 4.0 license)")
  )
})