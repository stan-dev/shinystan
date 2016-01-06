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

div(style = "text-align: center; margin-top: 100px;",
    shinystan_version(),
    br(),br(),
    a(style = "font-size: 16px;", strong("Stan Development Team"), 
      href="http://mc-stan.org/team/"),
    br(),
    a(style = "font-size: 14px;", "mc-stan.org", href="http://mc-stan.org/"),
    div(
      actionLink(inputId = "shinystan_citation_show", label = "Show Citation", 
                 style = "font-size: 12px;", class = "aoptions"),
      div(
          shinyjs::hidden(wellPanel(id = "citation_div", style = "text-align: left;",
                                    includeHTML("html/citation.html")
          )))
    ),
    br(),br(),
    h6("Author"),
    helpText(style = "font-size: 12px;", "Jonah Gabry"),
    br(),
    h6(style = "font-size: 12px;", "Contributors"),
    helpText(style = "font-size: 12px;", 
             includeHTML("html/contribs.html")
    ),
    br(),
    h6("Logo"),
    helpText(style = "font-size: 12px;", 
             a(href = "http://mc-stan.org/team/", "Michael Betancourt")),
    br(),
    h6("Shiny"),
    helpText(style = "font-size: 12px;", "ShinyStan is powered by the", 
             a(href = "http://shiny.rstudio.com", 
               "Shiny web application framework"), "(RStudio)"),
    br(),
    h6("Source code"),
    a(style="color: #190201;", href="http://github.com/stan-dev/shinystan", 
      target="_blank", tags$i(class="fa fa-github fa-3x"))
)
