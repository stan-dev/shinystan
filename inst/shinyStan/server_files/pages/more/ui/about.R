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

output$ui_about <- renderUI({
  div(
    h3("ShinyStan"),
    htmlOutput("ui_credits"),
    actionLink(style = "text-decoration: underline;", 
               inputId = "citation_modal", label = "Citing ShinyStan"),
    uiOutput("ui_cite"),
    br(),
    h3("Stan & RStan"),
    a(style = "text-decoration: underline;", "Stan Development Team", 
      href="http://mc-stan.org/team/"),
    br()
  )
})