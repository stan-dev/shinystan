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



output$ui_help <- renderUI({
  tags$div(
  br(),
  a(style = "color: maroon; font-size: 15px;", 
    "Click here to report a bug, request a new feature, or ask a question.", 
    href = "https://github.com/stan-dev/shinystan/issues"),
  br(),br(),
  h3("shinyStan help"),
  p("More coming soon."),
  uiOutput("help_navlist"), # output navlist with help files
  br(),
  hr(),
  h3("Stan help"),
  a("Stan website", href = "http://mc-stan.org"),
  br(),
  a("Stan users google group", href = "https://groups.google.com/forum/#!forum/stan-users")
  )
})
