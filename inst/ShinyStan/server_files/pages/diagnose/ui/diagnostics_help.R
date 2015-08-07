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


# 
# output$ui_diagnostics_help <- renderUI({
#   div(
#     withMathJax(),
#     actionLink("btn_open_nuts_glossary_copy", "Open glossary", 
#                icon = icon("book", lib = "glyphicon")),
#     uiOutput("nuts_glossary_modal_copy"),
#     p(h3(style = "color: #337ab7;","Validity of results")),
#     p(h4("Diagnostic")), 
#     p("Check where (if at all) the sampler is diverging."),
#     p(h4("Failed diagnostic?")),
#     p("Try rerunning the model with a higher target acceptance probability."),
#     p(h3(style = "color: #337ab7;","Slow sampling")),
#     p(h4("Diagnostic")), 
#     p("Make sure the tree depth is not saturating the maximum tree depth."),
#     p(h4("Failed diagnostic?")),
#     p("Try rerunning the model with a larger maximum treedepth.")
#   )
# })