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



output$ui_tex_modal <- renderUI({
  bsModal("tex", title = withMathJax("\\(\\LaTeX\\)"), trigger = "tex_options",
          helpText(style = "color: #337ab7;","The table will print in the R console and can be pasted seamlessly into a .tex file."),
          br(),
          selectizeInput("tex_params", width = "100%", label = strong("Select or enter parameter names"), choices = .make_param_list_with_groups(object), multiple = TRUE,
                         options = list(placeholder = "Leave blank for all parameters")),
          
          fluidRow(
            column(3, numericInput("tex_digits", label = "Digits", value = 1, min = 0)),
            column(8, offset = 1, textInput("tex_caption", label = "Caption"))
          ),
          flowLayout(checkboxInput("tex_booktabs", strong("Booktabs"), value = TRUE),
                     helpText("Use toprule, midrule, and bottomrule tags from LaTex package 'booktabs'.")),
          flowLayout(checkboxInput("tex_long", strong("Longtable"), value = FALSE),
                     helpText("Use LaTeX package 'longtable' for tables longer than a single page.")),
          # hr(),
          actionButton("tex_go", withMathJax("Print \\(\\LaTeX\\)"), icon = icon("print", lib = "glyphicon"))
  )
})

