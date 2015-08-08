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

fluidRow(
  column(4, 
         helpText(style = "margin-bottom: 2px;", "Table tips:"),
         helpText(style = "margin-top: 2px; font-size: 11px;", 
                  "Drag column names to rearrange the table columns."
         )),
  column(2, offset = 4, 
         div(
           strong(id = "table_digits_txt", "Digits"),
           numericInput("table_digits", label = NULL, 
                        value = 1, min = 0, max = 7, step = 1)
         )
  ),
  column(2, a_glossary("open_glossary_from_table"))
)