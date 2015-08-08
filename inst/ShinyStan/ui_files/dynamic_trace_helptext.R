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

div(
  br(),
  helpText(style = "font-size: 11px;", 
           "Use your mouse to highlight areas in the traceplot to zoom into. Double-click to reset.",
           "You can also use the range selector below the graph for panning and zooming.",
           "The number in the small black box in the bottom left corner controls the", em("roll period."),
           "If you specify a roll period of N the resulting graph will be a moving average,", 
           "with each plotted point representing the average of N points in the data.")
)
