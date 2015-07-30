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


# trace_plot <- reactive({
# 
#   if (input$param == "") {
#     return()
#   }
#   
#   customize <- input$trace_customize
#   zoom <- input$tracezoom
# 
#   if (customize & is.null(input$trace_rect)) {
#     # delay until the customization inputs are ready
#     return()
#   }
# 
#   do.call(".param_trace", args = list(
#     param       = input$param,
#     dat         = par_samps_all(),
#     chain       = input$trace_chain,
#     warmup_val  = warmup_val,
#     inc_warmup  = input$trace_warmup,
#     palette     = input$trace_palette,
#     style       = ifelse(customize, input$trace_style, "line"),
#     rect        = ifelse(customize, input$trace_rect, "Samples"),
#     rect_color  = ifelse(customize, input$trace_rect_color, "skyblue"),
#     rect_alpha  = ifelse(customize, input$trace_rect_alpha, 0.15),
#     x1          = ifelse(zoom, input$xzoom[1], NA),
#     x2          = ifelse(zoom, input$xzoom[2], NA),
#     y1          = ifelse(zoom, input$yzoom[1], NA),
#     y2          = ifelse(zoom, input$yzoom[2], NA)
#   ))
# })
