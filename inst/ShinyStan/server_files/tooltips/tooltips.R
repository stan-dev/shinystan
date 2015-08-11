# # This file is part of shinyStan
# # Copyright (C) 2015 Jonah Sol Gabry & Stan Development Team
# #
# # shinyStan is free software; you can redistribute it and/or modify it under the
# # terms of the GNU General Public License as published by the Free Software
# # Foundation; either version 3 of the License, or (at your option) any later
# # version.
# # 
# # shinyStan is distributed in the hope that it will be useful, but WITHOUT ANY
# # WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# # A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# # 
# # You should have received a copy of the GNU General Public License along with
# # this program; if not, see <http://www.gnu.org/licenses/>.
# 
# tooltip_ids <- c(
#   "download_multiview", 
#   "dynamic_trace_stack", 
#   "download_all_summary", 
#   "tex_options", 
#   "dens_transform_x", "hist_transform_x", "bivariate_transform_x", "trivariate_transform_x", 
#   "bivariate_transform_y", "trivariate_transform_y", 
#   "trivariate_transform_z"
#   )
# 
# tooltip_msgs <- c(
#   "Will be a list with three elements corresponding the the ggplot2 objects for the three plots.", 
#   "If 'Stacked' is selected, the chains will be stacked on top of one another rather than drawing them independently. The first series specified in the input data will wind up on top of the chart and the last will be on bottom. Note that the y-axis values no longer correspond to the true values when this option is enabled.",
#   "Save as data.frame (.RData)", 
#   "Print latex table to R console", 
#   rep("A function of x, e.g. log(x), sqrt(x), x^2, 1/x, etc. Should be a valid R expression.", 4),
#   rep("A function of y, e.g. log(y), sqrt(y), y^2, 1/y, etc. Should be a valid R expression.", 2),
#   "A function of z, e.g. log(z), sqrt(z), z^2, 1/z, etc. Should be a valid R expression."
#   )
# tooltip_placements <- c(rep("right", 4), rep("top", 7))
