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


pp_hist_resids <- reactive({
  pp_tests()
  s <- sample_id_for_resids()
  resids <- get_y() - get_yrep()[s, ]
  names(resids) <- paste0("resids(yrep_",s,")")
  do.call(".pp_hist_resids", args = list(
    resids = resids
  ))
})

output$pp_hist_resids_out <- renderPlot({
  x <- suppressMessages(pp_hist_resids())
  suppress_and_print(x)
}, bg = "transparent")
