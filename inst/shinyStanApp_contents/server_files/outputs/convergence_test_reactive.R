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



convergence_test <- reactive({

  if (is.null(input$convergence_R) | is.null(input$convergence_thin)) {
    return()
  }

  validate(need((nIter %% input$convergence_thin) == 0,
                message = "Error: this value for 'Thin' leaves a remainder."))

  do.call(".convergence_test", args = list(
    session = session, # needed for progress bar
    object  = samps_all,
    R       = input$convergence_R,
    thin    = input$convergence_thin
  ))
})
