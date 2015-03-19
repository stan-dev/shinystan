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



n_eff_warnings <- reactive({
  paste(.n_eff_warnings(fit_summary, threshold = input$n_eff_threshold), collapse = "\n")
})

rhat_warnings <- reactive({
  paste(.rhat_warnings(fit_summary, threshold = input$rhat_threshold), collapse = "\n")
})

mcse_over_sd_warnings <- reactive({
  paste(.mcse_over_sd_warnings(fit_summary, threshold = input$mcse_threshold), collapse = "\n")
})