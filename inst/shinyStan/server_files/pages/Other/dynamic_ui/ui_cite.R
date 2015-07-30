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



output$ui_cite <- renderUI({
  bsModal("cite_shinystan", title = "Citing shinyStan", trigger = "citation_modal",      
  pre(
"@Misc{shinystan-software:2015,
title = {{shinyStan}: {R} Package for Interactive Exploration of {MCMC} samples, Version 1.1.0},
author = {Gabry, Jonah and Stan Development Team},
year = {2015},
abstract = {The shinyStan R package provides an interface (a Shiny application) for exploring Markov chain Monte Carlo output through interactive visualizations and tables.},
url = {https://mc-stan.org}
}"
  )
)
})