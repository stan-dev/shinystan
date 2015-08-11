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

navlistPanel(well = FALSE, widths = c(2, 10),
             id = "diagnostics_help_navlist",
             tabPanel("accept_stat",
                      withMathJax(),
                      includeHTML("html/accept_stat.html"),
                      hr(), stan_manual()
             ),
             tabPanel("n_divergent",
                      withMathJax(),
                      includeHTML("html/ndivergent.html"),
                      hr(), stan_manual()
             ),
             tabPanel("stepsize",
                      withMathJax(),
                      includeHTML("html/stepsize.html"),
                      hr(), stan_manual()
             ),
             tabPanel("n_leapfrog",
                      withMathJax(),
                      includeHTML("html/nleapfrog.html"),
                      hr(), stan_manual()
             ),
             tabPanel("treedepth",
                      withMathJax(),
                      includeHTML("html/treedepth.html"),
                      hr(), stan_manual()
             ),
             tabPanel("NUTS",
                      withMathJax(),
                      includeHTML("html/nuts.html"),
                      hr(), stan_manual()
             )
)
