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
  withMathJax(),
  h3(style = "color: #337ab7;","What is posterior predictive checking?"),
  p(strong("The idea behind posterior predictive checking is simple:")),
  p(style = "text-indent: 10px", 
    em("If our model is a good fit then we should be able to use it to generate")), 
  p(style = "text-indent: 10px", em("data that looks a lot like the data we observed.")),
  br(),
  p("To generate this 'replicated' data we use the", 
    em("posterior predictive distribution")),
  span(style = "color: #337ab7; font-face: bold;", withMathJax("$$ p(y^{rep} | y )  = \\int p(y^{rep} | \\theta) p(\\theta | y ) d \\theta,$$")),
  p("where \\(y\\) is the observed data and \\(\\theta\\) the parameters in our model."),
  br(),
  p("For each draw of \\(\\theta\\) from the posterior \\(p(\\theta | y) \\) 
      we simulate data \\(y^{rep}\\) from the posterior predictive distribution \\(p(y^{rep} | y) \\)."),
  br(),
  p("Using the simulations of \\(y^{rep}\\) we can make various
    graphical displays comparing our observed data to the replications."),
  hr(),
  helpText("For a more thorough discussion of posterior predictive checking see Chapter 6 of", a("BDA3.", href = "http://www.stat.columbia.edu/~gelman/book/"))
)
