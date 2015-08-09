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

div(class = "help-glossary-div", 
    br(),br(),
    div(class = "help-glossary-nav-container",
    navlistPanel(well = TRUE, id = "help_navlist",
                 "Topics",
                 tabPanel("Questions, bugs, and new features",
                          div(class = "glossary-entry",
                          h4("Stan users group"),
                          p("To ask a question or suggest a new feature visit the",
                            a("Stan users message board.", 
                              href = "https://groups.google.com/forum/?fromgroups#!forum/stan-users")),
                          br(),
                          h4("GitHub issue tracker"),
                          p("To report a bug  or suggest a new feature visit the",
                            a("GitHub issue tracker.", 
                              href = "https://github.com/stan-dev/shinystan/issues"))
                          )
                 ),             
                 tabPanel("Saving plots",
                          div(class = "glossary-entry",
                          h4("Saving plots as ggplot2 objects"),
                          p("Clicking on a 'Save ggplot2 object' button will be save an .RData 
                          file that you can load into your Global Environment using the",
                            code("load"), "function in R. 
             You can then make changes to the plot using the functions in the 
             ggplot2 package."
                          ),
                          p("Any plot that can be saved as a ggplot2 object can also be saved 
                            as a PDF.")
                          )
                 ),
                 tabPanel("Large models and launch speed",
                          div(class = "glossary-entry",
                          h4("Launching ShinyStan faster"),
                          p("For large models, the", code("launch_shinystan"), 
                            "function will launch the app quick when used with a",
                            "shinystan object (rather than a stanfit object)",
                            "because no conversion is required."),
                          p("If ShinyStan takes a long time to launch for your",
                            "model then it can help to first create a",
                            "shinystan object using the ", code("as.shinystan"),
                            "function. Alternatively, the first time you launch",
                            "ShinyStan using a stanfit object, a shinystan",
                            "object will be returned if you assign the value of",
                            code("launch_shinystan"), "to a name, e.g."),
                          p(code("sso <- launch_shinystan(my_stanfit)")),
                          p("rather than just"),
                          p(code("launch_shinystan(my_stanfit)")),
                          p("The next time you launch ShinyStan for the same",
                            "model you can launch it using", code("sso"),
                            "rather than", code("my_stanfit"), "and it should",
                            "be quicker to launch.")
                 )
                 )
    )
    ),
    br(),br()
)
