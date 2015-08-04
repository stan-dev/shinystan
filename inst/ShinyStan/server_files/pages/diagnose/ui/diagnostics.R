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



conditionalPanel_parameter <- function(...) {
  cond <- "input.diagnostics_navlist == 'By model parameter'"
  conditionalPanel(cond, ...)
}
dygraphOutput_175px <- function(id) {
  dygraphs::dygraphOutput(id, height = "175px")
}
plotOutput_200px <- function(id, ...) {
  plotOutput(id, height = "200px")
}
plotOutput_400px <- function(id, ...) {
  plotOutput(id, height = "400px")
}

hT11 <- function(...) helpText(style = "font-size: 11px;", ...)
help_interval <- hT11(
  "Highlighted interval shows \\(\\bar{x} \\pm sd(x)\\)")
help_lines <- hT11(
  "Lines are mean (solid) and median (dashed)")
help_max_td <- hT11(
  "Horizontal line indicates the max_treedepth setting")
help_points <- hT11(
  "Large red points indicate which (if any) iterations",
  "encountered a divergent transition. Yellow indicates",
  "a transition hitting the maximum treedepth.")  
help_dynamic <- hT11(
  "Use your mouse or the sliders to select areas in the",
  "traceplot to zoom into. The other plots on the screen", 
  "will update accordingly. Double-click to reset.")
# help_violin <- helpText("The violin plot ")
output$diagnostics_warnings_text <- renderText({
  divs <- sum(ndivergent_pw()[,-1])
  hits <- sum(treedepth_pw()[,-1] == MISC$max_td)
  d <- divs > 0
  h <- hits > 0
  if (d && h) msg <- paste("WARNINGS -- Diverging error:", divs, "iterations.",
                           "Maximum treedepth reached:", hits, "iterations.")
  else if (d && !h) msg <- paste("WARNINGS -- Diverging error:",
                                 divs, "iterations.")
  else if (!d && h) msg <- paste("WARNINGS -- Maximum treedepth reached:", 
                                 hits, "iterations.")
  else msg <- NULL
  msg
})

output$ui_diagnostics_customize <- renderUI({
  wellPanel(
    fluidRow(
      column(3, h4(textOutput("diagnostic_chain_text"))),
      column(4, conditionalPanel_parameter(h5("Parameter"))),
      column(4, conditionalPanel_parameter(h5("Transformation f(x) =")))
    ),
    fluidRow(
      column(3, div(style = "width: 100px;", 
                    numericInput("diagnostic_chain", label = NULL, value = 0, 
                                 min = 0, max = object@nChains))),
      column(4, conditionalPanel_parameter(selectizeInput(
        inputId = "diagnostic_param", label = NULL, multiple = FALSE, 
        choices = .make_param_list(object), 
        selected = .make_param_list(object)[1]))),
      column(3, conditionalPanel_parameter(
        textInput("diagnostic_param_transform", label = NULL, value = "x")
        )),
      column(2, conditionalPanel_parameter(
        actionButton("diagnostic_param_transform_go", "Transform")))
    ),
    helpText(strong(style = "color: red; font-size: 13px;", 
                    textOutput("diagnostics_warnings_text")))
  )
})

# model parameter ---------------------------------------------------------
output$ui_diagnostics_parameter <- renderUI({
  div(
    fluidRow(
      column(7, help_dynamic,
             dygraphOutput_175px("dynamic_trace_diagnostic_parameter_out")),
      column(5, help_lines, plotOutput_200px("p_hist_out"))
    ),
    help_points,
    fluidRow(
      column(6, plotOutput_200px("param_vs_lp_out"),
             plotOutput_200px("param_vs_stepsize_out")),
      column(6, plotOutput_200px("param_vs_accept_stat_out"),
             plotOutput_200px("param_vs_treedepth_out"))
    )
  )
})

# sample (accept_stat, lp) ------------------------------------------------
output$ui_diagnostics_sample <- renderUI({
  div(
    fluidRow(
      column(7,
             fluidRow(
               column(6, help_dynamic,
                      dygraphOutput_175px("dynamic_trace_diagnostic_lp_out"),
                      br(),
                      dygraphOutput_175px("dynamic_trace_diagnostic_accept_stat_out")),
               column(6, help_lines, plotOutput_200px("lp_hist_out"), br(),
                      plotOutput_200px("accept_stat_hist_out"))
             )
      ),
      column(5, help_points, 
             plotOutput_400px("accept_stat_vs_lp_out"))
    )
  )
})

# treedepth ---------------------------------------------------------------
output$ui_diagnostics_treedepth <- renderUI({
  div(
    fluidRow(
      column(7, help_dynamic,
             dygraphOutput_175px("dynamic_trace_diagnostic_treedepth_out"),
             br(),br(),
             plotOutput("treedepth_vs_lp_out", height = "150px")
      ),
      column(5, plotOutput_400px("treedepth_vs_accept_stat_out"))
    ),
    splitLayout( 
      plotOutput("treedepth_ndivergent_hist_out", height = "125px"),
      plotOutput("treedepth_ndivergent0_hist_out", height = "125px"),
      plotOutput("treedepth_ndivergent1_hist_out", height = "125px")
    ),
    br()
  )
})

# N divergent -------------------------------------------------------------
output$ui_diagnostics_ndivergent <- renderUI({
  fluidRow(
    column(7, #plotOutput_200px("ndivergent_trace_out"),
           help_dynamic,
           dygraphOutput_175px("dynamic_trace_diagnostic_ndivergent_out"), 
           br(),br(),
           plotOutput("ndivergent_vs_lp_out", height = "150px")),
    column(5, plotOutput_400px("ndivergent_vs_accept_stat_out"))
  )
})

# stepsize ----------------------------------------------------------------
output$ui_diagnostics_stepsize <- renderUI({
  fluidRow(
    column(7, help_dynamic,
           dygraphOutput_175px("dynamic_trace_diagnostic_stepsize_out"), 
           br(),br(),
           plotOutput("stepsize_vs_lp_out", height = "150px")),
    column(5, plotOutput_400px("stepsize_vs_accept_stat_out"))
  )
})
