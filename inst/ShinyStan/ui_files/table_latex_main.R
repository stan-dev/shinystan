mainPanel(
  width = 8,
  actionButton(
    "tex_go",
    withMathJax("Update \\(\\LaTeX\\)"),
    icon = icon("print", lib = "glyphicon")
  ),
  br(), br(),
  verbatimTextOutput("summary_stats_latex_out")
)