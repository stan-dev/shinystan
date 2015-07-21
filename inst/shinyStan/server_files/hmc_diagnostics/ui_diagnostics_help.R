output$ui_diagnostics_help <- renderUI({
  withMathJax()
  div(
    actionLink("btn_open_nuts_glossary_copy", "Open glossary", icon = icon("book", lib = "glyphicon")),
    uiOutput("nuts_glossary_modal_copy"),
    p(h3(style = "color: #337ab7;","Are my results valid?")),
    p(h4("Diagnostic")), 
    p("Check that N divergent = 0 for each iteration."),
    p(h4("Failed diagnostic?")),
    p("Use trace plots to identify where the sampler is diverging."), 
    p("Try rerunning the model with a higher target acceptance probability."),
    p(h3(style = "color: #337ab7;","Sampling is slow")),
    p(h4("Diagnostic")), 
    p("Make sure the tree depth is not saturating the maximum tree depth."),
    p(h4("Failed diagnostic?")),
    p("Try rerunning the model with a larger value for max_treedepth.")
  )
})