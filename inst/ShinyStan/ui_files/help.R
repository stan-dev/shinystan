div(
  class = "help-glossary-div",
  br(), br(),
  div(
    class = "help-glossary-nav-container",
    navlistPanel(
      well = TRUE,
      id = "help_navlist",
      "Topics",
      tabPanel(
        "Questions, bugs, and new features",
        div(
          class = "glossary-entry",
          h4("Stan users group"),
          p(
            "To ask a question or suggest a new feature visit the",
            a(
              "Stan users message board.", 
              href = "https://groups.google.com/forum/?fromgroups#!forum/stan-users"
            )
          ),
          br(),
          h4("GitHub issue tracker"),
          p(
            "To report a bug  or suggest a new feature visit the",
            a(
              "GitHub issue tracker.", 
              href = "https://github.com/stan-dev/shinystan/issues"
            )
          )
        )
      ),
      tabPanel(
        "Saving plots",
        div(
          class = "glossary-entry",
          h4("Saving plots as ggplot2 objects"),
          p(
            "Clicking on a 'Save ggplot2 object' button will be save an .RData
            file that you can load into your Global Environment using the",
            code("load"),
            "function in R.
            You can then make changes to the plot using the functions in the
            ggplot2 package."
          ),
          p(
            "Any plot that can be saved as a ggplot2 object can also be saved
            as a PDF."
          )
      )), 
      tabPanel(
        "Large models and launch speed",
        div(
          class = "glossary-entry",
          h4("Launching ShinyStan faster"),
          p(
            "The", code("drop_parameters"), "function in the", 
            strong("shinystan"), "R package will allow you to reduce the size", 
            "of a shinystan object by removing parameters.", 
            "See", code("help('drop_parameters', 'shinystan')"), 
            "for the documentation."
          ),
          p(
            "Additionally, for large models, the", code("launch_shinystan"),
            "function will launch the app faster when used with a",
            "shinystan object rather than a stanfit object",
            "(because no conversion is required).",
            "If ShinyStan takes a long time to launch for your",
            "model then it can help to first create a",
            "shinystan object using the", code("as.shinystan"), "function.",
            "Alternatively, the first time you launch",
            "ShinyStan using a stanfit object, a shinystan",
            "object will be returned if you assign the value of",
            code("launch_shinystan"),
            "to a name, e.g."
          ),
          p(code("sso <- launch_shinystan(stanfit)")),
          p("rather than just"),
          p(code("launch_shinystan(stanfit)")),
          p(
            "The next time you launch ShinyStan for the same",
            "model you can launch it using", code("sso"), "rather than",
            code("stanfit"), "and it should be quicker to launch.",
            "If it is still too slow then dropping some large parameters", 
            "from the shinystan object is the best solution."
          )
        )
      )
    )
  ),
  br(), br()
)
