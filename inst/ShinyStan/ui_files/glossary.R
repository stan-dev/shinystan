div(
  class = "help-glossary-div",
  withMathJax(),
  br(),br(),
  div(
    withMathJax(),
    class = "help-glossary-nav-container",
    navlistPanel(
      well = TRUE,
      id = "glossary_navlist",
      tabPanel(
        "Effective sample size",
        includeHTML("html/neff.html")
      ),
      tabPanel(
        "Monte Carlo uncertainty",
        includeHTML("html/mcse.html")
      ),
      tabPanel(
        "Rhat",
        includeHTML("html/rhat.html")
      ),
      tabPanel(
        "No-U-Turn Sampler (NUTS)",
        includeHTML("html/nuts.html")
      ),
      tabPanel(
        "accept_stat",
        includeHTML("html/accept_stat.html")
      ),
      tabPanel(
        "divergent",
        withMathJax(),
        includeHTML("html/ndivergent.html")
      ),
      tabPanel(
        "stepsize",
        includeHTML("html/stepsize.html")
      ),
      tabPanel(
        "n_leapfrog",
        includeHTML("html/nleapfrog.html")
      ),
      tabPanel(
        "treedepth",
        includeHTML("html/treedepth.html")
      )
    )
  ),
  br(),br()
)
