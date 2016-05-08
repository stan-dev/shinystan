div(
  withMathJax(),
  h3(style = "color: #337ab7;", "What is posterior predictive checking?"),
  p(
    strong("The idea behind posterior predictive checking is simple:")
  ),
  p(
    style = "text-indent: 10px",
    em("If our model is a good fit then we should be able to use it to generate")
  ),
  p(
    style = "text-indent: 10px", 
    em("data that looks a lot like the data we observed.")
  ),
  br(),
  p(
    "To generate this 'replicated' data we use the",
    em("posterior predictive distribution")
  ),
  span(
    style = "color: #337ab7; font-face: bold;",
    withMathJax(
      "$$ p(y^{rep} | y )  = \\int p(y^{rep} | \\theta) p(\\theta | y ) d \\theta,$$"
    )
  ),
  p(
    "where \\(y\\) is the observed data and \\(\\theta\\) the parameters in our model."
  ),
  br(),
  p(
    "For each draw of \\(\\theta\\) from the posterior \\(p(\\theta | y) \\)
    we simulate data \\(y^{rep}\\) from the posterior predictive distribution \\(p(y^{rep} | y) \\)."
  ),
  br(),
  p(
    "Using the simulations of \\(y^{rep}\\) we can make various
    graphical displays comparing our observed data to the replications."
  ),
  hr(),
  helpText(
    "For a more thorough discussion of posterior predictive checking see Chapter 6 of",
    a("BDA3.", href = "http://www.stat.columbia.edu/~gelman/book/")
  )
)
