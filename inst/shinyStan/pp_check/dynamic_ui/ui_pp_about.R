output$ui_pp_about <- renderUI({
           div(
           h3("What is posterior predictive checking?"),
           p(strong("The idea behind posterior predictive checking is simple:")),
           p(style = "text-indent: 10px", 
             em("If our model is a good fit then we should be able to use it to generate")), 
           p(style = "text-indent: 10px", em("data that looks a lot like the data we observed.")),
           br(),
           p("To generate this 'replicated' data we use the", 
             em("posterior predictive distribution")),
           withMathJax("$$ p(y^{rep} | y )  = \\int p(y^{rep} | \\theta) p(\\theta | y ) d \\theta,$$"),
           p(withMathJax("where \\(y\\) is the observed data and \\(\\theta\\) the parameters in our model.")),
           br(),
           p("When we use a Markov chain Monte Carlo method", 
             withMathJax("to draw samples from the posterior distribution \\(p(\\theta | y)\\), 
                                                          for each of these samples we can simulate data \\(y^{rep}\\) from the posterior predictive distribution.")),
           br(),
           p(withMathJax("Using the simulations of \\(y^{rep}\\) we can make various
                                                          graphical displays comparing our observed data to the replications.")),
           hr(),
           helpText("Based on ideas presented in ", a("Bayesian Data Analysis, Third Edition", href = "http://www.crcpress.com/product/isbn/9781439840955"), "by Gelman et al.")
  )
})