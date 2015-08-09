div(class = "help-glossary-div", 
    withMathJax(),
    br(),br(),
    div(class = "help-glossary-nav-container",
    navlistPanel(well = TRUE, id = "glossary_navlist",
                 tabPanel("Effective sample size",
                          div(class = "glossary-entry",
                          p(h3("Effective sample size"), 
                            strong(style = "font-size: 14px;", "\\(n_{\\rm eff}\\), \\(ESS\\)")),
                          p(
                            "\\(n_{\\rm eff}\\) is an estimate of the effective number of independent
                            draws from the posterior distribution of the estimand of interest. Because the
                            draws within a chain are not independent if there is autocorrelation, the
                            effective sample size will be smaller than the total number of
                            iterations."
                          ),
                          p(
                            "The estimation of effective sample size is described in detail in the 'Markov
                            Chain Monte Carlo Sampling' chapter of the ", a("Stan Modeling Language User's
                                                                            Guide and Reference Manual", href = "http://mc-stan.org/documentation/"), "and in
                            Chapter 11 of ", a("BDA3.", href = "http://www.stat.columbia.edu/~gelman/book/"))
                 )
                 ),
                 tabPanel("Monte Carlo uncertainty",
                          div(class = "glossary-entry",
                          p(h3("Monte Carlo uncertainty"), strong(style = "font-size: 14px;", "\\(mcse\\)")),
                          p(
                            "The standard error of the mean of the posterior draws (not to be confused
with the standard deviation of the posterior draws) is the uncertainty
associated with the Monte Carlo approximation. This quantity approaches 0 as
the sample size goes to infinity, whereas the standard deviation of the posterior
draws approaches the standard deviation of the posterior distribution."
                          )
                          )
                 ),
                 tabPanel("Rhat",
                          withMathJax(),
                          div(class = "glossary-entry",
                          p(h3("Rhat"), strong(style = "font-size: 14px;", "\\(\\hat{R}\\)")),
                          p(
                            "One way to monitor whether a chain has converged to the equilibrium
                            distribution is to compare its behavior to other randomly initialized chains.
                            This is the motivation for the Gelman and Rubin potential scale reduction
                            statistic,", "\\(\\hat{R}\\)",". The", "\\(\\hat{R}\\)", "statistic measures
                            the ratio of the average variance of samples within each chain to the variance
                            of the pooled samples across chains; if all chains are at equilibrium, these
                            will be the same and Rˆ will be one. If the chains have not converged to a
                            common distribution, the", "\\(\\hat{R}\\)","statistic will be greater than one."
                          ),
                          p(
                            "Gelman and Rubin’s recommendation is that the independent Markov chains
                            be initialized with diffuse starting values for the parameters and sampled
                            until all values for", "\\(\\hat{R}\\)", "are below 1.1."
                          ),
                          p(
                            "Details on the computatation of the ", "\\(\\hat{R}\\)", "statistic
and some of its limitations can be found in the 'Markov Chain Monte Carlo Sampling' chapter of the ",
                            a("Stan Modeling Language User's Guide and Reference Manual", href = "http://mc-stan.org/documentation/"),
                            "and in Chapter 11 of ", a("BDA3.", href = "http://www.stat.columbia.edu/~gelman/book/")
                          )
                          )
                 ),
                 tabPanel("No-U-Turn Sampler (NUTS)",
                          withMathJax(),
                          includeHTML("html/nuts.html")
                 ),
                 tabPanel("accept_stat",
                          withMathJax(),
                          includeHTML("html/accept_stat.html")
                 ),
                 tabPanel("n_divergent",
                          withMathJax(),
                          includeHTML("html/ndivergent.html")
                 ),
                 tabPanel("stepsize",
                          withMathJax(),
                          includeHTML("html/stepsize.html")
                 ),
                 tabPanel("n_leapfrog",
                          withMathJax(),
                          includeHTML("html/nleapfrog.html")
                 ),
                 tabPanel("treedepth",
                          withMathJax(),
                          includeHTML("html/treedepth.html")
                 )
    )
    ),
    br(),br()
)
