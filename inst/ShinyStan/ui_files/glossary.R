shinyjs::hidden(
  div(id = "glossary_div", 
      style = "padding-top: 10px;",
      withMathJax(),
      h1(style = "text-align: center;", "Glossary"),
      br(),
      div(
        navlistPanel(well = TRUE, id = "glossary_navlist",
                     "Glossary Entries",
                     tabPanel("Effective sample size",
                              p(h4("Effective sample size"),"\\(n_{\\rm eff}\\)"),
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
                     ),
                     tabPanel("Monte Carlo uncertainty",
                              p(h4("Monte Carlo uncertainty"), "\\(mcse\\)"),
                              p(
                                "The standard error of the mean of the posterior draws (not to be confused
with the standard deviation of the posterior draws) is the uncertainty
associated with the Monte Carlo approximation. This quantity approaches 0 as
the sample size goes to infinity, whereas the standard deviation of the posterior
draws approaches the standard deviation of the posterior distribution."
                              )
                     ),
                     tabPanel("Rhat",
                              withMathJax(),
                              p(h4("Rhat"), "\\(\\hat{R}\\)"),
                              p(em("From", a("Stan Modeling Language User's Guide and Reference Manual", href = "http://mc-stan.org/documentation/")),":"),
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
                              hr(),
                              p(
                                "Details on the computatation of the ", "\\(\\hat{R}\\)", "statistic
and some of its limitations can be found in the 'Markov Chain Monte Carlo Sampling' chapter of the ",
                                a("Stan Modeling Language User's Guide and Reference Manual", href = "http://mc-stan.org/documentation/"),
                                "and in Chapter 11 of ", a("BDA3.", href = "http://www.stat.columbia.edu/~gelman/book/")
                              )
                     ),
                     
                     "HMC/NUTS",
                     tabPanel("accept_stat",
                              p(h4("accept_stat")),
                              br(),
                              algorithm_nuts,
                              p("The Metropolis acceptance probability averaged over samples in the slice."),
                              br(),
                              algorithm_hmc,
                              p("The Metropolis acceptance probability.")
                     ),
                     tabPanel("int_time",
                              p(h4("int_time")),
                              br(),
                              algorithm_nuts,
                              p("Not applicable."),
                              br(),
                              algorithm_hmc,
                              p("Total integration time.")
                     ),
                     tabPanel("n_divergent",
                              p(h4("n_divergent")),
                              br(),
                              algorithm_nuts,
                              p("The number of leapfrog iterations with diverging error;
                                because NUTS terminates at the first divergent iteration this should always
                                be either 0 or 1."),
                              p("The average value of n_divergent is therefore the proportion of iterations
                                with diverging error."),
                              br(),
                              algorithm_hmc,
                              p("Not applicable.")
                     ),
                     tabPanel("n_leapfrog",
                              p(h4("n_leapfrog")),
                              br(),
                              algorithm_nuts,
                              p("The number of leapfrog steps (calculations) taken during the Hamiltonian simulation."),
                              br(),
                              algorithm_hmc,
                              p("Not applicable.")
                     ),
                     tabPanel("step_size",
                              p(h4("step_size")),
                              br(),
                              algorithm_nuts,
                              p("The integrator step size used in the Hamiltonian simulation."),
                              br(),
                              algorithm_hmc,
                              p("The integrator step size used in the Hamiltonian simulation.")
                     ),
                     tabPanel("tree_depth",
                              withMathJax(),
                              p(h4("tree_depth")),
                              br(),
                              algorithm_nuts,
                              p("The depth of tree used by NUTS, equal to \\(\\log_2{(\\text{n_leapfrog})}\\)."),
                              br(),
                              algorithm_hmc,
                              p("Not applicable.")
                     )
        )
      )
  )
)
