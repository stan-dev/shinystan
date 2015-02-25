# modal windows for the glossary entries

# see below function for outputs

# function to output glossary  --------------------------------------------
make_glossary_modal_output <- function(id, trigger) {
  renderUI({
    bsModal(id, title = strong("shinyStan glossary"), trigger,
            withMathJax(),

            navlistPanel(well = FALSE,

                         tabPanel("n_eff",
                                  withMathJax(),
                                  p(h4("Effective sample size"),"\\(n_{\\rm eff}\\)"),
                                  p(
                                    "\\(n_{\\rm eff}\\) is an estimate of the effective number of independent
draws from the posterior distribution of the estimand of interest. Because the
samples within a chain are not independent if there is autocorrelation, the
number of effective samples will be smaller than the actual number of
simulations."
                                  ),
                                  p(
                                    "The estimation of effective sample size is described in detail in the 'Markov
Chain Monte Carlo Sampling' chapter of the ", a("Stan Modeling Language User's
Guide and Reference Manual", href = "http://mc-stan.org/manual.html"), "and in
Chapter 11 of ", a("BDA3.", href = "http://www.stat.columbia.edu/~gelman/book/"))
                         ),
                         tabPanel("Rhat",
                                  withMathJax(),
                                  p(h4("Rhat"), "\\(\\hat{R}\\)"),
                                  p("From", a("Stan Modeling Language User's Guide and Reference Manual", href = "http://mc-stan.org/manual.html"),":"),
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
                                    a("Stan Modeling Language User's Guide and Reference Manual", href = "http://mc-stan.org/manual.html"),
                                    "and in Chapter 11 of ", a("BDA3.", href = "http://www.stat.columbia.edu/~gelman/book/")
                                  )
                         ),
                         tabPanel("se_mean",
                                  p(h4("Monte Carlo uncertainty"), "\\(se_{\\rm mean}\\)"),
                                  p(
                                    "The standard error of the mean of the posterior samples (not to be confused
with the standard deviation of the posterior samples) is the uncertainty
associated with the Monte Carlo approximation. This quantity approaches 0 as
the number of samples goes to infinity, whereas the standard deviation of the
posterior samples approaches the standard deviation of the posterior
distribution."
                                  )
                         )
            )
    )
  })

}

# outputs -----------------------------------------------------------------
# make two outputs glossary_modal (for stats page), glossary_modal_copy (for
# rhat, neff, mcse plot page)
output$glossary_modal <- make_glossary_modal_output(id = "glossary", trigger="btn_open_glossary")
output$glossary_modal_copy <- make_glossary_modal_output(id = "glossary_copy", trigger="btn_open_glossary_copy")
