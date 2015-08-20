div(class = "help-glossary-div", 
    withMathJax(),
    br(),br(),
    div(class = "help-glossary-nav-container",
    navlistPanel(well = TRUE, id = "glossary_navlist",
                 tabPanel("Effective sample size",
                          withMathJax(),
                          includeHTML("html/neff.html")
                 ),
                 tabPanel("Monte Carlo uncertainty",
                          withMathJax(),
                          includeHTML("html/mcse.html")
                 ),
                 tabPanel("Rhat",
                          div(id = "rhat_glossary",
                          withMathJax(),
                          includeHTML("html/rhat.html")
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
