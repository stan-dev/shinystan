# modal windows for the sampler param glossary entries
output$glossary_modal_nuts <- renderUI({

  algorithm_nuts <- h5(style = "color: #428bca;", "algorithm = NUTS")
  algorithm_hmc <- h5(style = "color: #428bca;", "algorithm = HMC")

  bsModal("glossary_nuts", "shinyStan glossary", trigger = "btn_open_glossary_nuts",
          withMathJax(),

          navlistPanel(well = FALSE,

                       tabPanel("accept_stat",
                                p(h4("accept_stat")),
                                br(),
                                algorithm_nuts,
                                p("The Metropolis acceptance probability averaged over samples in the slice."),
                                br(),
                                algorithm_hmc,
                                p("The Metropolis acceptance probability.")
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
                       ),
                       tabPanel("int_time",
                                p(h4("int_time")),
                                br(),
                                algorithm_nuts,
                                p("Not applicable."),
                                br(),
                                algorithm_hmc,
                                p("Total integration time.")
                       )
          )
  )
})
