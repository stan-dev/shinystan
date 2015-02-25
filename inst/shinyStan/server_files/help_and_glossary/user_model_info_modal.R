output$user_model_info_modal <- renderUI({
  bsModal("user_model_info_why_modal", title = "shinyStan help", trigger = "btn_user_model_info_why",
          h4("Why use the 'Notes' feature?"),
          p(
            "If you want to allow other users to explore your model with
            shinyStan, you can send them your shinystan object (e.g. as an .RData file) 
            and they will see any notes you've saved."
          ),
          p(
            "For example, you might want to add comments about the names
of the parameters or clarifications about other choices you made"
          ),
          tags$blockquote(style = "font-size: 14px;", 
                          "e.g., 'theta is the estimated treatment effect', 'I reparameterized
the beta distribution in terms of the mean and sample size,' etc."),
          p(
            "or highlight aspects of the model you want feedback on"
          ),
          tags$blockquote(style = "font-size: 14px;", 
                          "e.g. 'It looks like the ratio of effective sample size to total sample size is very low for a lot of the parameters. 
Is there a different parameterization that might help?"
          )
          )
})



