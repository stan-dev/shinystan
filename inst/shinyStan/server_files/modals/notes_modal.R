# This file is part of shinyStan
# Copyright (C) 2015 Jonah Sol Gabry & Stan Development Team
#
# shinyStan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinyStan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.



output$user_model_info_modal <- renderUI({
  shinyBS::bsModal("user_model_info_why_modal", title = "shinyStan help", trigger = "btn_user_model_info_why",
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
                          "e.g. 'It looks like the ratio of the effective sample size to the total number of iterations is very low for a lot of the parameters. 
Is there a different parameterization that might help?"
          )
          )
})



