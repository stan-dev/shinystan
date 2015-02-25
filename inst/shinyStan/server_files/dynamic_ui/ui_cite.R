output$ui_cite <- renderUI({
  bsModal("cite_shinystan", title = "Citing shinyStan", trigger = "citation_modal",      
  pre(
"@Misc{shinystan-software:2015,
title = {shinyStan: R Package for Interactive Exploration of Markov Chain Monte Carlo Output, Version 1.0.0},
author = {Jonah Gabry and shinyStan Team},
year = {2015},
abstract = {The shinyStan R package provides an interface (a Shiny application) for exploring Markov chain Monte Carlo output through interactive visualizations and tables.},
url = {https://mc-stan.org}
}"
  )
)
})