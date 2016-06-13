fluidRow(
  column(
    width = 6, 
    sliderInput("animate_iters",strong_bl("Select iterations: "),
                min=1,max=.nIter,
                value=c(pmax(.nIter - 200, (.nIter - round(.nIter*.5))),nIter),step=1,round=TRUE)),
  column(
    width = 4,
    selectInput(
      "animate_color",
      label = strong_bl("x-axis"),
      choices = row.names(RColorBrewer::brewer.pal.info),
      selected = "Set1",
      multiple = FALSE
    )
  ),
  column(
    width=2,
    checkboxInput("animate_standardize","Standardize X variables?",value=FALSE)
  )
)