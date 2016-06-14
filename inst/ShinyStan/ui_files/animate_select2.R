fluidRow(
  column(
    width = 6, 
    sliderInput("animate_iters",strong_bl("Select iterations: "),
                min=1,max=.nIter,
                value=c(pmax(.nIter - 50, (.nIter - round(.nIter*.5))),.nIter),step=1,round=TRUE,width='100%')),
  column(
    width=2,
    checkboxInput("animate_standardize","Standardize X variables?",value=FALSE)
  )
)