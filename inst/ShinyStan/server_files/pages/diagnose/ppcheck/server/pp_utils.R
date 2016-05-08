
# validate input tests ----------------------------------------------------
pp_tests <- reactive({
  validate(
    need(
      get_y(), 
      message = "Waiting for y \n"
    ),
    need(
      get_yrep(), 
      message = "Waiting for y_rep \n"
    )
  )
})

# y -------------------------------------------------------------------
get_y <- reactive({
  if (!is.null(pp_y)) {
    return(pp_y)
  } else {
    validate(need(input$y_name, message = "Waiting for y"))
    y <- get(input$y_name)
    validate(
      need(
        !isTRUE(length(dim(y)) > 1), 
        message = "Error: y should be a vector"
      ),
      need(
        is.numeric(y), 
        message = "Error: y should be a numeric vector"
      )
    )
    return(y)
  }
})

# y_rep -------------------------------------------------------------------
has_yrep_name <- reactive({
  a <- input$yrep_name  # name selected from model parameters / generated quantities
  b <- input$yrep_name2  # name of object in global environment
  validate(need(a != "" || b != "", message = "Waiting for y_rep"))
  if (a != "" && b != "")
    validate(need(FALSE, message = "y_rep can only be specified once"))
  return(TRUE)
})
get_yrep <- reactive({
  if (!is.null(pp_yrep)) {
    return(pp_yrep)
  } else {
    validate(need(has_yrep_name(), message = "Waiting for y_rep"))
    if (input$yrep_name2 != "") {
      return(get(input$yrep_name2))
    } else {
      yreps <- grep(paste0("^", input$yrep_name, "\\["), PARAM_NAMES)
      out <- SAMPS_post_warmup[, , yreps]
      dd <- dim(out)
      validate(need(
        dd[3] == length(as.vector(get_y())), 
        message = "ncol(y_rep) should equal length(y)"
      ))
      out <- array(out, dim = c(prod(dd[1:2]), dd[3]))
      return(out)
    }
  }
})

# sample_ids_for_hist ------------------------------------------------------
nrow_yrep <- reactive({
  nrow(get_yrep())
})
sample_ids_for_hist <- reactive({
  go <- input$resample_hist_go          
  isolate(sample(nrow_yrep(), 8))
})
# sample_ids_for_dens ------------------------------------------------------
sample_ids_for_dens <- reactive({
  go <- input$resample_dens_go          
  isolate(sample(nrow_yrep(), min(nrow_yrep(), 50)))
})
# sample_id_for_resids ------------------------------------------------------
sample_id_for_resids <- reactive({
  go <- input$resample_resids_go          
  isolate(sample(nrow_yrep(), 1))
})
