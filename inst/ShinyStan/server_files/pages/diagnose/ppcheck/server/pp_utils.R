
# validate input tests ----------------------------------------------------
pp_tests <- reactive({
  validate(need(get_y(), message = "Waiting for y \n"), 
           need(get_yrep(), message = "Waiting for y_rep \n"))
})

# y_rep -------------------------------------------------------------------
get_yrep <- reactive({
  if (!is.null(pp_yrep)) 
    return(pp_yrep)
  else {
    validate(need(input$yrep_name, message = "Waiting for y_rep"))
    yreps <- grep(paste0("^",input$yrep_name,"\\["), param_names)
    out <- samps_post_warmup[,,yreps]
    dd <- dim(out)
    out <- array(out, dim = c(prod(dd[1:2]), dd[3]))
    return(out)
  }
})

get_y <- reactive({
  if (!is.null(pp_y)) return(pp_y)
  else {
    validate(need(input$y_name, message = "Waiting for y"))
    return(get(input$y_name))
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
