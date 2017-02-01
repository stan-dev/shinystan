# summary statistics for sampler parameters -------------------------------
summary_stats_sampler <- reactive({
  validate(
    need(STAN_ALGORITHM %in% c("NUTS", "HMC"), message = "Only available for algorithm = NUTS or HMC"),
    need(input$sampler_warmup, message = "Loading...")
  )
  sp <- if (input$sampler_warmup == "include")
    SAMPLER_PARAMS else SAMPLER_PARAMS_post_warmup
  
  do.call(
    ".sampler_summary",
    args = list(
      sampler_params  = sp,
      warmup_val      = N_WARMUP,
      report          = input$sampler_report,
      digits          = input$sampler_digits
    )
  )
})

output$sampler_summary <- DT::renderDataTable({
  DT::datatable({
    summary_stats_sampler()
  }, options = list(
    # rownames = FALSE,
    processing = TRUE,
    deferRender = TRUE,
    scrollX = TRUE,
    scrollY = "200px",
    scrollCollapse = TRUE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE
  ))
})
