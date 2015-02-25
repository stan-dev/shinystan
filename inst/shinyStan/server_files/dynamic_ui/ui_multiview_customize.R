ui_multiview_customize <- renderUI({
  absolutePanel(
    bottom = 150, right = 20, width = 240,
    draggable = TRUE,
    wellPanel(style = "background-color: #222222 ;",
              h4(style = "color: #428bca; ","shinyStan customize"),
              hr(),
              bsCollapse(open = "multiview_options_collapse",
                bsCollapsePanel(title = "Options", id = "multiview_options_collapse",
                                checkboxInput("multiview_warmup", label = "Include warmup", value = FALSE),
                                hr(),
                                downloadButton("download_multiview", "Save as ggplot2 object")
                )
              ),

    ),
    style = "opacity: 0.8; color: white;"
  )
})
