# output$ui_multiparam_sort <- renderUI({
#   column(2,
#          bsButtonGroup("multiparam_sort", label = h5("Sorting"), value = "k", toggle = "radio", block = FALSE, vertical = FALSE, size = "mini",
#                        bsButton("btn_help_multiparam_sort", label = "?", value = "help"),
#                        bsButton("btn_multiparam_sort_j", label = strong("j"), value = "j"),
#                        bsButton("btn_multiparam_sort_k", label = strong("k"), value = "k")
#                        ),
#          uiOutput("help_modal_multiparam_sort")
#   )
# })


output$ui_multiparam_selectize <- renderUI({
  choices <- make_param_list_with_groups_sort()
  selected <- c(input$params_to_plot)
  column(7, selectizeInput("params_to_plot",
                           label = h5("Select or enter parameter names"),
                           width = '100%',
                           choices = choices,
                           multiple = TRUE))
})
