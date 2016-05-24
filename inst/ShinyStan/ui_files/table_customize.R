fluidRow(
  column(
    width = 4,
    helpText(style = "margin-bottom: 2px;", "Table tips:"),
    helpText(style = "margin-top: 2px; font-size: 11px;", "Drag column names to rearrange the table columns.")
  ),
  column(
    width = 2,
    offset = 4,
    div(
      strong(id = "table_digits_txt", "Digits"),
      numericInput(
        "table_digits",
        label = NULL,
        value = 1,
        min = 0,
        max = 7,
        step = 1
      )
    )
  ),
  column(width = 2, a_glossary("open_glossary_from_table"))
)