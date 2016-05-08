sidebarPanel(
  width = 4,
  h4(strong(
    withMathJax("\\(\\LaTeX\\) table generator")
  )),
  selectInput(
    "tex_params",
    width = "100%",
    label = "Parameters (blank = all)",
    multiple = TRUE,
    choices = .param_list_with_groups,
    selected = if (length(.param_names) == 1) .param_names else .param_names[1:2]
  ),
  numericInput(
    "tex_digits",
    label = "Digits",
    value = 1,
    min = 0
  ),
  div(
    style = "padding: 1px;",
    checkboxGroupInput(
      "tex_columns",
      label = "Columns",
      choices = c(
        "Rhat",
        "Effective sample size" = "n_eff",
        "Posterior mean" = "mean",
        "Posterior standard deviation" = "sd",
        "Monte Carlo error" = "se_mean",
        "Quantile: 2.5%" = "2.5%",
        "Quantile: 25%" = "25%",
        "Quantile: 50%" = "50%",
        "Quantile: 75%" = "75%",
        "Quantile: 97.5%" = "97.5%"
      ),
      selected = c("Rhat", "n_eff", "mean", "sd", "2.5%", "50%", "97.5%")
    )
  ),
  textInput("tex_caption", label = "Caption"),
  checkboxGroupInput(
    "tex_pkgs",
    "Packages",
    choices = c("Booktabs", "Longtable"),
    selected = "Booktabs",
    inline = TRUE
  ),
  br()
)
