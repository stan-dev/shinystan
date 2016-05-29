source_ui <- function(...) {
  source(
    file.path("ui_files", ...), 
    local = TRUE
  )$value
}

save_and_close_button <- function() {
  tags$button(
    id = 'save_and_close_button',
    type = "button",
    class = "btn action-button",
    onclick = "window.close();",
    "Save & Close"
  )
}

shinystan_version <- function() {
  # prevents error when deployed to shinyapps.io
  ver <- try(utils::packageVersion("shinystan"))
  if (inherits(ver, "try-error"))
    return()
  else
    strong(paste("Version", ver))
}

logo_and_name <- function() {
  div(div(
    img(
      src = "wide_ensemble.png",
      class = "wide-ensemble",
      width = "100%"
    )
  ),
  div(
    style = "margin-top: 25px",
    img(src = "stan_logo.png", class = "stan-logo"),
    div(id = "shinystan-title", "ShinyStan")
  ))
}


# save and close reminder -------------------------------------------------
save_and_close_reminder <- function(id) {
  helpText(
    id = id,
    p(
      "To make sure the changes aren't lost, use the",
      span(class = "save-close-reminder", "Save & Close"),
      "button in the top left corner to exit the app before",
      "closing the browser window."
    )
  )
}


# show/hide options/glossary ---------------------------------------------
a_options <- function(name) {
  lab <- if (name == "table")
    "Table Options" else "Show/Hide Options"
  div(class = "aoptions",
      checkboxInput(
        inputId = paste0(name, "_options_show"),
        label = strong(style = "margin-top: 20px; color: #222222;", lab),
        value = FALSE
      ))
}
a_glossary <- function(id) {
  div(class = "aoptions",
      actionLink(
        inputId = id,
        label = strong(style = "margin-top: 20px; color: #222222;", "Glossary"),
        icon = icon("book", lib = "glyphicon")
      ))
}



# plotOutput generators ---------------------------------------------------
dygraphOutput_175px <- function(id) 
  dygraphs::dygraphOutput(id, height = "175px")
plotOutput_200px <- function(id, ...) 
  plotOutput(id, height = "200px")
plotOutput_400px <- function(id, ...) 
  plotOutput(id, height = "400px")



# conditionalPanel generator for EXPLORE/density  -------------------------
condPanel_dens_together <- function(...) {
  conditionalPanel(condition = "input.dens_chain_split == 'Together'", ...)
}
condPanel_dens_prior <- function(dist, ...) {
  cond <- paste0("input.dens_prior ==","'", dist,"'")
  conditionalPanel(cond, ...)
}


# conditional transparency settings ---------------------------------------
alpha_calc_pt <- function(N) {
  if (N <= 100) return(1)
  else if (N <= 200) return(0.75)
  else if (N >= 1500) return(0.15) 
  else 1 - pnorm(N/1500)
}

alpha_calc_lines <- function(N) {
  if (N < 50) return(0.5)
  if (N < 500) return(0.4)
  if (N < 1000) return(0.3)
  if (N < 5000) return(0.2)
  else return(0.1)
}


# transformations ---------------------------------------------------------
transformation_selectInput <- function(id) {
  selectInput(
    id,
    label = NULL,
    choices = transformation_choices,
    selected = "identity"
  )
}

transform_helpText <- function(var = "x") {
  div(
    if (var == "x") 
      helpText(style = "font-size: 13px;", 
               "To apply a transformation",
               "select a function and click", 
               code("Transform"))
    else if (var == "x,y")
      helpText(style = "font-size: 13px;", 
               "To apply transformations",
               "select a function for x and/or y", 
               "and click", code("Transform"))
    else 
      helpText(style = "font-size: 13px;", 
               "To apply transformations",
               "select a function for x, y, and/or z", 
               "and click", code("Transform"))
  )
}


# diagnostics help text ---------------------------------------------------
hT11 <- function(...)
  helpText(style = "font-size: 11px;", ...)
help_interval <- hT11("Highlighted interval shows \\(\\bar{x} \\pm sd(x)\\)")
help_lines <- hT11("Lines are mean (solid) and median (dashed)")
help_max_td <- hT11("Horizontal line indicates the max_treedepth setting")
help_points <- hT11(
  "Large red points indicate which (if any) iterations",
  "encountered a divergent transition. Yellow indicates",
  "a transition hitting the maximum treedepth."
)
help_dynamic <- hT11(
  "Use your mouse or the sliders to select areas in the",
  "traceplot to zoom into. The other plots on the screen",
  "will update accordingly. Double-click to reset."
)



# ppcheck plot descriptions ----------------------------------------------
plot_descriptions <-
  c(
    plot_hists_rep_vs_obs = "Distributions of observed data and a random sample of replications",
    plot_dens_rep_vs_obs = "Density estimate of observed data (blue) and a random sample of replications",
    plot_obs_vs_avg_y_rep = "Observations vs average simulated value",
    plot_hist_resids = "Residuals",
    plot_avg_rep_vs_avg_resid_rep = "Average simulated value vs average residual",
    plot_test_statistics = "Distributions of test statistics \\(T(y^{rep})\\)"
  )



# stan manual reference ---------------------------------------------------
stan_manual <- function() {
  helpText(
    style = "font-size: 12px;",
    "Glossary entries are compiled (with minor edits) from various excerpts of the",
    a(
      "Stan Modeling Language User's Guide and Reference Manual",
      href = "http://mc-stan.org/documentation/"
    ),
    "(",
    a(href = "http://creativecommons.org/licenses/by/3.0/", "CC BY (v3)"),
    ")"
  )
}


# objects to use in ui.R and ui_files -------------------------------------
if (!exists(".SHINYSTAN_OBJECT")) {
  .SHINYSTAN_OBJECT <- shinystan:::.sso_env[[".SHINYSTAN_OBJECT"]]
}
.model_name <- slot(.SHINYSTAN_OBJECT, "model_name")
.param_names <- slot(.SHINYSTAN_OBJECT, "param_names")
.param_list <- .make_param_list(.SHINYSTAN_OBJECT)
.param_list_with_groups <- .make_param_list_with_groups(.SHINYSTAN_OBJECT)
.nChains <- slot(.SHINYSTAN_OBJECT, "n_chain")
.nIter <- slot(.SHINYSTAN_OBJECT, "n_iter")
.nWarmup <- slot(.SHINYSTAN_OBJECT, "n_warmup")
.model_code <- slot(.SHINYSTAN_OBJECT, "model_code")
.notes <- slot(.SHINYSTAN_OBJECT, "user_model_info")
.has_rstanarm_ppcs <-
  isTRUE(.SHINYSTAN_OBJECT@misc$stanreg) &&
  !is.null(.SHINYSTAN_OBJECT@misc$pp_check_plots)

if (exists("object"))
  rm(object)
if (exists(".SHINYSTAN_OBJECT"))
  rm(.SHINYSTAN_OBJECT)
gc()
