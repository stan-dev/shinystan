#' Add mcmc_mcse plot for use in shinystan. Based on Bayesplot package 
#' mcmc_neff and mcmc_rhat.
#'
# monte carlo standard error -------------------------------------------
#' @noRd
mcmc_mcse_hist <- function(ratio, ..., binwidth = NULL, breaks = NULL) {
  check_ignored_arguments(...)
  data <- mcmc_mcse_data(ratio)
  
  ggplot(
    data,
    mapping = aes_(
      x = ~ value,
      color = ~ rating,
      fill = ~ rating)) +
    geom_histogram(
      size = .25,
      na.rm = TRUE,
      binwidth = binwidth,
      breaks = breaks) +
    scale_color_diagnostic("mcse") +
    scale_fill_diagnostic("mcse") +
    labs(x = expression(mcse/sd), y = NULL) +
    dont_expand_y_axis(c(0.005, 0)) +
    yaxis_title(FALSE) +
    yaxis_text(FALSE) +
    yaxis_ticks(FALSE) +
    bayesplot_theme_get()
}

#' @noRd
mcmc_mcse_data <- function(ratio, ...) {
  check_ignored_arguments(...)
  ratio <- drop_NAs_and_warn(new_mcse_ratio(ratio))
  diagnostic_data_frame(ratio)
}


# internal ----------------------------------------------------------------


#' Convert numeric vector of diagnostic values to a factor
#'
#' @param x A numeric vector.
#' @param breaks A numeric vector of length two. The resulting factor variable
#'   will have three levels ('low', 'ok', and 'high') corresponding to (
#'   `x <= breaks[1]`, `breaks[1] < x <= breaks[2]`, `x > breaks[2]`).
#' @return A factor the same length as `x` with three levels.
#' @noRd
diagnostic_factor <- function(x, breaks, ...) {
  UseMethod("diagnostic_factor")
}

diagnostic_factor.mcse_ratio <- function(x, breaks = c(0.05, 0.1)) {
  cut(x, breaks = c(-Inf, breaks, Inf),
      labels = c("low", "ok", "high"),
      ordered_result = FALSE)
}

diagnostic_data_frame <- function(x) {
  # x <- auto_name(sort(x))
    # quick fix getting right diagnostic factor because of class drop
    # at auto_name(sort(x))
  x <- structure(auto_name(sort(x)), class = c("mcse_ratio", "numeric"))
  stopifnot(!anyDuplicated(names(x)))
  diagnostic <- class(x)[1]
  
  d <- data.frame(
    diagnostic = diagnostic,
    parameter = factor(seq_along(x), labels = names(x)),
    value = as.numeric(x),
    rating = diagnostic_factor(x))
  
  labels <- diagnostic_color_labels[[diagnostic]]
  d$description <- as.character(labels[d$rating])
  d
}

auto_name <- function(xs) {
  if (is.null(names(xs))) {
    names(xs) <- zero_pad_int(seq_along(xs))
  }
  xs
}

# c(1, 2, 10, 20, 100) => c("001", "002", "010", "020", "100")
zero_pad_int <- function(xs) {
  formatter <- paste0("%0", max(nchar(xs)), "d")
  sprintf(formatter, xs)
}

diagnostic_points <- function(size = NULL) {
  args <- list(shape = 21, na.rm = TRUE)
  do.call("geom_point", c(args, size = size))
}


# Functions wrapping around scale_color_manual() and scale_fill_manual(), used to
# color the intervals by rhat value
scale_color_diagnostic <- function(diagnostic = c("rhat", "neff", "mcse")) {
  d <- match.arg(diagnostic)
  diagnostic_color_scale(d, aesthetic = "color")
}

scale_fill_diagnostic <- function(diagnostic = c("rhat", "neff", "mcse")) {
  d <- match.arg(diagnostic)
  diagnostic_color_scale(d, aesthetic = "fill")
}

diagnostic_color_scale <- function(diagnostic = c("rhat", "neff_ratio", "mcse_ratio"),
                                   aesthetic = c("color", "fill")) {
  diagnostic <- match.arg(diagnostic)
  aesthetic <- match.arg(aesthetic)
  dc <- diagnostic_colors(diagnostic, aesthetic)
  do.call(
    match.fun(paste0("scale_", aesthetic, "_manual")),
    list(
      name = NULL,
      drop = FALSE,
      values = dc$values,
      labels = dc$color_labels
    )
  )
}

diagnostic_colors <- function(diagnostic = c("rhat", "neff_ratio", "mcse_ratio"),
                              aesthetic = c("color", "fill")) {
  diagnostic <- match.arg(diagnostic)
  aesthetic <- match.arg(aesthetic)
  color_levels <- c("light", "mid", "dark")
  if (diagnostic == "neff_ratio") {
    color_levels <- rev(color_levels)
  }
  if (diagnostic == "mcse_ratio") {
    color_levels <- color_levels
  }
  if (aesthetic == "color") {
    color_levels <- paste0(color_levels, "_highlight")
  }
  
  color_labels <- diagnostic_color_labels[[diagnostic]]
  
  list(diagnostic = diagnostic,
       aesthetic = aesthetic,
       color_levels = color_levels,
       color_labels = color_labels,
       values = rlang::set_names(get_color(color_levels), c("low", "ok", "high")))
}

diagnostic_color_labels <- list(
  rhat = c(
    low  = expression(hat(R) <= 1.05),
    ok   = expression(hat(R) <= 1.10),
    high = expression(hat(R) > 1.10)
  ),
  neff_ratio = c(
    low  = expression(N[eff] / N <= 0.1),
    ok   = expression(N[eff] / N <= 0.5),
    high = expression(N[eff] / N > 0.5)
  ),
  mcse_ratio = c(
    low  = expression(mcse / sd <= 0.05),
    ok   = expression(mcse / sd <= 0.1),
    high = expression(mcse / sd > 0.1)
  )
)

# drop NAs from a vector and issue warning
drop_NAs_and_warn <- function(x) {
  is_NA <- is.na(x)
  if (anyNA(x)) {
    rlang::warn(paste0(
      "Dropped ", sum(is_NA), " NAs from '",
      deparse(substitute(x)), "'."
    ))
  }
  x[!is_NA]
}


#' Indexing method -- needed so that sort, etc. don't strip names.
#' @noRd
new_mcse_ratio <- function(x) {
  # Convert a 1-d arrays to a vectors
  if (is.array(x) && length(dim(x)) == 1) {
    x <- as.vector(x)
  }
  as_mcse_ratio(validate_mcse_ratio(x))
}

validate_mcse_ratio <- function(x) {
  stopifnot(is.numeric(x), !is.list(x), !is.array(x))
  if (any(x < 0, na.rm = TRUE)) {
    rlang::abort("All mcse ratios must be positive.")
  }
  x
}

as_mcse_ratio <- function(x) {
  structure(x, class = c("mcse_ratio", "numeric"), names = names(x))
}

#' Indexing method -- needed so that sort, etc. don't strip names.
#' @noRd
`[.mcse_ratio` <- function (x, i, j, drop = TRUE, ...) {
  as_mcse_ratio(NextMethod())
}



# Check for ignored arguments
#' @noRd
check_ignored_arguments <- function(..., ok_args = character()) {
  dots <- list(...)
  if (length(dots)) {
    unrecognized <- if (!length(ok_args))
      names(dots) else setdiff(names(dots), ok_args)
    if (length(unrecognized)) {
      rlang::warn(paste(
        "The following arguments were unrecognized and ignored:",
        paste(unrecognized, collapse = ", ")
      ))
    }
  }
}


get_color <- function(levels) {
  levels <- full_level_name(levels)
  stopifnot(all(levels %in% scheme_level_names()))
  color_vals <- color_scheme_get()[levels]
  unlist(color_vals, use.names = FALSE)
}

full_level_name <- function(x) {
  map <- c(
    l = "light",
    lh = "light_highlight",
    m = "mid",
    mh = "mid_highlight",
    d = "dark",
    dh = "dark_highlight",
    light = "light",
    light_highlight = "light_highlight",
    mid = "mid",
    mid_highlight = "mid_highlight",
    dark = "dark",
    dark_highlight = "dark_highlight"
  )
  unname(map[x])
}

scheme_level_names <- function() {
  c("light",
    "light_highlight",
    "mid",
    "mid_highlight",
    "dark",
    "dark_highlight")
}

dont_expand_y_axis <- function(expand = c(0,0)) {
  scale_y_continuous(expand = expand)
}