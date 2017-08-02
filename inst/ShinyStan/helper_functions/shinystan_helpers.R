# param_trace_multi ------------------------------------------------------
# trace plots for multiple parameters
.param_trace_multi <- function(params = NULL, all_param_names, dat,
                               warmup_val = 0,
                               chain = 0, palette = "Default",
                               rect = "Samples", rect_color = "skyblue",
                               rect_alpha = 0.1,
                               layout = "Long", x1, x2) {


  params <- .update_params_with_groups(params, all_param_names)
  if(length(params) == 0) {
    params <- dimnames(dat)$parameters[1:min(4, dim(dat)[3])]
  }
  params <- unique(params)
  dat <- reshape2::melt(dat[,,params, drop=FALSE])

  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
  }
  dat$iterations <- x1:x2
  if (chain != 0)
    dat <- subset(dat, chains == paste0("chain:",chain))
  rect_xmin <- ifelse(rect == "Samples", Inf, -Inf)
  shading_rect <- annotate("rect", xmin = rect_xmin, xmax = warmup_val,
                           ymin = -Inf, ymax = Inf, fill = rect_color, alpha = rect_alpha)

  xy_labs <- labs(y = "Value", x = "Iteration")
  nclrs <- length(unique(dat$chains))

  lgnd_title <- ""
  if(palette == "Default") clrs <- scale_color_discrete(name = lgnd_title)
  if(palette == "Gray") clrs <- scale_color_grey(name = lgnd_title)
  if(palette == "Brewer (spectral)") clrs <- scale_color_brewer(name = lgnd_title, palette = "Spectral")
  if(palette == "Rainbow") clrs <- scale_colour_manual(name = lgnd_title, values = rainbow(nclrs))

  lgnd_txt <- theme(legend.text =  element_text(size = 13, face = "bold"))

  graph <- ggplot(dat, aes(x = iterations, y = value, color = chains))
  graph <- graph + xy_labs + clrs + theme_classic() %+replace% (axis_color + axis_labs + fat_axis + h_lines + lgnd_top + lgnd_txt + strip_txt + transparent)
  if (rect != "None") graph <- graph + shading_rect
  graph <- graph + geom_line(size = 0.35) + scale_x_continuous(limits = c(x1, x2))

  if (layout == "Grid") {
    graph <- graph + facet_wrap(~ parameters, scales = "free_y")
  } else {
    graph <- graph + facet_grid(parameters ~., scales = "free_y")
  }

  graph
}


# param_hist --------------------------------------------------------------
# histogram for a single parameter
.param_hist <- function(param, dat, chain, binwd,
                        transform_x = "identity",
                        fill_color = "gray20", line_color = "gray35",
                        title = TRUE) {

  ttl <- "Histogram of Posterior Draws \n"
  if (transform_x != "identity") {
    t_x <- get(transform_x)
    dat <- apply(dat, 2, t_x)
  }
  x_lab <- if (transform_x != "identity")
    paste0(transform_x, "(", param, ")") else param

  dat <- reshape2::melt(dat)
  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }
  if (chain != 0) dat <- subset(dat, chains == paste0("chain:",chain))

  graph <- ggplot(dat, aes(x = value))

  if (binwd == 0) {
    graph <- graph + geom_histogram(fill = fill_color, color = line_color, size = 0.2)
  } else {
    graph <- graph + geom_histogram(fill = fill_color, color = line_color,
                                    binwidth = binwd, size = 0.2)
  }
  graph <- graph +
    labs(x = x_lab, y = "") +
    theme_classic() %+replace%
    (title_txt + axis_color + axis_labs + fat_axis + no_yaxs + transparent)
  if (title == TRUE) graph <- graph + ggtitle(ttl)

  graph
}


# param_dens --------------------------------------------------------------
# density plot for a single parameter

# data.frame of prior families and function names
priors <- data.frame(family = c("Normal", "t", "Cauchy", "Beta", "Exponential",
                                "Gamma", "Inverse Gamma"),
                     fun = c("dnorm", ".dt_loc_scale", "dcauchy", "dbeta",
                             "dexp", "dgamma", ".dinversegamma"))

.param_dens <- function(param, dat, chain,
                        fill_color = NULL, line_color = NULL,
                        point_est = "None", CI,
                        x_breaks = "Some", # y_breaks = "None",
                        x_lim = NULL,
                        chain_split = FALSE,
                        title = TRUE,
                        transform_x = "identity",
                        prior_fam = "None", prior_params) {

  ttl <- "Kernel Density Estimate \n"
  if (transform_x != "identity") {
    t_x <- get(transform_x)
    dat <- apply(dat, 2, t_x)
  }
  x_lab <- if (transform_x != "identity")
    paste0(transform_x, "(", param, ")") else param

  dat <- reshape2::melt(dat)
  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }
  if (chain != 0)
    dat <- subset(dat, chains == paste0("chain:",chain))

  Mean <- mean(dat$value)
  Median <- median(dat$value)
  dens_dat <- with(density(dat$value), data.frame(x,y))
  MAP <- with(dens_dat, x[which.max(y)])

  fclr <- ifelse(is.null(fill_color), "black", fill_color)
  lclr <- ifelse(is.null(line_color), "lightgray", line_color)

  many_breaks <- function(x) pretty(x, n = 15)
  too_many_breaks <- function(x) pretty(x, n = 35)
  if(x_breaks == "None") x_scale <- scale_x_continuous(breaks = NULL)
  if(x_breaks == "Some") x_scale <- scale_x_continuous()
  if(x_breaks == "Many") x_scale <- scale_x_continuous(breaks = many_breaks)

  if (chain == 0 & chain_split == TRUE) {
    graph <- ggplot(dat, aes(x = value, color = chains, fill = chains))
    if (prior_fam != "None") {
      graph <- graph + stat_function(alpha=0.75,color = "black", fun = as.character(priors$fun[priors$family==prior_fam]), args = prior_params, show_guides = TRUE)
    }
    graph <- graph +
      geom_density(alpha = 0.15) +
      scale_color_discrete("") +
      scale_fill_discrete("") +
      labs(x = x_lab, y = "") +
      x_scale + # y_scale +
      theme_classic() %+replace% (title_txt + axis_color + axis_labs + fat_axis + no_yaxs + transparent)

    if (title == TRUE) graph <- graph + ggtitle(ttl)
    return(graph)
  }

  graph <- ggplot(dens_dat, aes(x = x, ymax = y))
  if (prior_fam != "None") {
    graph <- graph +
      stat_function(fun = as.character(priors$fun[priors$family==prior_fam]),
                    args = prior_params)
  }
  graph <- graph +
    labs(x = param, y = "") +
    x_scale + # y_scale +
    labs(x = x_lab, y = "") +
    geom_ribbon(ymin = 0, fill = fclr, color = lclr, alpha = if (prior_fam == "None") 1 else 0.85) +
    theme_classic() %+replace% (title_txt + axis_color + axis_labs + fat_axis + no_yaxs + transparent)

  if (title == TRUE) graph <- graph + ggtitle(ttl)
  if (point_est != "None") {
    graph <- graph + annotate("segment",
                              x = get(point_est), xend = get(point_est),
                              y = 0, yend = max(dens_dat$y),
                              color = lclr, lwd = 1, lty = 2)
  }
  if (CI != "None") {
    lev <- (1 - as.numeric(CI))/2
    quant <- quantile(dat$value, probs = c(lev, 1 - lev))
    graph <- (graph +
                annotate("segment", x = quant, xend = quant, y = 0, yend = max(dens_dat$y), color = lclr, lty = rep(1:length(CI),2))
    )
  }
  if (!is.null(x_lim)) graph <- graph + scale_x_continuous(limits = x_lim)
  graph
}


# autocorr ----------------------------------------------------
.ac_fun <- function(x, lag.max, partial = FALSE) {
  if (!partial)
    acf(x, lag.max = lag.max, plot = FALSE)$acf[,, 1L]
  else
    pacf(x, lag.max = lag.max, plot = FALSE)$acf[,, 1L]
}
.ac_plot_data <- function(dat, lags, partial = FALSE) {
  nc <- length(unique(dat$chains))
  ac_list <- tapply(dat$value, INDEX = dat$chains, FUN = .ac_fun, lag.max = lags,
                    partial = partial, simplify = FALSE)
  nl <- if (partial) lags else lags + 1
  ch <- factor(rep(1:nc, each = nl), labels = paste0("chain:", 1:nc))
  ll <- rep(seq(if (partial) 1 else 0, lags), nc)
  data.frame(chains = ch, ac = do.call("c", args = ac_list), lag = ll)
}
.ac_plot_data_multi <- function(dat, lags, partial = FALSE) {
  nc <- length(unique(dat$chains))
  np <- length(unique(dat$parameters))
  ac_list <- tapply(dat$value, INDEX = list(dat$chains, dat$parameters),
                    FUN = .ac_fun, lag.max = lags,
                    partial = partial, simplify = FALSE)
  nl <- if (partial) lags else lags + 1
  ch <- factor(rep(rep(1:nc, each = nl), np), labels = paste0("chain:", 1:nc))
  ll <- rep(seq(if (partial) 1 else 0, lags), nc * np)
  pp <- factor(rep(1:np, each = nc * nl), labels = levels(dat$parameters))
  data.frame(parameters = pp, chains = ch, ac = do.call("c", args = ac_list), lag = ll)
}

# markov chain autocorrelation plot for single parameters
.autocorr_single_plot <- function(samps, lags) {
  dat <- reshape2::melt(samps)
  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }
  ac_dat <- .ac_plot_data(dat, lags)
  ac_labs <- labs(x = "Lag", y = "Autocorrelation")
  ac_theme <- theme_classic() %+replace%
    (axis_color + axis_labs + fat_axis + no_lgnd + transparent)
  y_scale <- scale_y_continuous(breaks = seq(0, 1, 0.25),
                                labels = c("0","","0.5","",""))
  graph <- ggplot(ac_dat, aes(x = lag, y = ac))
  graph <- graph +
    geom_bar(position = "identity", stat = "identity", fill = base_fill) +
    y_scale + ac_theme
  graph
}

# markov chain autocorrelation plot for multiple parameters
.autocorr_plot <- function(samps, partial = FALSE,
                           lags = 25, flip = FALSE,
                           combine_chains = FALSE) {

  params <- dimnames(samps)$parameters
  nParams <- length(params)
  nChains <- dim(samps)[2L]
  dat <- reshape2::melt(samps)
  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }
  ac_type <- if (partial) "partial" else "correlation"
  if (nParams == 1) ac_dat <- .ac_plot_data(dat, lags = lags, partial = partial)
  else ac_dat <- .ac_plot_data_multi(dat, lags = lags, partial = partial)

  ac_labs <- labs(x = "Lag", y = if (partial)
    "Partial autocorrelation" else "Autocorrelation")
  ac_theme <- theme_classic() %+replace%
    (axis_color + axis_labs + fat_axis + no_lgnd + strip_txt + transparent)
  y_scale <- scale_y_continuous(breaks = seq(0, 1, 0.25),
                                labels = c("0","","0.5","",""))
  title_theme <- theme(plot.title =  element_text(face = "bold", size = 18))
  if (combine_chains) {
    graph <- ggplot(ac_dat, aes(x= lag, y = ac))
    graph <- graph +
      geom_bar(position = "identity", stat = "identity",
               fill = base_fill, size = 0.4) +
      y_scale +
      ac_labs +
      ac_theme

    if (nParams == 1) return(graph + ggtitle(paste(params, "\n")) + title_theme)
    else return(graph + facet_wrap(~parameters))
  }

  graph <- ggplot(ac_dat, aes(x = lag, y = ac, color = factor(chains),
                              fill = factor(chains)))
  graph <- graph +
    geom_bar(position = "identity", stat = "identity", size = 0.4) +
    scale_fill_manual(values = rep(base_fill, nChains)) +
    scale_color_manual(values = rep(vline_base_clr, nChains)) +
    y_scale +
    ac_labs +
    ac_theme

  if (nParams == 1) {
    graph <- graph +
      facet_wrap(~chains) + ggtitle(paste(params, "\n")) + title_theme
    return(graph)
  } else { # nParams > 1

    while(is.null(flip)) return()

    graph <- graph +
      if (flip) facet_grid(chains ~ parameters) else facet_grid(parameters ~ chains)
    return(graph)
  }
}





# multiparam_plot --------------------------------------------------
# main plot of multiple parameters
.multiparam_plot <- function(samps, params = NULL, all_param_names,
                             show_density, show_ci_line, CI.level = 0.5,
                             show.level = 0.95, point_est, rhat_values,
                             color_by_rhat, rhat_palette, fill_color,
                             outline_color, est_color) {

  # params <- .update_params_with_regex(params, all_param_names)
  params <- .update_params_with_groups(params, all_param_names)
  .e <- environment()
  dim.samps <- dim(samps) #nIter, nChain, nParam
  if(length(params) == 0) {
    params <- dimnames(samps)$parameters[1:min(12, dim.samps[3])]
    if ("log-posterior" %in% params) {
      params <- params[-which(params == "log-posterior")]
    }
  }
  params <- unique(params)

  Blues <- c("#C6DBEF", "#4292C6", "#08306B")
  Grays <- c("#D9D9D9", "#737373", "#000000")
  Greens <- c("#C7E9C0", "#41AB5D", "#00441B")
  Oranges <- c("#FDD0A2", "#F16913", "#7F2704")
  Purples <- c("#DADAEB", "#807DBA", "#3F007D")
  Reds <- c("#FCBBA1", "#EF3B2C", "#67000D")
  rhat_pal <- get(rhat_palette)
  rhat_id <- ifelse(rhat_values < 1.05, "A",
                    ifelse(rhat_values < 1.1, "B", "C"))
  rhat_id <- factor(rhat_id[params], levels = c("A","B", "C"),
                    labels = c("<1.05", "<1.1", ">1.1"))
  rhat_colors <- scale_color_manual(name = bquote(hat(R)),
                                    values = rhat_pal,
                                    drop = FALSE)
  rhat_lgnd <- theme(legend.position = "top",
                     legend.title =  element_text(size = 13, face = "bold"),
                     legend.text =  element_text(size = 12))

  nParams <- length(params)
  nIter <- prod(dim.samps[1:2])
  samps.use <- array(samps[,,params], c(nIter, nParams))
  colnames(samps.use) <- params

  probs.use <- c(0.5 - show.level / 2,
                 0.5 - CI.level / 2,
                 0.5,
                 0.5 + CI.level / 2,
                 0.5 + show.level / 2)
  samps.quantile <- t(apply(samps.use, 2, quantile, probs = probs.use))
  y <- as.numeric(seq(nParams, 1, by = -1))
  xlim.use <- c(min(samps.quantile[,1]), max(samps.quantile[,5]))
  xrange <- diff(xlim.use)
  xlim.use[1] <- xlim.use[1] - 0.05 * xrange
  xlim.use[2] <- xlim.use[2] + 0.05 * xrange

  xy.df <- data.frame(params, y, samps.quantile)

  colnames(xy.df) <- c("params", "y", "ll", "l", "m", "h", "hh")
  if (point_est == "Mean") {
    xy.df$m <- unname(colMeans(samps.use))
  }
  p.base <- ggplot(xy.df, environment = .e)
  p.name <- scale_y_continuous(breaks = y, labels = params, limits = c(0.5, nParams + 1))
  p.theme <- theme(axis.title= element_blank(),
                   panel.background =  element_blank(),
                   panel.border =  element_blank(),
                   axis.ticks.y =  element_blank(),
                   axis.text= element_text(size=12),
                   axis.text.y= element_text(face = "bold"),
                   axis.line= element_line(size = 4, color = axis_line_color),
                   axis.line.y= element_line(size = 0.5, color = axis_line_color),
                   legend.position = "none",
                   panel.grid.major =  element_line(size = 0.4),
                   panel.grid.minor.y =  element_blank())

  p.all <- p.base + xlim(xlim.use) + p.name + theme_bw() + p.theme + transparent

  if (show_ci_line | show_density) {
    p.ci <- geom_segment(aes(x = ll, xend = hh, y = y, yend = y),
                         colour = outline_color)
    p.all <- p.all + p.ci
  }
  if (show_density) {
    nPoint.den <- 512
    #plot density
    y.den <- matrix(0, nrow = nPoint.den, ncol = nParams)
    x.den <- matrix(0, nrow = nPoint.den, ncol = nParams)
    for(i in 1:nParams){
      d.temp <- density(samps.use[,i],
                        from = samps.quantile[i,1],
                        to = samps.quantile[i,5],
                        n = nPoint.den)
      x.den[,i] <- d.temp$x
      y.max <- max(d.temp$y)
      y.den[,i] <- d.temp$y / y.max * 0.8 + y[i]
    }
    df.den <- data.frame(x = as.vector(x.den), y = as.vector(y.den),
                         name = rep(params, each = nPoint.den))
    p.den <- geom_line(data = df.den, aes(x = x, y = y, group = name),
                       color = outline_color)

    #shaded interval
    y.poly <- matrix(0, nrow = nPoint.den + 2, ncol = nParams)
    x.poly <- matrix(0, nrow = nPoint.den + 2, ncol = nParams)
    for(i in 1:nParams){
      d.temp <- density(samps.use[,i],
                        from = samps.quantile[i,2],
                        to = samps.quantile[i,4],
                        n = nPoint.den)
      x.poly[,i] <- c(d.temp$x[1], as.vector(d.temp$x), d.temp$x[nPoint.den])
      y.max <- max(d.temp$y)
      y.poly[,i] <- as.vector(c(0, as.vector(d.temp$y) / y.max * 0.8, 0) + y[i])
    }
    df.poly <- data.frame(x = as.vector(x.poly), y = as.vector(y.poly),
                          name = rep(params, each = nPoint.den + 2))
    p.poly <- geom_polygon(data = df.poly, aes(x = x, y = y, group = name, fill = y))
    p.col <- scale_fill_gradient(low = fill_color, high = fill_color, guide = "none")

    #point estimator
    if (color_by_rhat) {
      p.point <- geom_segment(aes(x = m, xend = m, y = y, yend = y + 0.25,
                                  color = rhat_id), size = 1.5)
      p.all + p.poly + p.den + p.col + p.point + rhat_colors + rhat_lgnd
    } else {
      p.point <- geom_segment(aes(x = m, xend = m, y = y, yend = y + 0.25),
                              colour = est_color,
                              size = 1.5)
      p.all + p.poly + p.den + p.col + p.point
    }

  } else {
    p.ci.2 <- geom_segment(aes(x = l, xend = h, y = y, yend = y),
                           colour = fill_color, size = 2)
    if (color_by_rhat) {
      p.point <- geom_point(aes(x = m, y = y, fill = rhat_id), color = "black",
                            shape = 21, size = 4)
      p.all + p.ci.2 + p.point + rhat_colors + rhat_lgnd
    } else {
      p.point <- geom_point(aes(x = m, y = y), size = 4, color = fill_color,
                            fill = est_color, shape = 21)
      p.all + p.ci.2 + p.point
    }
  }
}


# histogram of rhat, n_eff/N or mcse/sd -----------------------------------
.rhat_neff_mcse_hist <- function(dat, which, N) {
  # samps: post-warmup samples
  xlab <- switch(which,
         rhat = "Rhat statistic",
         n_eff = "Effective sample size / iterations",
         mcse = "Monte Carlo se / posterior sd"
         )
  my_labs <- labs(y = "", x = xlab)
  base_fill
  graph <- qplot(x = x, data = dat, color = I(vline_base_clr),
                 fill = I(base_fill), size = I(0.2))
  graph <- graph +
    my_labs +
    theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs + transparent)

  graph
}

# n_eff_warnings -----------------------------------------------------------
.n_eff_warnings <- function(summary, threshold = 10,
                            N_total = NULL) {
  n_eff <- summary[,"n_eff"]
  warn_params <- names(which(n_eff / N_total < threshold / 100))
  ll <- length(warn_params)
  if (ll == 0) "None"
  else paste0(warn_params, collapse = ", ")
}

# rhat_warnings -----------------------------------------------------------
.rhat_warnings <- function(summary, threshold = 1.10) {
  rhat <- summary[,"Rhat"]
  warn_params <- names(which(rhat > threshold))
  ll <- length(warn_params)
  if (ll == 0) "None"
  else paste0(warn_params, collapse = ", ")
}

# mcse_over_sd_warnings -----------------------------------------------------------
.mcse_over_sd_warnings <- function(summary, threshold = 10) {
  dat <- summary[,c("se_mean", "sd")]
  warn_params <- names(which(dat[,1] > (threshold/100) * dat[,2]))

  ll <- length(warn_params)
  if (ll == 0) "None"
  else paste0(warn_params, collapse = ", ")
}


# dynamic trace plot ------------------------------------------------------
.param_trace_dynamic <- function(param_samps, chain,
                                 warmup_val, warmup_shade = TRUE,
                                 stack = FALSE, grid = FALSE,
                                 x_lab = NULL, y_lab = NULL) {

  dim_samps <- dim(param_samps)
  if (is.null(dim_samps)) nChains <- 1
  else nChains <- dim_samps[2]
  if (nChains == 1) {
    param_chains <- xts::as.xts(ts(param_samps, start = 1))
  } else {
    if (chain != 0) {
      param_samps <- param_samps[, chain]
      param_chains <- xts::as.xts(ts(param_samps, start = 1))
    } else {
      param_chains <- xts::as.xts(ts(param_samps[,1], start = 1))
      for (i in 2:nChains) {
        param_chains <- cbind(param_chains, xts::as.xts(ts(param_samps[,i], start = 1)))
      }
      colnames(param_chains) <- paste0("Chain", 1:nChains)
    }
  }

  `%>%` <- dygraphs::`%>%`
  shade_to <- if (warmup_shade)
    paste0(warmup_val,"-01-01") else "0001-01-01"
  y_axis_label_remove <- if (stack)
    "white" else NULL
  clrs <- color_vector(nChains)
  if (chain != 0) clrs <- clrs[chain]
  dygraphs::dygraph(param_chains, xlab = x_lab, ylab = y_lab) %>%
    dygraphs::dyAxis("y", axisLabelColor = y_axis_label_remove) %>%
    dygraphs::dyAxis("x", axisLabelColor = "white") %>%
    dygraphs::dyOptions(colors = clrs, stackedGraph = stack, drawGrid = grid,
                        animatedZooms = TRUE, axisLineColor = axis_line_color) %>%
    dygraphs::dyLegend(show = "never") %>%
    dygraphs::dyHighlight(highlightCircleSize = 4,
                highlightSeriesBackgroundAlpha = 1/3,
                hideOnMouseOut = TRUE,
                highlightSeriesOpts = list(strokeWidth = 1.75)) %>%
    dygraphs::dyRoller(rollPeriod = 1) %>%
    dygraphs::dyShading(from = "0001-01-01", to = shade_to, color = "#EFEFEF", axis = "x") %>%
    dygraphs::dyCSS(css = "css/ShinyStan_dygraphs.css")
}

# trivariate_plot ---------------------------------------------------------
.param_trivariate <- function(samps, params,
                              transform_x = "identity", transform_y = "identity",
                              transform_z = "identity",
                              pt_size = 1, pt_color = "gray35", show_grid = TRUE,
                              flip_y = TRUE) {
  nParams <- 3
  dim_samps <- dim(samps)
  nIter <- dim_samps[1] * dim_samps[2]
  samps_use <- array(samps[,, params], c(nIter, nParams))
  colnames(samps_use) <- params

  t_x <- get(transform_x)
  t_y <- get(transform_y)
  t_z <- get(transform_z)

  if (transform_x != "identity") {
    samps_use[,1] <- t_x(samps_use[,1])
    colnames(samps_use)[1] <- paste0(transform_x, "(", params[1], ")")
  }
  if (transform_y != "identity") {
    samps_use[,2] <- t_y(samps_use[,2])
    colnames(samps_use)[2] <- paste0(transform_y, "(", params[2], ")")
  }
  if (transform_z != "identity") {
    samps_use[,3] <- t_z(samps_use[,3])
    colnames(samps_use)[3] <- paste0(transform_z, "(", params[3], ")")
  }
  threejs::scatterplot3js(samps_use, size = pt_size, color = pt_color,
                          grid = show_grid, flip.y = flip_y)
}


# bivariate plot ----------------------------------------------------------
.bivariate_plot <- function(samps, sp = NULL, max_td = NULL,
                            param, param2,
                            pt_alpha = 0.10,
                            pt_size = 2,
                            pt_shape = 10,
                            pt_color = "gray20",
                            ellipse_color = "black",
                            ellipse_lev = "None",
                            ellipse_lty = 1,
                            ellipse_lwd = 1,
                            ellipse_alpha = 1,
                            lines = "back",
                            lines_color = "gray",
                            lines_alpha,
                            points = TRUE,
                            transform_x = "identity",
                            transform_y = "identity"
){

  shape_translator <- function(x) {
    shape <- if (x >= 6) x + 9 else x
    shape
  }

  params <- c(param, param2)
  nParams <- 2
  nIter <- dim(samps)[1] * dim(samps)[2]
  samps_use <- array(samps[,,params], c(nIter, nParams))
  colnames(samps_use) <- params

  t_x <- get(transform_x)
  # t_x <- function(x) eval(parse(text = transform_x))
  t_y <- get(transform_y)
  x_lab <- if (transform_x != "identity")
    paste0(transform_x, "(", param, ")") else param
  y_lab <- if (transform_y != "identity")
    paste0(transform_y, "(", param2, ")") else param2
  param_labs <- labs(x = x_lab, y = y_lab)

  dat <- data.frame(
    x = if (transform_x == "identity")
      samps_use[,param] else t_x(samps_use[,param]),
    y = if (transform_y == "identity")
      samps_use[,param2] else t_y(samps_use[,param2]))
  if (!is.null(sp)) {
    dat$divergent <- c(sapply(sp, FUN = function(y) y[, "divergent__"]))
    dat$hit_max_td <- if (is.null(max_td)) 0 else
      c(sapply(sp, FUN = function(y) as.numeric(y[, "treedepth__"] == max_td)))
  } else {
    dat$divergent <- 0
    dat$hit_max_td <- 0
  }
  graph <- ggplot(dat, aes(x = x, y = y, xend=c(tail(x, n=-1), NA),
                           yend=c(tail(y, n=-1), NA)))

  if (lines == "hide") {
    graph <- graph + geom_point(alpha = pt_alpha, size = pt_size,
                                shape = shape_translator(pt_shape),
                                color = pt_color)
  } else { # if lines = "back" or "front"
    if (lines == "back") {
      graph <- graph +
        geom_path(alpha = lines_alpha, color = lines_color) +
        geom_point(alpha = pt_alpha, size = pt_size,
                   shape = shape_translator(pt_shape), color = pt_color)
    } else { # lines = "front"
      graph <- graph +
        geom_point(alpha = pt_alpha, size = pt_size,
                   shape = shape_translator(pt_shape), color = pt_color) +
        geom_path(alpha = lines_alpha, color = lines_color)
    }
  }
  if (ellipse_lev != "None")
    graph <- graph + stat_ellipse(level = as.numeric(ellipse_lev), color = ellipse_color,
                                  linetype = ellipse_lty, size = ellipse_lwd, alpha = ellipse_alpha)
  if (!all(dat$divergent == 0))
    graph <- graph + geom_point(data = subset(dat, divergent == 1), aes(x,y),
                                size = pt_size + 0.5, shape = 21,
                                color = "#570000", fill = "#ae0001")
  if (!all(dat$hit_max_td == 0))
    graph <- graph + geom_point(data = subset(dat, hit_max_td == 1), aes(x,y),
                                size = pt_size + 0.5, shape = 21,
                                color = "#5f4a13", fill = "#eeba30")
  graph + param_labs +
    theme_classic() %+replace% (no_lgnd + axis_labs + fat_axis + axis_color + transparent)
}

