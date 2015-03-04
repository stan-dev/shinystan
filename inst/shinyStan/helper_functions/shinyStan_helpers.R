# misc. functions --------------------------------------------------------
.in_range <- function(x, a, b) {
  x >= a & x <= b
}


# ggplot theme elements --------------------------------------------
axis_line_color <- "#346fa1"
axis_color <- theme(axis.line = element_line(color = axis_line_color))
axis_labs <- theme(axis.title = element_text(face = "bold", size = 13))
title_txt <- theme(plot.title = element_text(face = "bold", size = 14))
fat_axis <- theme(axis.line.x = element_line(size = 3, color = axis_line_color), 
                  axis.line.y = element_line(size = 0.5, color = axis_line_color))
h_lines <- theme(panel.grid.major = element_line(size = 0.25, linetype = 3, color = "turquoise4"),
                 panel.grid.major.x = element_blank())
v_lines <- theme(panel.grid.major = element_line(size = 0.25, linetype = 3, color = "turquoise4"),
                 panel.grid.major.y = element_blank())
no_lgnd <- theme(legend.position = "none")
lgnd_bot <- theme(legend.position = "bottom")
lgnd_top <- theme(legend.position = "top")
no_yaxs <- theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

# make_param_list ------------------------------------------------------
# generate list of parameter names for selectInput
.make_param_list <- function(object) {
  choices <- list()
  ll <- length(object@param_dims)
  choices[1:ll] <- ""
  names(choices) <- object@param_groups
  for(i in 1:ll) {
    if (length(object@param_dims[[i]]) == 0) {
      choices[[i]] <- list(object@param_groups[i])
    }
    else {
      temp <- paste0(object@param_groups[i],"\\[")
      choices[[i]] <- object@param_names[grep(temp, object@param_names)]
    }
  }
  choices
}

# make_param_list_with_groups ------------------------------------------------------
# generate list of parameter names for selectInput and include parameter groups
.make_param_list_with_groups <- function(object, sort_j = FALSE) {
  choices <- list()
  ll <- length(object@param_dims)
  LL <- sapply(1:ll, function(i) length(object@param_dims[[i]]))
  
  choices[1:ll] <- ""
  names(choices) <- object@param_groups
  for(i in 1:ll) {
    if (LL[i] == 0) {
      choices[[i]] <- list(object@param_groups[i])
    }
    else {
      group <- object@param_groups[i]
      temp <- paste0("^",group,"\\[")
      ch <- object@param_names[grep(temp, object@param_names)]
      
      # change sorting so e.g. "beta[1,1] beta[1,2] beta[2,1] beta[2,2]"
      # instead of "beta[1,1] beta[2,1] beta[1,2] beta[2,2]"
      if (sort_j == TRUE & LL[i] > 1) ch <- gtools::mixedsort(ch) 
      
      ch_out <- c(paste0(group,"_as_shiny_stan_group"), ch)
      names(ch_out) <- c(paste("ALL", group), ch)
      choices[[i]] <- ch_out
    }
  }
  
  choices
}



# param_summary -----------------------------------------------------------
# summary stats for a single parameter
.param_summary <- function(param, summary) {
  out <- summary[param, c("Rhat", "n_eff", "mean", "sd", "2.5%", "50%", "97.5%")]
  out["n_eff"] <- round(out["n_eff"])
  outmat <- matrix(out, 1, length(out))
  colnames(outmat) <- names(out)
  rownames(outmat) <- NULL
  outmat
}


# all_summary -------------------------------------------------------------
# summary stats for all parameters
.all_summary <- function(fit_summary, digits = 2, cols) {
  df <- as.data.frame(fit_summary[, cols])
  df <- round(df, digits)
  if ("n_eff" %in% cols) {
    df[,"n_eff"] <- round(df[,"n_eff"])
  }
  out <- cbind(Parameter = rownames(df), df)
  out
}

# sampler_summary ---------------------------------------------------------
# summary statistics for algorithm=NUTS or algorithm=HMC sampler parameters
.sampler_summary <- function(sampler_params, inc_warmup, warmup_val,
                             report = "average", algorithm = "NUTS", digits = 4) {
  sampler_stuff <- function(param) {
    X <- sampler_params
    if (inc_warmup == FALSE) {
      X <- lapply(1:length(sampler_params), function(i) {
        out <- sampler_params[[i]]
        out[-(1:warmup_val), ]
      })
    }
    if (report == "maximum") { # then return max
      out <- sapply(X, FUN = function(x) max(x[,param]) )
    } else {
      if (report == "minimum") { # then return min
        out <- sapply(X, FUN = function(x) min(x[,param]) )
      } else { # return avg
        out <- sapply(X, FUN = function(x) mean(x[,param]) )
      }
    }
    names(out) <- paste0("chain",1:length(out))
    out
  }
  params <- colnames(sampler_params[[1]])
  out <- sapply(params, FUN = function(i) sampler_stuff(param = i))

  if (length(dim(out)) > 1) { # i.e. multiple chains
    out <- rbind("All chains" = colMeans(out), out)
    colnames(out) <- gsub("__","",colnames(out))
    out <- apply(out, 2, round, digits)
    out <- cbind(Chain = rownames(out), out)
  } else {
    # if only 1 chain
    names(out) <- gsub("__.chain1", "", names(out))
    out <- t(out)
    out <- round(out, digits)
  }

  return(out)
}




# param_trace -------------------------------------------------------------
# trace plot for a single parameter
.param_trace <- function(param, dat,
                         warmup_val, inc_warmup = FALSE,
                         chain,
                         style, palette,
                         rect, rect_color, rect_alpha,
                         x1, x2, y1, y2) {
  # x1, x2, y1, y2 are the x and y axis limits for the tracezoom feature
  # style: either "point" or "line"

  dat <- melt(dat)

  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }

  if (!inc_warmup) {
    dat <- subset(dat, iterations >= warmup_val)
  }
  if (chain != 0) { # note: we use chain == 0 for all chains
    dat <- subset(dat, chains == paste0("chain:",chain))
  }

  rect_xmin <- ifelse(rect == "Samples", Inf, -Inf)
  shading_rect <- annotate("rect", xmin = rect_xmin, xmax = warmup_val,
                           ymin = -Inf, ymax = Inf, fill = rect_color, alpha = rect_alpha)

  xy_labs <- labs(y = param, x = ifelse(inc_warmup, "Iteration \n Warmup | Samples", "Iteration (warmpup excluded)"))
  nclrs <- length(unique(dat$chains))
  if(palette == "Default") clrs <- scale_color_discrete()
  if(palette == "Gray") clrs <- scale_color_grey()
  if(palette == "Brewer (spectral)") clrs <- scale_color_brewer(palette = "Spectral")
  if(palette == "Rainbow") clrs <- scale_colour_manual(values = rainbow(nclrs))

  theme_elements <- axis_color + fat_axis + no_lgnd + axis_labs
  if (style == "line") theme_elements <- theme_elements + h_lines
  graph <- ggplot(dat, aes(x = iterations, y = value, color = chains))
  graph <- graph + xy_labs + clrs + theme_classic() %+replace% theme_elements # (fat_axis + h_lines + no_lgnd)
  if (rect != "None") graph <- graph + shading_rect
  if (rect == "None" & inc_warmup) graph <- graph + geom_vline(xintercept = warmup_val, color = "gray35", size = 1.5)

  if (style == "point") {
    print(ddply(dat, "chains", summarise, mean = mean(value))$mean)
    graph <- graph + geom_point(size = 1.5, alpha = 0.55)
  } else {
    graph <- graph + geom_line(size = 0.35)
  }

  if (is.na(x1)) {
    return(graph)
  } else {
    graph <- (graph +
                scale_y_continuous(limits = c(y1, y2)) +
                scale_x_continuous(limits = c(x1, x2))
    )
    return(graph)
  }
}


# param_trace_multi ------------------------------------------------------
# trace plots for multiple parameters
.param_trace_multi <- function(params = NULL, all_param_names, dat, warmup_val = 0,
                               inc_warmup = TRUE, chain = 0, palette = "Default",
                               rect = "Samples", rect_color = "skyblue", rect_alpha = 0.1,
                               layout = "Long", x1, x2) {

  params <- .update_params_with_groups(params, all_param_names)
  if(length(params) == 0) {
    params <- dimnames(dat)$parameters[1:min(4, dim(dat)[3])]
  }
  params <- unique(params)
  dat <- melt(dat)
  dat <- subset(dat, parameters %in% params)

  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }

  if (!inc_warmup) dat <- subset(dat, iterations >= warmup_val)
  if (chain != 0) dat <- subset(dat, chains == paste0("chain:",chain))

  rect_xmin <- ifelse(rect == "Samples", Inf, -Inf)
  shading_rect <- annotate("rect", xmin = rect_xmin, xmax = warmup_val,
                           ymin = -Inf, ymax = Inf, fill = rect_color, alpha = rect_alpha)

  xy_labs <- labs(y = "Value", x = "Iteration")
  nclrs <- length(unique(dat$chains))

  ldgn_title <- ""
  if(palette == "Default") clrs <- scale_color_discrete(name = ldgn_title)
  if(palette == "Gray") clrs <- scale_color_grey(name = ldgn_title)
  if(palette == "Brewer (spectral)") clrs <- scale_color_brewer(name = ldgn_title, palette = "Spectral")
  if(palette == "Rainbow") clrs <- scale_colour_manual(name = ldgn_title, values = rainbow(nclrs))


  lgnd_txt <- theme(legend.text = element_text(size = 13, face = "bold"))
  strip_txt <- theme(strip.text = element_text(size = 12, face = "bold", color = "white"),
                     strip.background = element_rect(color = "#346fa1", fill = "#346fa1"))

  graph <- ggplot(dat, aes(x = iterations, y = value, color = chains))
  graph <- graph + xy_labs + clrs + theme_classic() %+replace% (axis_color + axis_labs + fat_axis + h_lines + lgnd_top + lgnd_txt + strip_txt)
  if (rect != "None") graph <- graph + shading_rect
  graph <- graph + geom_line(size = 0.35)
  if (!is.na(x1)) {
    graph <- graph + scale_x_continuous(limits = c(x1, x2))
  }
  if (layout == "Grid") {
    graph <- graph + facet_wrap(~ parameters, scale = "free_y")
  } else {
    graph <- graph + facet_grid(parameters ~., scale = "free_y")
  }

  graph
}


# param_hist --------------------------------------------------------------
# histogram for a single parameter
.param_hist <- function(param, dat, chain, binwd,
                        fill_color = "gray35", line_color = "gray35",
                        title = TRUE) {
  
  ttl <- "Histogram of Posterior Draws (post-warmup) \n"
  dat <- melt(dat)

  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }

  if (chain != 0) {
    dat <- subset(dat, chains == paste0("chain:",chain))
  }

  graph <- ggplot(dat, aes(x = value))

  if (binwd == 0) {
    graph <- graph + geom_histogram(fill = fill_color, color = line_color)
  } else {
    graph <- graph + geom_histogram(fill = fill_color, color = line_color, binwidth = binwd)
  }

  graph <- graph +
    labs(x = param, y = "") +
    theme_classic() %+replace% (title_txt + axis_color + axis_labs + fat_axis + no_yaxs)

  if (title == TRUE) graph <- graph + ggtitle(ttl)

  graph
}


# param_dens --------------------------------------------------------------
# density plot for a single parameter
.param_dens <- function(param, dat, chain,
                        fill_color = NULL, line_color = NULL,
                        point_est = "None", CI,
                        x_breaks = "Some", # y_breaks = "None",
                        chain_split = FALSE,
                        title = TRUE) {

  ttl <- "Kernel Density Estimate (post-warmup) \n"
  
  dat <- melt(dat)

  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }

  if (chain != 0) {
    dat <- subset(dat, chains == paste0("chain:",chain))
  }

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
  #   if(x_breaks == "Too Many") x_scale <- scale_x_continuous(breaks = too_many_breaks)
  #   if(y_breaks == "None") y_scale <- scale_y_continuous(breaks = NULL)
  #   if(y_breaks == "Some") y_scale <- scale_y_continuous()
  #   if(y_breaks == "Many") y_scale <- scale_y_continuous(breaks = many_breaks)
  #   if(y_breaks == "Too Many") y_scale <- scale_y_continuous(breaks = too_many_breaks)
  #

  if (chain == 0 & chain_split == TRUE) {
    graph <- ggplot(dat, aes(x = value, color = chains, fill = chains)) +
      geom_density(alpha = 0.15) +
      scale_color_discrete("") + scale_fill_discrete("") +
      labs(x = param, y = "") +
      x_scale + # y_scale +
      theme_classic() %+replace% (title_txt + axis_color + axis_labs + fat_axis + no_yaxs)
    if (title == TRUE) graph <- graph + ggtitle(ttl)
    return(graph)
  }

  graph <- ggplot(dens_dat, aes(x = x, ymax = y))
  graph <- graph +
    labs(x = param, y = "") +
    x_scale + # y_scale +
    geom_ribbon(ymin = 0, fill = fclr, color = fclr) +
    theme_classic() %+replace% (title_txt + axis_color + axis_labs + fat_axis + no_yaxs)

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
  graph
}




# autocorr_single_plot ----------------------------------------------------
# markov chain autocorrelation plot for single parameter (for multiview)
.autocorr_single_plot <- function(samps, lags) {
  
  dat <- melt(samps)

  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }

  ac_dat <- ddply(dat, "chains", here(summarise),
                  ac = acf(value, lag.max = lags, plot = FALSE)$acf[,,1],
                  lag = 0:lags)

  ac_labs <- labs(x = "Lag", y = "Autocorrelation")
  strip_txt <- theme(strip.text = element_text(size = 12, face = "bold", color = "white"),
                     strip.background = element_rect(color = "#346fa1", fill = "#346fa1"))
  ac_theme <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_lgnd + strip_txt)
  y_scale <- scale_y_continuous(breaks = seq(0, 1, 0.25), labels = c("0","","0.5","",""))

  graph <- ggplot(ac_dat, aes(x = lag, y = ac))
  graph <- graph +
    geom_bar(position = "identity", stat = "identity", fill = "gray35") +
      y_scale + ac_theme
  graph
}


# autocorr_plot -----------------------------------------------------------
# markov chain autocorrelation plot for multiple parameters
.autocorr_plot <- function(samps, params = NULL, all_param_names,
                           nChains,
                           lags = 25, flip = FALSE,
                           combine_chains = FALSE) {



  params <- .update_params_with_groups(params, all_param_names)
  if(length(params) == 0) {
    dim.samps <- dim(samps) 
    params <- dimnames(samps)$parameters[1] # defaults to first parameter
  }
  params <- unique(params)
  dat <- samps[,,params]
  dat <- melt(dat)

  if (!("chains" %in% colnames(dat))) { # fixes for if there's only 1 chain:
    dat$chains <- "chain:1"
    dat$iterations <- 1:nrow(dat)
  }

  nParams <- length(params)
  if (nParams == 1) {
    ac_dat <- ddply(dat, "chains", here(summarise),
                    ac = acf(value, lag.max = lags, plot = FALSE)$acf[,,1],
                    lag = 0:lags)
  }
  if (nParams > 1) {
    ac_dat <- ddply(dat, c("parameters", "chains"), here(summarise),
                    ac = acf(value, lag.max = lags, plot = FALSE)$acf[,,1],
                    lag = 0:lags)
  } 

  ac_labs <- labs(x = "Lag", y = "Autocorrelation")
  strip_txt <- theme(strip.text = element_text(size = 12, face = "bold", color = "white"),
                     strip.background = element_rect(color = "#346fa1", fill = "#346fa1"))
  ac_theme <- theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_lgnd + strip_txt)
  y_scale <- scale_y_continuous(breaks = seq(0, 1, 0.25), labels = c("0","","0.5","",""))
  title_theme <- theme(plot.title = element_text(face = "bold", size = 18))
  if (combine_chains) {
    if (nParams == 1) graph <- ggplot(ac_dat, aes(x = lag, y = ac))
    else graph <- ggplot(ac_dat, aes(x= lag, y = ac))
      
    graph <- graph +
      geom_bar(position = "identity", stat = "identity", fill = "gray35") +
      y_scale + ac_labs + ac_theme
    
    if (nParams == 1) return(graph + ggtitle(paste(params, "\n")) + title_theme)
    else return(graph + facet_wrap(~parameters))
  }


  graph <- ggplot(ac_dat, aes(x = lag, y = ac, fill = factor(chains)))
  graph <- graph +
    geom_bar(position = "identity", stat = "identity") +
    scale_fill_manual(values = rep("gray35", object@nChains)) +
    y_scale +
    ac_labs +
    ac_theme

  if (nParams == 1) {
    graph <- graph + facet_wrap(~chains) + ggtitle(paste(params, "\n")) + title_theme
    return(graph)
  } else { # nParams > 1
    
    while(is.null(flip)) return()
    
    graph <- graph + if (flip) facet_grid(chains ~ parameters) else facet_grid(parameters ~ chains)
    return(graph)
  }
}



# update_params_with_groups -----------------------------------------------
# update parameter selection for multi-parameter plot
.update_params_with_groups <- function(params, all_param_names) {
  as_group <- grep("_as_shiny_stan_group", params)
  if (length(as_group) == 0) {
    return(params)
  }

  make_group <- function(group_name) {
#     temp <- paste0(group_name,"\\[")
#     group <- all_param_names[grep(temp, all_param_names)]
#     group <- group[which(substr(group, 1, nchar(group_name)) == group_name)]
    all_param_names[grep(paste0("^",group_name,"\\["), all_param_names)]
  }
  single_params <- params[-as_group]
  grouped_params <- params[as_group]
  groups <- gsub("_as_shiny_stan_group", "", grouped_params)
  groups <- sapply(groups, make_group)
  updated_params <- c(single_params, unlist(groups))
  updated_params
}


# plot_param_vertical_gg --------------------------------------------------
# main plot of multiple parameters
.plot_param_vertical_gg <- function(samps,
                                    params = NULL,
                                    all_param_names,
                                    show_density,
                                    show_ci_line,
                                    CI.level = 0.5,
                                    show.level = 0.95,
                                    point_est,
                                    rhat_values,
                                    color_by_rhat,
                                    rhat_palette,
                                    fill_color,
                                    outline_color,
                                    est_color) {

  params <- .update_params_with_groups(params, all_param_names)
  .e <- environment()
  dim.samps <- dim(samps) #nIter, nChain, nParam
  if(length(params) == 0) {
    params <- dimnames(samps)$parameters[1:min(12, dim.samps[3])]
    if ("lp__" %in% params) {
      params <- params[-which(params == "lp__")]
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
  rhat_id <- factor(rhat_id[params], levels = c("A","B", "C"), labels = c("<1.05", "<1.1", ">1.1"))
  rhat_colors <- scale_color_manual(name = bquote(hat(R)),
                                    values = rhat_pal,
                                    drop = FALSE)
  rhat_lgnd <- theme(legend.position = "top",
                     legend.title = element_text(size = 13, face = "bold"),
                     legend.text = element_text(size = 12))

  nParams <- length(params)
  nIter <- dim.samps[1] * dim.samps[2]
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
    xy.df$m <- apply(samps.use, 2, mean)
  }
  p.base <- ggplot(xy.df, environment = .e)
  p.name <- scale_y_continuous(breaks = y, labels = params, limits = c(0.5, nParams + 1))
  p.theme <- theme(axis.title=element_blank(),
                   panel.background = element_blank(),
                   panel.border = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text=element_text(size=12),
                   axis.text.y=element_text(face = "bold"),
                   axis.line=element_line(size = 4, color = "#346fa1"),
                   axis.line.y=element_line(size = 0.5),
                   legend.position = "none",
                   panel.grid.major = element_line(size = 0.4),
                   panel.grid.minor.y = element_blank())

  p.all <- p.base + xlim(xlim.use) + p.name + theme_bw() + p.theme

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

    #shaded the confidence interval
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
      p.point <- geom_segment(aes(x = m, xend = m, y = y, yend = y + 0.25, color = rhat_id),
                              size = 1.5)
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
      p.point <- geom_point(aes(x = m, y = y, color = rhat_id), size = 3)
      p.all + p.ci.2 + p.point + rhat_colors + rhat_lgnd
    } else {
      p.point <- geom_point(aes(x = m, y = y), size = 3.5, color = fill_color, fill = est_color, shape = 21)
      p.all + p.ci.2 + p.point
    }
  }
}


# histogram of rhat, n_eff/N or mcse/sd -----------------------------------
.rhat_neff_mcse_hist <- function(fit_summary, samps, which) {
  # samps: post-warmup samples

  if (which == "rhat") {
    dat <- fit_summary[,"Rhat"]
    dat <- data.frame(parameter = names(dat), x = dat)
    my_labs <- labs(y = "", x = "Rhat statistic")
  }
  if (which == "n_eff") {
    N <- length(samps[,,1])
    dat <- fit_summary[,"n_eff"]
    dat <- data.frame(parameter = names(dat), x = dat/N)
    my_labs <- labs(y = "", x = "Effective sample size / iterations")
  }
  if (which == "mcse") {
    dat <- fit_summary[, c("se_mean", "sd")]
    dat <- dat[,1] / dat[,2]
    dat <- data.frame(parameter = names(dat), x = dat)
    my_labs <- labs(y = "", x = "Monte Carlo se / posterior sd")
  }
  graph <- qplot(x = x, data = dat, color = I("gray65"), fill = I("gray35"))
  graph <- graph + my_labs + theme_classic() %+replace% (axis_color + axis_labs + fat_axis + no_yaxs)

  graph
}

# n_eff_warnings -----------------------------------------------------------
.n_eff_warnings <- function(summary, threshold = 10, N_total = length(samps_post_warmup[,,1])) {
  n_eff <- summary[,"n_eff"]
  warn_params <- names(which(n_eff/N_total < threshold/100 ))
  ll <- length(warn_params)
  if (ll == 0) {
    return("None")
  }
  return(paste0(warn_params, collapse = ", "))
}

# rhat_warnings -----------------------------------------------------------
.rhat_warnings <- function(summary, threshold = 1.10) {
  rhat <- summary[,"Rhat"]
  warn_params_1 <- names(which(rhat > threshold))
  ll <- length(warn_params_1)
  if (ll == 0) {
    return("None")
  }
  return(paste0(warn_params_1, collapse = ", "))
}

# mcse_over_sd_warnings -----------------------------------------------------------
.mcse_over_sd_warnings <- function(summary, threshold = 10) {
  dat <- summary[,c("se_mean", "sd")]
  warn_params <- names(which(dat[,1] > (threshold/100) * dat[,2]))

  ll <- length(warn_params)
  if (ll == 0) {
    return("None")
  }
  return(paste0(warn_params, collapse = ", "))
}


# dynamic trace plot ------------------------------------------------------
.param_trace_dynamic <- function(param_samps, param_name, chain,
                                 warmup_val, warmup_shade = TRUE,
                                 stack = FALSE, grid = FALSE) {
#   get_gg_colors <- function(nColors) {
#     colors <- hcl(h = seq(15, 375, length = nColors+1), l = 65, c = 100)
#     colors[1:nColors]
#   }
#
#   clrs <- get_gg_colors(nChains)

  dim_samps <- dim(param_samps)
  if (is.null(dim_samps)) {
    nChains <- 1
  } else {
    nChains <- dim_samps[2]
  }

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

  shade_to <- if (warmup_shade) paste0(warmup_val,"-01-01") else "0001-01-01"
  y_axis_label_remove <- if (stack) "white" else NULL
  dygraphs::dygraph(param_chains, xlab = "Iteration", ylab = param_name) %>%
    dygraphs::dyAxis("y", rangePad = "5", axisLabelColor = y_axis_label_remove) %>%
    dygraphs::dyAxis("x", rangePad = "3") %>%
    dygraphs::dyOptions(stackedGraph = stack, drawGrid = grid, animatedZooms = TRUE, axisLineColor = axis_line_color) %>%
    dygraphs::dyRangeSelector(height = 40, strokeColor = "#346fa1", fillColor = "#d9e7f4") %>%
    dygraphs::dyLegend(show = "auto", width = 400) %>%
    #     dygraphs::dyEvent(date = as.Date(warmup_val),
    #             label = "", color = "gray35", strokePattern = "dotted") %>%
    dygraphs::dyHighlight(highlightCircleSize = 4,
                highlightSeriesBackgroundAlpha = 1/3,
                hideOnMouseOut = TRUE,
                highlightSeriesOpts = list()) %>%
    dygraphs::dyRoller(rollPeriod = 1) %>%
    dygraphs::dyShading(from = "0001-01-01", to = shade_to, color = "#d9e7f4") %>%
    dygraphs::dyCSS(css = "shinyStan.css")
}

# trivariate_plot ---------------------------------------------------------
.param_trivariate <- function(samps, params,
                              pt_size = 1, show_grid = TRUE, flip_y = TRUE) {
  nParams <- length(params)
  dim_samps <- dim(samps)
  nIter <- dim_samps[1] * dim_samps[2]
  samps_use <- array(samps[,,params], c(nIter, nParams))
  colnames(samps_use) <- params
  threejs::scatterplot3js(samps_use, size = pt_size, grid = show_grid, flip.y = flip_y)
}



# bivariate plot ----------------------------------------------------------
.bivariate_plot <- function(samps, param, param2,
                            pt_alpha = 0.10,
                            pt_size     = 2,
                            pt_shape    = 10,
                            pt_color    = "firebrick",
                            ellipse_color    = "black",
                            ellipse_lev = "None",
                            ellipse_lty      = 1,
                            ellipse_lwd      = 1,
                            ellipse_alpha    = 1,
                            lines = TRUE,
                            points = TRUE
){

  shape_translator <- function(x) {
    shape <- ifelse(x >= 6, x + 9, x)
    shape
  }

  params <- c(param, param2)
  nParams <- 2
  nIter <- dim(samps)[1] * dim(samps)[2]
  samps_use <- array(samps[,,params], c(nIter, nParams))
  colnames(samps_use) <- params
  dat <- data.frame(x = samps_use[,param], y = samps_use[,param2])
  graph <- ggplot(dat, aes(x = x, y = y, xend=c(tail(x, n=-1), NA), yend=c(tail(y, n=-1), NA))) + labs(x = param, y = param2)
  if (lines) graph <- graph + geom_segment(alpha = 1, color = "gray85", arrow = grid::arrow(length=grid::unit(0.2,"cm"))) 
  
  
  
  
  if (points) graph <- graph + geom_point(alpha = pt_alpha, size = pt_size, shape = shape_translator(pt_shape), color = pt_color)
  if (ellipse_lev != "None") {
    graph <- graph + stat_ellipse(level = as.numeric(ellipse_lev), color = ellipse_color, linetype = ellipse_lty, size = ellipse_lwd, alpha = ellipse_alpha)
  }

  graph + theme_classic() %+replace% (no_lgnd + axis_labs + fat_axis + axis_color)
}

