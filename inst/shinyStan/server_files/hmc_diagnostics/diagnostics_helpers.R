small_axis_labs <- theme(axis.title = element_text(face = "bold", size = 11))
diagnostics_fat_axis <- theme(axis.line.x = element_line(size = 3, color = "black"), 
                  axis.line.y = element_line(size = 0.5, color = "black"))
thm <- theme_classic() %+replace% (no_lgnd + diagnostics_fat_axis + small_axis_labs + transparent)
thm_no_yaxs <- thm + no_yaxs

base_fill <- "skyblue"
overlay_fill <- "skyblue4"
vline_base_clr <- "black"
vline_overlay_clr <- "black"
single_trace_clr <- overlay_fill
divergent_fill <- "#ae0001" # "#5cffcc"
hit_max_td_fill <- "#eeba30"
divergent_clr <-  "black" #"#772000" # "#40b28e"
hit_max_td_clr <- "black" # "#473600"
div_and_hit_shape <- 21

color_vector <- function(n) {
  hues = seq(15, 375, length=n+1)
  # hcl(h=hues, l=65, c=100)[1:n]
  hcl(h=hues, l=50, c=50)[1:n]
}

# post-warmup sampler parameters ------------------------------------------
sp_nuts_check <- reactive({
  validate(
    need(sampler_params[[1L]] != "Not Stan", message = "Only available for Stan models"),
    need(stan_algorithm == "NUTS", message = "Only available for algorithm = NUTS"),
    need(input$diagnostic_chain, message = "Loading...")
  )
})

.sampler_param_pw <- function(sp, which = "accept_stat__", warmup_val) {
  sp_pw <- lapply(1:length(sp), function(i) {
    out <- sp[[i]][, which]
  })
  sp_mat <- do.call("cbind", sp_pw)
  colnames(sp_mat) <- paste0("chain:", 1:ncol(sp_mat))
  sp_mat <- cbind(iterations = (warmup_val+1):(warmup_val + nrow(sp_mat)), sp_mat)
  as.data.frame(sp_mat)
}


accept_stat_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params_post_warmup, which = "accept_stat__", 
                    warmup_val = warmup_val)
})
treedepth_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params_post_warmup, which = "treedepth__", 
                    warmup_val = warmup_val)
})
ndivergent_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params_post_warmup, which = "n_divergent__", 
                    warmup_val = warmup_val)
})
stepsize_pw <- reactive({
  sp_nuts_check()
  .sampler_param_pw(sampler_params_post_warmup, which = "stepsize__", 
                    warmup_val = warmup_val)
})

.sampler_param_vs_param <- function(p, sp, divergent = NULL, hit_max_td = NULL, 
                                    p_lab, sp_lab, chain = 0, violin = FALSE) {
  xy_labs <- labs(y = if (missing(p_lab)) NULL else p_lab, 
                  x = if (missing(sp_lab)) NULL else sp_lab)
  df <- data.frame(sp = do.call("c", sp), p = c(p))
  if (violin) df$sp <- as.factor(round(df$sp, 4))
  if (!is.null(divergent)) df$divergent <- do.call("c", divergent)
  if (!is.null(hit_max_td)) df$hit_max_td <- do.call("c", hit_max_td)
  
  base <- ggplot(df, aes(sp,p)) + xy_labs + thm 
  if (chain == 0) {
    if (violin) graph <- base + geom_violin(color = vline_base_clr, fill = base_fill) 
    else {
      graph <- base + geom_point(alpha = 0.5, color = base_fill)
      if (!is.null(divergent))
        graph <- graph + geom_point(data = subset(df, divergent == 1), aes(sp,p), 
                                    color = divergent_clr, fill = divergent_fill,
                                    size = 3, shape = div_and_hit_shape)
      if (!is.null(hit_max_td))
        graph <- graph + geom_point(data = subset(df, hit_max_td == 1), aes(sp,p), 
                                    color = hit_max_td_clr, fill = hit_max_td_fill, 
                                    size = 3, shape = div_and_hit_shape)
    }
    return(graph)
  }
  chain_data <- data.frame(sp = sp[, chain], p = p[, chain])
  if (!is.null(divergent)) chain_data$div <- divergent[, chain]
  if (!is.null(hit_max_td)) chain_data$hit <- hit_max_td[, chain]
  chain_clr <- color_vector(ncol(sp))[chain]
  chain_fill <- chain_clr
#   chain_clr <- if (sp_lab != "Sampled Step Size") 
#     vline_overlay_clr else color_vector(ncol(sp))[chain]
#   chain_fill <- if (sp_lab != "Sampled Step Size") 
#     overlay_fill else color_vector(ncol(sp))[chain]
  if (violin) {
    chain_data$sp <- as.factor(round(chain_data$sp, 4))
    graph <- base + 
      geom_violin(color = vline_base_clr, fill = base_fill) + 
      geom_violin(data = chain_data, aes(sp, p), color = chain_clr, 
                  fill = chain_fill, alpha = 0.5)
    return(graph)
  }
  graph <- base + 
    geom_point(alpha = 0.5, color = base_fill) + 
    geom_point(data = chain_data, aes(sp,p), color = chain_fill, alpha = 0.5)
  if (!is.null(divergent))
    graph <- graph + geom_point(data = subset(chain_data, div == 1), aes(sp,p), 
                                color = divergent_clr, fill = divergent_fill,
                                size = 3, shape = div_and_hit_shape)
  if (!is.null(hit_max_td))
    graph <- graph + geom_point(data = subset(chain_data, hit == 1), aes(sp,p), 
                                color = hit_max_td_clr, fill = hit_max_td_fill, 
                                size = 3, shape = div_and_hit_shape)
  graph
}

.p_hist <- function(df, lab, chain = 0) {
  thm <- thm_no_yaxs
  mdf <- reshape2::melt(df, id.vars = "iterations")
  base <- ggplot(mdf, aes(x = value)) + 
    geom_histogram(binwidth = diff(range(mdf$value))/30, fill = base_fill, 
                   color = vline_base_clr, size = 0.05) + 
    labs(x = if(missing(lab)) NULL else lab, y = "") + 
    thm
  if (chain == 0) {
    graph <- base + 
      geom_vline(xintercept = mean(mdf$value), color = vline_base_clr) + 
      geom_vline(xintercept = median(mdf$value), color = vline_base_clr, lty = 2)
    return(graph)
  }
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  chain_clr <- color_vector(ncol(df) - 1)[chain]
  chain_fill <- chain_clr
  base + thm + geom_histogram(data = chain_data,
                              binwidth = diff(range(chain_data$value))/30,
                              fill = chain_fill, alpha = 0.5) +
    geom_vline(xintercept = mean(chain_data$value), color = vline_overlay_clr) + 
    geom_vline(xintercept = median(chain_data$value), 
               color = vline_overlay_clr, lty = 2)
}

.p_trace <- function(df, lab, chain = 0) {
  xy_labs <- labs(x = "Iteration", y = if(missing(lab)) NULL else lab)
  mdf <- reshape2::melt(df, id.vars = "iterations")
  base <- ggplot(mdf, aes(x = iterations, y = value, group = variable))
  interval1 <- mean(mdf$value, na.rm = TRUE) + c(-1,1) * sd(mdf$value, na.rm = TRUE)
  yrange <- range(mdf$value)
  if (yrange[1] > interval1[1]) yrange[1] <- interval1[1]
  if (yrange[2] < interval1[2]) yrange[2] <- interval1[2]
  interval1_rect <- annotate("rect", xmin = -Inf, xmax = Inf,
                             ymin = interval1[1], ymax = interval1[2],
                             fill = base_fill)
  if (chain == 0) {
    graph <- base + interval1_rect +
      geom_path(aes(color = variable), size = 0.25) + 
      scale_color_grey() + 
      scale_y_continuous(limits = yrange)
    return(graph + xy_labs + thm)
  }
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  interval1_rect <- annotate("rect", 
                             xmin = -Inf, 
                             xmax = Inf,
                             ymin = interval1[1], 
                             ymax = interval1[2],
                             fill = base_fill, alpha = 0.5)
  interval2 <- mean(chain_data$value, na.rm = TRUE) + 
    c(-1,1) * sd(chain_data$value, na.rm = TRUE)
  interval2_rect <- annotate("rect", 
                             xmin = -Inf, 
                             xmax = Inf,
                             ymin = interval2[1], 
                             ymax = interval2[2],
                             fill = overlay_fill, alpha = 0.25)
  if (yrange[1] > interval2[1]) yrange[1] <- interval2[1]
  if (yrange[2] < interval2[2]) yrange[2] <- interval2[2]
  graph <- base + 
    interval1_rect + interval2_rect + 
    geom_path(data = chain_data, aes(x = iterations, y = value), 
              color = single_trace_clr, size = 0.5) + 
    scale_y_continuous(limits = yrange)
  
  graph + xy_labs + thm
}

.treedepth_ndivergent_hist <- function(df_td, df_nd, chain = 0, divergent = c("All", 0, 1)) {
  plot_title <- theme(plot.title = element_text(size = 11, hjust = 0))
  plot_theme <- thm + plot_title
  x_lab <- if (divergent == "All") "Treedepth (All)" else paste0("Treedepth (N Divergent = ", divergent,")")
  plot_labs <- labs(x = x_lab, y = "") 
  
  mdf_td <- reshape2::melt(df_td, id.vars = "iterations")
  mdf_nd <- reshape2::melt(df_nd, id.vars = "iterations")
  mdf <- cbind(mdf_td, div = mdf_nd$value)
  plot_data <- if (divergent == "All") mdf else subset(mdf, div == divergent)
  if (nrow(plot_data) == 0) return(NULL)
  
  graph <- ggplot(plot_data, aes(x = factor(value)), na.rm = TRUE) + 
    stat_bin(aes(y=..count../sum(..count..)), width=1, fill = base_fill,
             color = vline_base_clr, size = 0.05) + 
    plot_labs + 
    plot_theme
  if (chain == 0) return(graph)
  chain_clr <- color_vector(ncol(df_td) - 1)[chain]
  chain_fill <- chain_clr
  chain_data <- subset(plot_data, variable == paste0("chain:",chain))
  graph + stat_bin(data = chain_data, aes(y=..count../sum(..count..)), 
                   fill = chain_fill, alpha = 0.5, width = 1)
}

.ndivergent_trace <- function(df, chain = 0) {
  xy_labs <- labs(x = "Iteration", y = "N Divergent")
  y_scale <- scale_y_continuous(breaks = c(0,1))
  
  mdf <- reshape2::melt(df, id.vars = "iterations")
  
  base <- ggplot(mdf, aes(x = iterations, xend = iterations, y = 0, yend = value)) +
    xy_labs + y_scale + thm
  
  if (chain == 0) {
    n_divergent <- sum(mdf$value)
    graph <- base + 
      geom_segment(size = 0.25) + 
      ggtitle(paste(n_divergent, "divergent post-warmup iterations")) 
    return(graph)
  }
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  n_divergent <- sum(chain_data$value)
  base +
    geom_segment(size = 0.25, color = "gray") + 
    geom_segment(data = chain_data, 
                 aes(x = iterations, xend = iterations, y = 0, yend = value), 
                 size = 0.5, color = vline_overlay_clr) + 
    ggtitle(paste(n_divergent, "divergent post-warmup iterations in Chain", chain)) 
}

.stepsize_trace <- function(df, chain = 0) {
  xy_labs <- labs(x = "Iteration", y = "Sampled Step Size")
  
  mdf <- reshape2::melt(df, id.vars = "iterations")
  base <- ggplot(mdf, aes(x = iterations, y = value, group = variable)) + 
    xy_labs + thm
  if (chain == 0) return(base + geom_path(size = 0.5))
  chain_data <- subset(mdf, variable == paste0("chain:",chain))
  base + 
    geom_path(size = 0.5, color = "gray") + 
    geom_path(data = chain_data, aes(x = iterations, y = value), 
              color = vline_overlay_clr)
}

.sampler_param_vs_sampler_param_violin <- function(df_x, df_y, lab_x, lab_y, 
                                                   chain = 0) {
  
  xy_labs <- labs(y = lab_y, x = lab_x)
  df <- data.frame(x = do.call("c", df_x), y = do.call("c", df_y))
  df$x <- as.factor(df$x)
  
  base <- ggplot(df, aes(x,y)) + xy_labs + thm 
  graph <- base + geom_violin(color = vline_base_clr, fill = base_fill) 
  if (chain == 0) return(graph)
  chain_clr <- color_vector(ncol(df_x))[chain]
  chain_fill <- chain_clr
#   chain_clr <- if (lab_x != "Sampled Step Size") 
#     vline_overlay_clr else color_vector(ncol(df_x))[chain]
#   chain_fill <- if (lab_x != "Sampled Step Size") 
#     overlay_fill else color_vector(ncol(df_x))[chain]
  chain_data <- data.frame(x = as.factor(df_x[, chain]), y = df_y[, chain])
  graph + geom_violin(data = chain_data, aes(x,y), color = chain_clr, 
                      fill = chain_fill, alpha = 0.5)
}


.dynamic_trace_diagnostics <- function(param_samps, param_name, chain = 0,
                                       stack = FALSE, grid = FALSE, 
                                       group = NULL) {
  dim_samps <- dim(param_samps)
  if (is.null(dim_samps)) {
    nChains <- 1
  } else {
    nChains <- dim_samps[2]
  }
  if (nChains == 1) {
    param_chains <- xts::as.xts(ts(param_samps, start = 1))
  } else {
    if (chain != 0) {
      param_samps <- param_samps[, chain]
      param_chains <- xts::as.xts(ts(param_samps, start = 1))
    } else {
      param_chains <- xts::as.xts(ts(param_samps[,1], start = 1))
      for (i in 2:nChains) {
        param_chains <- cbind(param_chains, 
                              xts::as.xts(ts(param_samps[,i], start = 1)))
      }
      colnames(param_chains) <- paste0("Chain", 1:nChains)
    }
  }
  `%>%` <- dygraphs::`%>%`
  y_axis_label_remove <- if (stack) "white" else NULL
  clrs <- color_vector(nChains) 
  step_plot <- param_name %in% c("Treedepth", "N Divergent")
  fill_graph <- param_name == "N Divergent"
  stroke_width <- if (step_plot) 0.33 else 0.75
  if (chain != 0) clrs <- clrs[chain]
  dygraphs::dygraph(param_chains, xlab = param_name, ylab = NULL, 
                    group = group) %>%
    dygraphs::dyOptions(colors = clrs, stackedGraph = stack, drawGrid = grid,
                        stepPlot = step_plot, 
                        fillGraph = fill_graph, fillAlpha = 0.5,
                        strokeWidth = 0.75, animatedZooms = TRUE, 
                        drawXAxis = TRUE, drawYAxis = !fill_graph, 
                        drawAxesAtZero = TRUE, axisLineColor = "black") %>%
    dygraphs::dyAxis("x", pixelsPerLabel = 1e4, axisLineWidth = 3) %>%
    dygraphs::dyAxis("y", pixelsPerLabel = 20) %>%
    dygraphs::dyRangeSelector(height = 1, retainDateWindow = TRUE) %>%
    dygraphs::dyLegend(show = "never") %>%
    dygraphs::dyCSS(css = "css/shinyStan_dygraphs.css")
}
