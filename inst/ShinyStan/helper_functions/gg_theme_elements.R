# ggplot theme elements to be used as needed  


# transparent background --------------------------------------------------
transparent <- theme(
  panel.background = element_blank(), 
  plot.background = element_blank()
  )


# axes and titles ---------------------------------------------------------
axis_line_color <- "gray20"
axis_color <- theme(
  axis.line = element_line(color = axis_line_color)
  )
axis_labs <- theme(
  axis.title = element_text(face = "bold", size = 13)
  )
title_txt <- theme(
  plot.title = element_text(face = "bold", size = 14)
  )
fat_axis <- theme(
  axis.line.x = element_line(size = 3, color = axis_line_color), 
  axis.line.y = element_line(size = 0.5, color = axis_line_color)
  )
no_yaxs <- theme(
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.y = element_blank()
  )

# facet strips ------------------------------------------------------------
strip_txt <- theme(
  strip.text = element_text(size = 12, face = "bold", color = "white"),
  strip.background = element_rect(color = axis_line_color, fill = axis_line_color)
  )


# horizontal and vertical lines -------------------------------------------
h_lines <- theme(
  panel.grid.major = element_line(size = 0.10, linetype = 3, color = "turquoise4"),
  panel.grid.major.x = element_blank()
  )
v_lines <- theme(
  panel.grid.major = element_line(size = 0.25, linetype = 3, color = "turquoise4"),
  panel.grid.major.y = element_blank()
  )


# legends -----------------------------------------------------------------
no_lgnd <- theme(
  legend.position = "none"
  )
lgnd_bot <- theme(
  legend.position = "bottom", 
  legend.background = element_blank()
  )
lgnd_top <- theme(
  legend.position = "top", 
  legend.background = element_blank()
  )
lgnd_left <- theme(
  legend.position = "left", 
  legend.background = element_blank()
  )
lgnd_right <- theme(
  legend.position = "right", 
  legend.background = element_blank()
  )


# colors and shapes -------------------------------------------------------
base_fill <- "#66a7e0" 
overlay_fill <- "#006dcc" 
vline_base_clr <- "#006dcc"
pt_outline_clr <- "#328ad6"
divergent_fill <- "#ae0001" 
hit_max_td_fill <- "#eeba30"
divergent_clr <-  "black" 
hit_max_td_clr <- "black" 
div_and_hit_shape <- 21

blue_color <- "#006dcc"
