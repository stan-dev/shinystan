# Make custom saving function that works better with gg_animate()
# Based on saveVideo function from package animate
# Modified to use resolution and only involve png output for simplicity

# in_dir <- function (wd, expr) 
# {
#   owd = setwd(wd)
#   on.exit(setwd(owd))
#   expr
# }



outputVideo <- function(in_plots, video.name = 'animation.mp4', img.name = 'Rplot', ffmpeg = animation::ani.options('ffmpeg'),
                        other.opts=NULL, width,height,resolution,frame_speed,num_cores=1) {
  if(num_cores>1) {
    num_chain <- num_cores
  } else {
    num_chain <- 1
  }
  
  
  if(!dir.exists(dirname(video.name))){
    dir.create(dirname(video.name))
  }
  
  owd <- setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)
  
  if(is.null(other.opts)) {
    
    other.opts <- paste0("-c:v libvpx -pix_fmt yuv420p"," -crf 7 -b:v 2M -c:a libvorbis")
  }
file.ext <- 'png'
num <-  ifelse(file.ext == 'pdf', '', '%d')
unlink(paste(img.name, '*.', file.ext, sep = ''))
save_dir <- tempdir()
img.fmt_template <-  paste(img.name, num, '.', file.ext, sep = '')
img.fmt_template <-  file.path(save_dir, img.fmt_template)


over_plots <- function(x) {
png(file.path(save_dir,paste0('Rplot',x,".", file.ext)), width = width,
        height = height,res=resolution)
plot_ggplot_build(in_plots$plots[[x]])
dev.off()
}
parallel::mclapply(1:length(in_plots$plots),over_plots,mc.cores=num_chain)
## call FFmpeg
ffmpeg <-  paste('ffmpeg', '-y', '-framerate',frame_speed, '-i',
               basename(img.fmt_template), other.opts, basename(video.name))
message('Executing: ', ffmpeg)
cmd <- system(ffmpeg,intern=TRUE)

if (class(cmd)!= 'try-error') {
  setwd(owd)
  if(!grepl(save_dir,video.name,fixed = T))
    file.copy(file.path(save_dir, basename(video.name)), video.name, overwrite = TRUE)
  message('\n\nVideo has been created at: ',
          output.path <- normalizePath(video.name))
} else {
  
  message('\n\n Video file creation via ffmpeg failed.')
}


}

# This function is from the gg_animate package, but it is not exported, so it is used here

plot_ggplot_build <- function (b, newpage = is.null(vp), vp = NULL) 
{
  if (newpage) {
    grid::grid.newpage()
  }
  grDevices::recordGraphics(requireNamespace("ggplot2", quietly = TRUE), 
                            list(), getNamespace("ggplot2"))
  gtable <- ggplot_gtable(b)
  if (is.null(vp)) {
    grid::grid.draw(gtable)
  }
  else {
    if (is.character(vp)) 
      grid::seekViewport(vp)
    else grid::pushViewport(vp)
    grid::grid.draw(gtable)
    grid::upViewport()
  }
}

# helper function for parallel

over_plots <- function(x,counter_data,directory) {
  use_data <- counter_data[counter_data$parallel_counter==x,]
  files_dir <- file.path(directory,"Rplots_core_",x,"_%03d",".png")
  png(files_dir)
  for(j in 1:100) {
    for(i in use_data$id)
      plot(plots[[i]])
  }
  dev.off()
}

# This function is a modified version of the gg_animate_save function 

shiny_animate_save <- function(g, filename = NULL,frame_speed,
                               width,height,resolution,parallel=TRUE,num_cores=1) {

  g$filename <- filename
  
  # temporarily move to directory (may be current one, that's OK)
  # this helps with animation functions like saveGIF that work only in
  # current directory
  
  if(parallel==TRUE && num_cores>1) {
    num_cores <- num_cores
  } else {
    num_cores <- 1
  }
  withr::with_dir(dirname(filename), {
    outputVideo(g, basename(filename),frame_speed=frame_speed,width=width,height=height,resolution=resolution,num_cores=num_cores)
  })
  
  g$src <- base64enc::dataURI(file = filename, mime = 'video/webm')
  g$mime_type <- 'video/webm'
  g$saved <- TRUE
  g
}