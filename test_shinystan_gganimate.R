require(tweenr)

test1  <- exfit2@posterior_sample
test2  <- array(test1[,,c('mu','z[1,1]','z[1,2]')],c(100,3))
test2 <- test2[1:10,]
colnames(test2)  <- c('mu','z[1,1]','z[1,2]')
test2 <- as.data.frame(test2)
test2$time <- 1:nrow(test2)
test2$id <- 1
test2$ease <- 'quadratic-in-out'
test2 <- tween_elements(test2,'time','id','ease',nframes=100)
test3  <- reshape2::melt(test2,id.vars=c('mu','time','.frame','.group'),value.name='x')

testobj  <- ggplot(test3,aes(y=mu,x=x,colour=variable,frame=.frame)) + geom_point(size=3) + theme_bw() + geom_path(aes(cumulative=TRUE),size=0.1,alpha=0.5)
animation::ani.options(interval = 1/16)
outfile <- tempfile(fileext='.mp4')
testobj_animated  <- gganimate::gg_animate(testobj,filename='this_video.mp4')
gg_animate_save(testobj_animated,filename='this_video.mp4')
print(testobj_animated)


# library(ggplot2)
# library(gganimate)
# #library(ggforce)
# library(tweenr)
# 
# # Making up data
# d <- data.frame(x = rnorm(20), y = rnorm(20), time = sample(100, 20), alpha = 0, 
#                 size = 1, ease = 'elastic-out', id = 1:20, 
#                 stringsAsFactors = FALSE)
# d2 <- d
# d2$time <- d$time + 10
# d2$alpha <- 1
# d2$size <- 3
# d2$ease <- 'linear'
# d3 <- d2
# d3$time <- d2$time + sample(50:100, 20)
# d3$size = 10
# d3$ease <- 'bounce-out'
# d4 <- d3
# d4$y <- min(d$y) - 0.5
# d4$size <- 2
# d4$time <- d3$time + 10
# d5 <- d4
# d5$time <- max(d5$time)
# df <- rbind(d, d2, d3, d4, d5)
# 
# # Using tweenr
# dt <- tween_elements(df, 'time', 'id', 'ease', nframes = 500)
# 
# p <- ggplot(data = dt) + 
#   geom_point(aes(x=x, y=y, size=size, alpha=alpha, frame = .frame)) + 
#   scale_size(range = c(0.1, 20), guide = 'none') + 
#   scale_alpha(range = c(0, 1), guide = 'none') 
# 
# gg_animate(p, title_frame = F)
# 
# data <- data.frame(
#   x = c(1, 2, 2, 1, 2, 2),
#   y = c(1, 2, 2, 2, 1, 1),
#   time = c(1, 4, 10, 4, 8, 10),
#   group = c(1, 1, 1, 2, 2, 2),
#   ease = rep('cubic-in-out', 6)
# )
# 
# data <- tween_elements(data, 'time', 'group', 'ease', nframes = 100)
