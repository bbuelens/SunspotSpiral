
# toy example to demonstrate methods

library(ggplot2)
source("functionsForSpirals.R")

myBlue = "blue"
myRed = "darkred"

# events
n = 8
meanp = 50
periods = c(65, 45, 43, 51, 42, 51, 52, 53)
y = cumsum(periods)

# assume these are the max of sine cycles
yy = numeric()
for (i in 1:length(y)) {
   if (i==1) { 
      startx = 1
   } else {
      startx = y[i-1]
   }
   stopx =  y[i]
   nn = stopx - startx + 1
   yy[startx:stopx] = cos(2 * pi * seq(1:nn)/nn)
   if (i==length(y)) { # nog een achter plakken
      startx = stopx
      stopx = stopx + nn
      yy[startx:(stopx-1)] = cos(2 * pi * seq(1:nn)/nn)
   }
}
plot(yy, type="l")
abline(v=y, col="blue")

# function to plot a circle 
# http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
   r = diameter / 2
   tt <- seq(0,2*pi,length.out = npoints)
   xx <- center[1] + r * cos(tt)
   yy <- center[2] + r * sin(tt)
   return(data.frame(x = xx, y = yy))
}

circ = circleFun(diameter = 2, npoints = 100)
# ggplot(circ,aes(x,y)) + 
#    geom_path() + 
#    coord_fixed() + 
#    theme_bw()

thisp = 48 # select a period to analyze

P = data.frame(x = cos(2 * pi * y / thisp),
               y = sin(2 * pi * y / thisp))
Px = mean(P$x)
Py = mean(P$y)
Rstat = sqrt(Px^2 + Py^2)
Rpnt = data.frame(x=Px,y=Py)

ggplot(circ,aes(x,y)) + 
   geom_path() + 
   coord_fixed() + 
   theme_bw() + 
   geom_segment(data=P, aes(x=0, xend=x, y=0, yend=y), colour=myBlue) + 
   geom_point(data=P, aes(x=x, y=y), col=myBlue, size=3) + 
   geom_point(data=Rpnt, aes(x=x, y=y), col=myRed, size=4) + 
   geom_segment(data=Rpnt, aes(x=0, xend=x, y=0, yend=y), colour=myRed, size=1.2) +
   theme(axis.ticks = element_blank(), axis.text = element_blank()) + 
   theme(axis.title = element_blank()) 

# ggsave(filename = "example_p48.tiff", width = 2, height = 2,units = "in",scale=2)
# ggsave(filename = "example_p48.png", width = 2, height = 2,units = "in",scale=2)

# ggsave(filename = "example_p28.tiff", width = 2, height = 2,units = "in",scale=2)
# ggsave(filename = "example_p28.png", width = 2, height = 2,units = "in",scale=2)

pseq = seq(10,100)
rstat = numeric()
for (i in 1:length(pseq)) {
   rstat[i] = Rstat(y, pseq[i])
}

# where is R statistic maximal?
which.max(rstat)
pseq[which.max(rstat)] # estimated period

# R-statistic plot
qplot(x=pseq, y=rstat) + geom_line(colour="#252525") + 
   theme_bw() + 
   xlab("period") + ylab("R-statistic") + 
   geom_vline(xintercept=48, colour=myRed, size=1)
# ggsave(filename = "example_rstat.png", width = 4, height = 2,units = "in", scale=2)
# ggsave(filename = "example_rstat.tiff", width = 4, height = 2,units = "in", scale=2)

### Spiral plot gizmo ##########################################################

pp = spiralPoly(values = yy, period = 48)

aa = data.frame(x=1:length(yy), y=yy)
bb = data.frame(x=y,y=1)

ggplot(aa, aes(x=x,y=y)) + geom_line() + 
   theme_bw() + 
   theme(axis.title = element_blank()) +
   theme(legend.position="none") +
   geom_point(data=bb,aes(x=x, y=y), size=3, colour=myBlue)
# ggsave("example_series.png",width=4,height=2,units="in",scale=2)
# ggsave("example_series.tiff",width=4,height=2,units="in",scale=2)

circ2 = circleFun(diameter = 2.2, npoints = 100)
circseg = circleFun(diameter = 2.3, npoints = 9)

circlab = circleFun(center = c(0.01,0.01), diameter = 2.5, npoints = 9)
circlab = circlab[-1,]
circlab$lab = as.character(seq(from = 6,to = 48,by = 6))
circlab$lab[8] = "0/48"

myp = ggplot(pp, aes(x=x, y=y)) + 
   geom_polygon(aes(fill=value, group=id), colour="black") + 
   scale_fill_gradient(low="#090909", high="white" , guide="colourbar") + 
   coord_fixed() + 
   theme_bw() + 
   theme(axis.ticks = element_blank(), axis.text = element_blank()) + 
   theme(axis.title = element_blank()) + 
   geom_path(data=circ2,aes(x,y)) +
   geom_segment(data=circseg, aes(x=0, xend=x, y=0, yend=y), colour="#636363") +
   geom_text(data=circlab, aes(x=x,y=y,label=lab),size=4)  +
   theme(legend.position="right", legend.title=element_blank()) +
   guides(fill = guide_colorbar(barwidth = 0.5, barheight = 15))
myp
# ggsave(filename = "example_spiral.png", width = 3, height=2,units = "in", scale=2)
# ggsave(filename = "example_spiral.tiff", width = 3, height=2,units = "in", scale=2)

# now same spiral, with maxima plotted on top
# midpoints of the polygons:
midpoints = data.frame(x = aggregate(pp$x, by=list(pp$id), FUN = mean),
                       y = aggregate(pp$y, by=list(pp$id), FUN = mean))
peaks = midpoints[y,] # the peaks

myp +    
   geom_segment(data=peaks, aes(x=0, xend=x.x, y=0, yend=y.x), colour=myBlue) + 
   geom_point(data=peaks, aes(x=x.x, y=y.x), col=myBlue, size=2)  
 
# ggsave(filename = "example_combi.png", width = 3, height=2,units = "in", scale=2)
# ggsave(filename = "example_combi.tiff", width = 3, height=2,units = "in", scale=2)

# alternative, with circular analysis points and R-stat
radius = 1.1
angularOffset = 0.5 * 2 * pi / thisp
P = data.frame(x = radius * cos(2 * pi * y / thisp - angularOffset),
               y = radius * sin(2 * pi * y / thisp - angularOffset))
Px = mean(P$x / radius)
Py = mean(P$y / radius)
Rstat = sqrt(Px^2 + Py^2)
Rpnt = data.frame(x=Px,y=Py)

# for shape codes, see: http://sape.inf.usi.ch/quick-reference/ggplot2/shape

myp +    
   # on and to the circle
   geom_segment(data=P, aes(x=0, xend=x, y=0, yend=y), colour=myBlue) + 
   geom_point(data=P, aes(x=x, y=y), col=myBlue, size=2) + 
   # maxima on the spiral
   geom_point(data=peaks, aes(x=x.x, y=y.x), col=myBlue, size=1.2, shape=19) +
   # R-statistic
   geom_segment(data=Rpnt, aes(x=0, xend=x, y=0, yend=y), colour=myRed, size=1) + 
   geom_point(data=Rpnt, aes(x=x,y=y), size=4, colour=myRed, shape=18) 

# ggsave(filename = "example_combi_alt1.png", width = 3, height=2,units = "in", scale=2)
# ggsave(filename = "example_combi_alt1.tiff", width = 3, height=2,units = "in", scale=2)

