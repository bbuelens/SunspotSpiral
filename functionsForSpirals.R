# Functions

# http://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
localMaxima <- function(x) {
   # Use -Inf instead if x is numeric (non-integer)
   y <- diff(c(-.Machine$integer.max, x)) > 0L
   rle(y)$lengths
   y <- cumsum(rle(y)$lengths)
   y <- y[seq.int(1L, length(y), 2L)]
   if (x[[1]] == x[[2]]) {
      y <- y[-1]
   }
   y
}

# R-statistic for periodicity
Rstat = function(x, p) {
   a = sin(x*2*pi/p)
   b = cos(x*2*pi/p)
   return(sqrt(mean(a)^2 + mean(b)^2))
}

# construct polygons making up a spiral
spiralPoly = function(values,period) {
   # values: numeric vector with values
   # period: how many elements of values in 1 cycle
   # returns data frame with polygons
   
   dat = data.frame(id=1:length(values), value=values)
   
   pol = data.frame(id = rep(dat$id, each=4), x=0, y=0)
   
   deltaTheta = 2 * pi / period
   thisp = 1
   thisc = 1
   
   c = ceiling(length(values) / period) # number of cycles (or part of)
   rOffset = 1/ (c+1)
   rCycle =  1/ (c+1)
   thickness = (2/3) * rCycle
   rCell = rCycle / period
   
   polyCount = 1
   for (thisc in 1:c) {
      if (thisc < c) {
         pmax = period
      } else {
         pmax = length(values) - period * (c-1)
      }
      for (thisp in 1:pmax) {
         rBase = rOffset + (thisc - 1) * rCycle + (thisp-1) * rCell
         pol[polyCount,"x"] = rBase * cos((thisp-1) * deltaTheta)
         pol[polyCount,"y"] = rBase * sin((thisp-1) * deltaTheta)
         pol[polyCount+1,"x"] = (rBase + rCell) * cos(thisp * deltaTheta)
         pol[polyCount+1,"y"] = (rBase + rCell) * sin(thisp * deltaTheta)
         pol[polyCount+2,"x"] = (rBase + rCell + thickness) * cos(thisp * deltaTheta)
         pol[polyCount+2,"y"] = (rBase + rCell + thickness) * sin(thisp * deltaTheta)
         pol[polyCount+3,"x"] = (rBase + thickness) * cos((thisp-1) * deltaTheta)
         pol[polyCount+3,"y"] = (rBase + thickness) * sin((thisp-1) * deltaTheta)
         polyCount = polyCount + 4
      }
   }
   return(merge(dat, pol, by=c("id")))
}

# ggplot useful functions
cleanTheme = theme(axis.line=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   # legend.position="none",
                   panel.background=element_blank(),
                   # panel.border=element_blank(),
                   panel.grid.major=element_blank(),
                   panel.grid.minor=element_blank() #,
                   # plot.background=element_blank()
                  )

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
   r = diameter / 2
   tt <- seq(0,2*pi,length.out = npoints)
   xx <- center[1] + r * cos(tt)
   yy <- center[2] + r * sin(tt)
   return(data.frame(x = xx, y = yy))
}

