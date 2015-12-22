
# Circular analysis and spiral plot of Sunspot series
library(ggplot2)
library(caTools)
source("functionsForSpirals.R")
myBlue = "blue"
myRed = "darkred"

### read data
S = read.table("..//SN_ms_tot_V2.0.csv",header = FALSE,sep = ";",dec = ".") # smoothed
names(S) = c("year","month","yearfrac","sn","stderr","nobs","flag")
Sraw = read.table("..//SN_m_tot_V2.0.csv",header = FALSE,sep = ";",dec = ".") # raw
names(Sraw) = c("year","month","yearfrac","snRaw","stderr","nobs","flag")
dim(S) == dim(Sraw)
S$snRaw = Sraw$snRaw
S = S[S$sn!=-1, ]
S$monthCount = 1:nrow(S)
rm(Sraw)
nrow(S) # number of months in data set

### Determine peaks
S$snSmooth = runmean(S$sn, k = 30, alg = "fast", endrule = "constant", align = "center")
locmax = localMaxima(S$snSmooth)
length(locmax) # OK, 24
S$top = 0
S[locmax, "top"] = 1

S$label = ""
S$label[S$month==1] = S$year[S$month==1]

myBreaks = seq(6,3190,by=12*50)
myLabels = seq(1750, 2015, by=50)

ggplot(S, aes(x=monthCount, y=snRaw)) + 
   theme_bw() + 
   geom_line(colour="grey60") + 
   geom_line(data=S, aes(x=monthCount, y=sn), colour="black") + 
   geom_vline(xintercept = locmax, colour="blue") +
   xlab("") + 
   ylab("") +
   scale_x_continuous(breaks=myBreaks, labels=myLabels)
# ggsave("sunspots_classic.png",width=4,height=2,units="in",scale=2)
# ggsave("sunspots_classic.tiff",width=4,height=2,units="in",scale=2)

### periodicity

M = subset(S, top==1) # the maxima

p = seq(12,220)
myR = numeric()
for (i in 1:length(p)) {
   myR[i] = Rstat(x = M$monthCount, p = p[i])
}
# plot(p, myR, type="l")
p[which.max(myR)] # exactly 11 years
p[which.max(myR)] / 12

qplot(x=p, y=myR) + geom_line(colour="#252525") + 
   theme_bw() + 
   xlab("period (months)") + ylab("R-statistic") + 
   geom_vline(xintercept=p[which.max(myR)], colour=myRed, size=1)
# ggsave(filename = "sunspots_rstat.png", width = 4, height = 2,units = "in", scale=2)
# ggsave(filename = "sunspots_rstat.tiff", width = 4, height = 2,units = "in", scale=2)

thisp = p[which.max(myR)] # periodicity in months

### Construct spirals

snPolys = spiralPoly(S$sn, period=thisp)
dim(snPolys)
# midpoints of the polys:
midpoints = data.frame(x = aggregate(snPolys$x, by=list(snPolys$id), FUN = mean),
                       y = aggregate(snPolys$y, by=list(snPolys$id), FUN = mean))
peaks = midpoints[locmax,]

# Circle and segments
circle1 = circleFun(diameter=2.05, npoints=200)
circle2 = circleFun(diameter=2.10, npoints=11+1)
circlab = circleFun(center = c(0.01,0.01), diameter = 2.25, npoints = 11+1)
circlab = circlab[-1,]
circlab$lab = as.character(1:11)
circlab$lab[11] = "0/11"


ggp = ggplot(snPolys, aes(x=x, y=y)) + 
   geom_polygon(aes(fill=value, group=id)) + 
   theme(legend.position="right", legend.title=element_blank()) +
   guides(fill = guide_colorbar(barwidth = 0.5, barheight = 15,draw.ulim=TRUE)) + 
   scale_fill_gradient(low="#386cb0", high="#ffff99" , guide="colourbar", breaks = seq(0,300,25)) + 
   coord_fixed() + 
   cleanTheme + 
   theme(panel.background = element_rect(fill = "#f7f7f7", colour = "black")) +
   geom_path(data=circle1,aes(x,y)) +
   geom_segment(data=circle2, aes(x=0, xend=x, y=0, yend=y), colour="#636363", alpha=0.7) +
   geom_text(data=circlab, aes(x=x,y=y,label=lab),size=3) 
ggp

ggp + 
    geom_point(data=peaks, aes(x=x.x, y=y.x), colour=myBlue, size=1) + 
    geom_segment(data=peaks, aes(x=0, xend=x.x, y=0, yend=y.x), colour=myBlue) 

radius = 2.05 / 2
angularOffset = 0.5 * 2 * pi / thisp
P = data.frame(x = radius * cos(2 * pi * M$monthCount / thisp - angularOffset),
               y = radius * sin(2 * pi * M$monthCount / thisp - angularOffset))
Px = mean(P$x / radius)
Py = mean(P$y / radius)
Rstat = sqrt(Px^2 + Py^2)
Rpnt = data.frame(x=Px,y=Py)

ggp +    
   # on and to the circle
   geom_segment(data=P, aes(x=0, xend=x, y=0, yend=y), colour=myBlue) + 
   geom_point(data=P, aes(x=x, y=y), col=myBlue, size=2) + 
   # maxima on the spiral
   geom_point(data=peaks, aes(x=x.x, y=y.x), col=myBlue, size=1.2, shape=19) +
   # R-statistic
   geom_segment(data=Rpnt, aes(x=0, xend=x, y=0, yend=y), colour=myRed, size=1) + 
   geom_point(data=Rpnt, aes(x=x,y=y), size=4, colour=myRed, shape=18) 
# ggsave(filename = "sunspots_spiral1.png", width = 3, height=2,units = "in", scale=2)
# ggsave(filename = "sunspots_spiral1.tiff", width = 3, height=2,units = "in", scale=2)

### arty plot
library(colorspace)
mypalette=heat_hcl(n = 10)

gga = ggplot(snPolys, aes(x=x, y=y)) + 
   geom_polygon(aes(fill=value, group=id)) + 
   theme(legend.position="right", legend.title=element_blank()) +
   guides(fill = guide_colorbar(barwidth = 0.5, barheight = 15,draw.ulim=TRUE)) + 
   scale_fill_gradientn(colours = mypalette,  guide="colourbar", breaks = seq(0,300,25)) +
   coord_fixed() + 
   cleanTheme + 
   theme(plot.background = element_rect(fill = "black")) +
   geom_path(data=circle1,aes(x,y),colour="#bdbdbd") +
   geom_segment(data=circle2, aes(x=0, xend=x, y=0, yend=y), colour="#d9d9d9", alpha=0.7) +
   geom_text(data=circlab, aes(x=x,y=y,label=lab),size=3,colour="#bdbdbd") +
   theme(legend.background = element_rect(fill = "black"), legend.text = element_text(colour = "#bdbdbd")) +
   geom_point(data=peaks, aes(x=x.x, y=y.x), col=myBlue, size=1.0, shape=19)

gga

# ggsave(filename = "sunspots_spiral2.png", width = 3, height=2,units = "in", scale=2)
# ggsave(filename = "sunspots_spiral2.tiff", width = 3, height=2,units = "in", scale=2)







