library(dplyr)
library(raster)
library(RColorBrewer)
library(rnaturalearth)

###borders 
ne_countries()
granite <- list("sp.polygons", ne_countries(scale =10),col = "black",lwd = 1.5,first=FALSE)

#######read ndvi file
ndvi1 <- readRDS("tab/ndvi.rds")
head(ndvi1)
ndvi1.xyz  <- rasterFromXYZ(ndvi1, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
#plot(ndvi1.xyz)
#### read precipitation file
prec1 <- readRDS("tab/prec.rds")
head(prec1)
prec1.xyz  <- rasterFromXYZ(prec1, crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
#plot(prec1.xyz)
### stacks the rasters
########start the spearman corelatiion  

raster.st <- stack(prec1.xyz,ndvi1.xyz)

r1 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit65",names(x)))], x[c(grep("65f",names(x)))], method='spearman'))
r2 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit_81",names(x)))], x[c(grep("81f",names(x)))], method='spearman'))
r3 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit_97",names(x)))], x[c(grep("97f",names(x)))], method='spearman'))
r4 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit_113",names(x)))], x[c(grep("113f",names(x)))], method='spearman'))
r5 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit_129",names(x)))], x[c(grep("129f",names(x)))], method='spearman'))
r6 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit_145",names(x)))], x[c(grep("145f",names(x)))], method='spearman'))
r7 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit_161",names(x)))], x[c(grep("161f",names(x)))], method='spearman'))
r8 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit_177",names(x)))], x[c(grep("177f",names(x)))], method='spearman'))
r9 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit_193",names(x)))], x[c(grep("193f",names(x)))], method='spearman'))
r10 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit_209",names(x)))], x[c(grep("209f",names(x)))], method='spearman'))
r11 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit_225",names(x)))], x[c(grep("225f",names(x)))], method='spearman'))
r12 <- calc(raster.st, fun=function(x) cor(x[c(grep("Compozit_241",names(x)))], x[c(grep("241f",names(x)))], method='spearman'))

cor.stack <- stack(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12) #it works!
#####
##error to cor.test() for p value 
##r1 <- calc(raster.st, fun=function(x) cor.test(x[c(grep("Compozit65",names(x)))], x[c(grep("65f",names(x)))], method='spearman'))
#### start  layout 


luni <- c("06Mar-21Mar", 
          "22Mars-06Apr",
          "07Apr-22Apr",
          "23Apr-08May", 
          "09May-24May",
          "25May-9Jun",
          "10Jun-25Jun",
          "26Jun-11Jul",
          "12Jul-27Jul",
          "28Jul-12Aug",
          "13Aug-28Aug",
          "29Aug-13Sep")
names(cor.stack) <- luni
names(cor.stack) <- sub("X", "C_", names(cor.stack))

###culori
brks<-c(-1.0,-.8,-.6,-.4,-.2,0.0,.2,.4,.6,.8,1.0)
cols= rev(brewer.pal(n = length(brks),"RdBu"))
nb <- length(brks)-1
brk2<-seq(2,(length(brks)-1),1)
brk2<-brks[brk2]
arg <- list(at=brk2, labels=brk2)

###

pc<-spplot(cor.stack,#xlim=c(24,27.5),ylim=c(44.5,46),
           scales=list(draw=T), auto.key=list(cex=1.3),sp.layout=list(granite),
           colorkey=list(space='bottom', at = brks,height = 0.5,width=0.75,
                         labels=list(at=brk2,labels=brk2),axis.text=list(cex=1)),at=brks,cex=1.3,
           #legend=list(top=list(fun=grid::textGrob("ore", x=1.11,y=-1.2))),
           col.regions=cols,layout=c(3,4),
           par.settings = list(strip.background=list(col="#f0f0f0")),
           as.table = T)

png("Correlation_2001_2019.png", width = 2800,height = 2600,res =200)
pc
dev.off()

