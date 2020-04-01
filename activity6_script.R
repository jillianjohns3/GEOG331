#activity 6 woot woot

#install the packages first
#install.packages(c("raster","sp","rgdal","rgeos","plyr"))
setwd("~/Colgate/junior year/spring semester/env data science/activities/activity 6")

#load all the packages to your environment
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in vector data
g1966<- readOGR("data\\GNPglaciers_1966.shp")
g1998<- readOGR("data\\GNPglaciers_1998.shp")
g2005<- readOGR("data\\GNPglaciers_2005.shp")
g2015<- readOGR("data\\GNPglaciers_2015.shp")


#format of the data
str(g2015)

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]

#vector object, you can find projection
g1966@proj4string

#QUESTION 1

#map vector data and show different colors for a data value
spplot(g1966, "GLACNAME")

#check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
              "N. Swiftcurrent Glacier",
              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                        "Miche Wabun Glacier",
                        as.character(g2015@data$GLACNAME)))


#read in rgb imagery from landsat
redL <- raster("glacier_09_05_14\\l08_red.tif")
greenL <- raster("glacier_09_05_14\\l08_green.tif")
blueL <- raster("glacier_09_05_14\\l08_blue.tif")

#check coordinate system
redL@crs

#the coordinate system is the same as the vector data above. This means that you can plot them together without having to reproject.

#It will be easier to plot these rasters together if we turn them into a brick. A brick is a series of raster files with the same extent and resolution.
#Make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#hard to see the detalis at the scale for entire park, so use ext to zoom in on specific areas
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("NDVI\\NDVI_",ndviYear[i],".tif"))
  
}

str(NDVIraster[[1]])

#get projection
NDVIraster[[1]]@crs

#QUESTION 2

rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)


plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)


#code from tutorial
plot(NDVIraster[[1]])




#QUESTION 3


#set up years to read in
ndviYear <- seq(2003,2016)
#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("NDVI\\NDVI_",ndviYear[i],".tif"))
  
}

#par will graph both the plots
par(mfrow=c(1,2))
#Graph the plot for the first year of the NDVI data which is the 2003 raster data
plot(NDVIraster[[1]])
#Graph the 1966 polygons
plot(g1966, axes = TRUE)

#get projection
NDVIraster[[1]]@crs


#code from tutorial
#Vector Analysis: Glacier Retreat
#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)



#QUESTION 4

#shows one graph
par(mfrow=c(1,1))

#Graph the plot for the fourteenth year of the NDVI data which is the 2015 raster data
#how would I do this without figuring it out that it was the fourteenth year?
plot(NDVIraster[[14]])

#add on top of the NDVI raster data
plot(g2015p, add = TRUE)


#code from tutorial
#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

#join all the data together in a table not asssociated with the shapefile 
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
} 

#QUESTION 5

#find percent change in glacier size
gAll$percentchange <- (gAll$a1966m.sq - gAll$a2015m.sq)/(gAll$a1966m.sq) * 100
g2015p@data$percentchange <- gAll$percentchange
spplot(g2015p,"percentchange",col=NA)



#code from tutorial
#polygon that shows the difference in glaciers between 2015 and 1966
diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)

#plot with NDVI
plot(NDVIraster[[13]], axes=TRUE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

gArea(diffPoly, byid= TRUE)
diffPoly@polygons[[1]]@area
area(diffPoly, filename="", na.rm=FALSE, weights=TRUE)

#QUESTION 6

gAllMax <- subset(gAll,percentchange==max(percentchange))
gAllMax
#shows that the largest percent change is on the Boulder Glacier which had a percent change of 84%
g1966maxperchange <- subset(g1966, GLACNAME==gAllMax$GLACNAME)
g1998maxperchange <- subset(g1998, GLACNAME==gAllMax$GLACNAME)
g2005maxperchange <- subset(g2005, GLACNAME==gAllMax$GLACNAME)
g2015maxperchange <- subset(g2015, GLACNAME==gAllMax$GLACNAME)


par(mfrow=c(1,1))
plotRGB(rgbL,ext=c(272000,275600,5426000,5428500), stretch="lin", main="Boulder Glacier: 84% Loss from 1966 to 2015")
plot(g1966maxperchange, col="green", border=NA, add=TRUE)
plot(g1998maxperchange, col="orange", add=TRUE, border=NA)
plot(g2005maxperchange, col="yellow", add=TRUE, border=NA)
plot(g2015maxperchange, col="red", add=TRUE, border=NA)


#code from tutorial
#Raster Data Analysis

#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)


#QUESTION 7


#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)









#QUESTION 8

#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
glacier500mraster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(glacier500mraster)

#glaciers to raster data
glacierraster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacierZones <- glacier500mraster - glacierraster
plot(glacierZones)






#QUESTION 9

meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)


g2015p@data$meanChange <- meanChange[1:39]
spplot(g2015p,"meanChange",col=NA)


#QUESTION 10

#QUESTION 11
#??

g2015p@data$NDVIcol <- ifelse(g2015p@data$NDVImean<0.4,"blue","red")
plot(g2015p, add=TRUE, col=paste(g2015p@data$NDVIcol),border=FALSE)


#QUESTION 12

#QUESTION 13

