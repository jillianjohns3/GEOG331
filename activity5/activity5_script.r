#install.packages("lubricate")

#load in lubridate
library(lubridate)

#streamflows used to help forcast floods and areas prone to flooding
#graph of streamflow at a point in aa river at a specific time is a hydrograph
#Hydrographs typically show a distinctive peak in discharge following a rainfall event. 
#The duration, magnitude, and shape of the hydrograph peak depend on the amount and duration of rain, the shape of the stream, vegetation, land surface characterstics, and human infrastructure. 

#read in streamflow data
datH <- read.csv("y:\\Data\\activities\\a05\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)    

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("y:\\Data\\activities\\a05\\2049867.csv")                          
head(datP)

#only use most reliable measurements
#data quality flag,"A" meets publication standard (reliable)
datD <- datH[datH$discharge.flag == "A",]


#### define time for streamflow #####
#convert time to decimal days and decimal day of year 
#for easier control over plot ranges

#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay-1)/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + ((datP$decDay-1)/366),
                       datP$year + ((datP$decDay-1)/365))          

#changed bc divided by 365 and 366 would just be zero!

#QUESTION 2


#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#QUESTION 3
nrow(datD)
#see how many rows are in datD 

#QUESTION 4


#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)



#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
    type="l", 
    xlab="Year", 
    ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
    lwd=2,
    ylim=c(0,90),
    xaxs="i", yaxs ="i",#remove gaps from axes
    axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
        )       
axis(1, seq(0,360, by=40), #tick intervals
        lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
        seq(0,80, by=20),
        las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
                lwd=c(2,NA),#lines
                fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
                border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
                 bty="n")#no legend border



#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
    type="l", 
    xlab="Year", 
    ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
    lwd=2,
    ylim=c(0,90),
    xaxs="i", yaxs ="i",#remove gaps from axes
    axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
        )       
axis(1, seq(0,360, by=40), #tick intervals
        lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
        seq(0,80, by=20),
        las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
                lwd=c(2,NA),#lines
                col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
                pch=c(NA,15),#symbols
                 bty="n")#no legend border



#QUESTION 5

datD$month<-month(datesD)
aveF

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$month), FUN="mean")
colnames(aveF) <- c("month","monthlyAve")
sdF <- aggregate(datD$discharge, by=list(datD$month), FUN="sd")
colnames(sdF) <- c("month","monthlySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot 
plot(aveF$month,aveF$monthlyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(-30,60),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$month, rev(aveF$month)),#x coordinates
        c(aveF$monthlyAve-sdF$monthlySD,rev(aveF$monthlyAve+sdF$monthlySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,12, by=1), #tick intervals
     lab=seq(0,12, by=1)) #tick labels
axis(2, seq(-80,80, by=20),
     seq(-80,80, by=20),
     las = 2)#show ticks at 90 degree angle


#add a line of 2017 discharge
aveF17 <- aggregate(datD$discharge[datD$year == 2017], by=list(datD$month[datD$year == 2017]), FUN="mean")
colnames(aveF17) <- c("month","monthlyAve")
sdF17 <- aggregate(datD$discharge[datD$year == 2017], by=list(datD$month[datD$year == 2017]), FUN="sd")
colnames(sdF17) <- c("month","monthlySD")
lines(x=aveF17$month,y=aveF17$monthlyAve,col="blue")
legend("topright", c("mean","1 standard deviation","2017"), #legend items
       lwd=c(2,NA,2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"blue"),#colors
       pch=c(NA,15,NA),#symbols
       bty="n")#no legend border


#QUESTION 6
#looking at graph made in question 5


#QUESTION 7 






#length(datP$year=="2007"[datP$decDay < 2])

precipfunction <- for(i in datP$decDay){
  numprecip<- c()
  if(length(datP$decDay<i)==24){
    numprecip<-c(numprecip,24)
  } else{
    numprecip<-c(numprecip,length(datP$decDay<i))
  }
  return(numprecip)
  i<i+1
}

datD16$season<- lapply(datD16$doy,datD16function)



#Use aggregate function???
aggregate







#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]


min(hydroD$discharge)


#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl


par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
    hydroD$discharge, 
    type="l", 
    ylim=c(yl,yh), 
    lwd=2,
    xlab="Day of year", 
    ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
 polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
        c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
        col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}







#QUESTION 8
#subsest discharge and precipitation within range of interest
hydroDwinter <- datD[datD$doy >= 22 & datD$doy < 23 & datD$year == 2013,]
hydroPwinter <- datP[datP$doy >= 22 & datP$doy < 23 & datP$year == 2013,]

min(hydroDwinter$discharge)
#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
ylwinter <- floor(min(hydroDwinter$discharge))-1
#celing rounds up to the integer
yhwinter <- ceiling(max(hydroDwinter$discharge))+1
#minimum and maximum range of precipitation to plot
plwinter <- 0
pmwinter <-  ceiling(max(hydroPwinter$HPCP))+.5
#scale precipitation to fit on the 
hydroPwinter$pscale <- (((yhwinter-ylwinter)/(pmwinter-plwinter)) * hydroPwinter$HPCP) + ylwinter
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroDwinter$decDay,
     hydroDwinter$discharge, 
     type="l", 
     ylim=c(ylwinter,yhwinter), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroPwinter)){
  polygon(c(hydroPwinter$decDay[i]-0.017,hydroPwinter$decDay[i]-0.017,
            hydroPwinter$decDay[i]+0.017,hydroPwinter$decDay[i]+0.017),
          c(ylwinter,hydroPwinter$pscale[i],hydroPwinter$pscale[i],ylwinter),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}





library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()



#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
    geom_violin()





#QUESTION 9
library(ggplot2)
#have to make seasons a column of season as factors
datD16<-datD[datD$year=="2016",]


#2016 seasons
#winter: doy<80 >=355
#spring: >=80 <172
#summer: >=172 <286
#fall: >=286 <355


datD16function <- function(doy){
  if(doy < 80 | doy >= 355){
    seasonname<- "Winter"
  } else if(doy >= 80 & doy < 172){
    seasonname<- "Spring"
  } else if(doy >= 172 & doy < 286){
    seasonname<- "Summer"
  } else{
    seasonname<- "Fall"
  }
  return(seasonname)
}


datD16$season<- lapply(datD16$doy,datD16function)
datD16$season<-unlist(datD16$season)
datD16$season<-as.factor(datD16$season)
is.factor(datD16$season)


#make a violin plot
ggplot(datD16, aes(season,discharge)) + 
  geom_violin()+ ggtitle("2016 Discharge by Season")






#2017
library(ggplot2)
#have to make seasons a column of season as factors
datD17<-datD[datD$year=="2017",]


#2017 seasons
#winter: doy<79 >=356
#spring: >=79 <171
#summer: >=171 <285
#fall: >=285 <356


datD17function <- function(doy){
  if(doy < 79 | doy >= 356){
    seasonname<- "Winter"
  } else if(doy >= 79 & doy < 171){
    seasonname<- "Spring"
  } else if(doy >= 171 & doy < 285){
    seasonname<- "Summer"
  } else{
    seasonname<- "Fall"
  }
  return(seasonname)
}


datD17$season<- lapply(datD17$doy,datD17function)
datD17$season<-unlist(datD17$season)
datD17$season<-as.factor(datD17$season)
is.factor(datD17$season)


#make a violin plot
ggplot(datD17, aes(season,discharge)) + 
  geom_violin() + ggtitle("2017 Discharge by Season")



