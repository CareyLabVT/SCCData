#script written to create daily figures that are sent to lucky SCC team members :)
#written by CCC, last edited 12 Feb 2019 by Vahid-Dan

if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)

#download data files to working directory
output_dir <- "~/data/SCCData/daily-email/"
download.file('https://github.com/CareyLabVT/SCCData/raw/carina-data/FCRmet.csv','FCRmet.csv')
download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv','Catwalk.csv')
download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/FCRWaterLevel.csv','FCRWaterLevel.csv')
download.file('https://github.com/CareyLabVT/SCCData/raw/diana-data/FCRweir.csv','FCRweir.csv')

metheader<-read.csv("FCRmet.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
metdata<-read.csv("FCRmet.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(metdata)<-names(metheader) #combine the names to deal with Campbell logger formatting

end.time <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H:%M")), tzone = "Etc/GMT+5") #gives us current time with rounded minutes in EDT
start.time <- end.time - days(5) #to give us five days of data for looking at changes
full_time <- seq(start.time, end.time, by = "min") #create sequence of dates from past 5 days to fill in data

obs <- array(NA,dim=c(length(full_time),9)) #create array that will be filled in with 8 columns
met_timechange=max(which(metdata$TIMESTAMP=="2019-04-15 10:19:00")) #shows time point when met station was switched from GMT -4 to GMT -5
metdata$TIMESTAMP<-as.POSIXct(strptime(metdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned
metdata$TIMESTAMP[c(1:met_timechange-1)]<-with_tz(force_tz(metdata$TIMESTAMP[c(1:met_timechange-1)],"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set
metdata=metdata[-c(met_timechange-1),]

if (length(na.omit(metdata$TIMESTAMP[metdata$TIMESTAMP>start.time]))==0) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("MetDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time, "and", end.time, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
} else {
for(i in 1:length(full_time)){ #this loop looks for matching dates and extracts data from metdata file to obs array
    index = which(metdata$TIMESTAMP==full_time[i])
    if(length(index)>0){
      obs[i,] <- unlist(metdata[index,c(1,2,3,8,9,10,11,13,15)])
    }
  }
obs<-as.data.frame(obs) #make into DF
colnames(obs)<-names(metdata[index,c(1,2,3,8,9,10,11,13,15)]) #get column names
obs$TIMESTAMP<-full_time #now have your array with a proper timedate stamp!

pdf(paste0("MetDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
par(mfrow=c(3,2))
plot(obs$TIMESTAMP,obs$RECORD, main="RECORD", xlab="Time", ylab="Number", type='l')
plot(obs$TIMESTAMP,obs$BattV, main="Battery", xlab="Time", ylab="Volts", type='l')
plot(obs$TIMESTAMP,obs$AirTC_Avg, main="Air Temp", xlab="Time", ylab="degrees C", type='l')
plot(obs$TIMESTAMP,obs$RH, main="Rel Hum", xlab="Time", ylab="%", type='l')
plot(obs$TIMESTAMP,obs$Rain_mm_Tot, main="Rain", xlab="Time", ylab="mm", type='l')
plot(obs$TIMESTAMP,obs$WS_ms_Avg, main="Wind speed", xlab="Time", ylab="m/s",type='l')
plot(obs$TIMESTAMP,obs$SR01Up_Avg, main="Shortwave", xlab="Time", ylab="W/m2",type='l')
plot(obs$TIMESTAMP,obs$IR01UpCo_Avg, main="Longwave", xlab="Time", ylab="W/m2",type='l')
dev.off() #file made!
}


#time to now play with catwalk data!
catheader<-read.csv("Catwalk.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
catdata<-read.csv("Catwalk.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(catdata)<-names(catheader) #combine the names to deal with Campbell logger formatting

fcrlvlheader<-read.csv("FCRWaterLevel.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
fcrlvldata<-read.csv("FCRWaterLevel.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(fcrlvldata)<-names(fcrlvlheader) #combine the names to deal with Campbell logger formatting

catdata=merge(catdata,fcrlvldata, all = T)

end.time1 <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H")), tzone = "Etc/GMT+5") #gives us current time with rounded hours in EDT
start.time1 <- end.time1 - days(5) #to give us five days of data for looking at changes
full_time1 <- seq(start.time1, end.time1, by = "10 min") #create sequence of dates from past 5 days to fill in data

obs1 <- array(NA,dim=c(length(full_time1),41)) #create array that will be filled in with 39 columns (the entire size of the array)
cat_timechange=max(which(catdata$TIMESTAMP=="2019-04-15 10:00:00"))
catdata$TIMESTAMP<-as.POSIXct(strptime(catdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned
catdata$TIMESTAMP[c(1:cat_timechange-1)]<-with_tz(force_tz(catdata$TIMESTAMP[c(1:cat_timechange-1)],"Etc/GMT+4"), "Etc/GMT+5") #pre time change data gets assigned proper timezone then corrected to GMT -5 to match the rest of the data set

if (length(na.omit(catdata$TIMESTAMP[catdata$TIMESTAMP>start.time1]))==0) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("CatwalkDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time1, "and", end.time1, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
} else {
for(j in 5:ncol(catdata)){
  catdata[,j]<-as.numeric(levels(catdata[,j]))[catdata[,j]]#need to set all columns to numeric values
}

for(i in 1:length(full_time1)){ #this loop looks for matching dates and extracts data from metdata file to obs array
  index = which(catdata$TIMESTAMP==full_time1[i])
  if(length(index)>0){
    obs1[i,] <- unlist(catdata[index,c(1:41)])
  }
}

obs1<-as.data.frame(obs1) #make into DF
obs1[,1] <- full_time1 #now have your array with a proper timedate stamp!
colnames(obs1)<-names(catdata[index,c(1:41)]) #get column names

pdf(paste0("CatwalkDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
par(mfrow=c(3,2))

plot(obs1$TIMESTAMP,obs1$RECORD, main="Campbell Logger Record", xlab="Time", ylab="Number", type='l')
plot(obs1$TIMESTAMP,obs1$BattV, main="Campbell Logger Battery", xlab="Time", ylab="Volts", type='l')
if(min(tail(na.omit(obs1$BattV)))<11.5){
  mtext("Battery Charge Low", side = 3, col="red")
}
plot(obs1$TIMESTAMP,obs1$EXO_battery, main="EXO Battery", xlab="Time", ylab="Volts", type='l')
plot(obs1$TIMESTAMP,obs1$EXO_cablepower, main="EXO Cable Power", xlab="Time", ylab="Volts", type='l')
plot(obs1$TIMESTAMP,obs1$EXO_depth, main="EXO Depth", xlab="Time", ylab="Meters", type='l')

plot(obs1$TIMESTAMP,obs1$EXO_pressure, main="Sonde Pressure", xlab="Time", ylab="psig", type='l')
points(obs1$TIMESTAMP, obs1$Lvl_psi, col="blue4", type='l')
legend("topleft", c("1.6m EXO", "9m PT"), text.col=c("black", "blue4"), x.intersp=0.001)

#par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
plot(obs1$TIMESTAMP,obs1$dotemp_9, main="Water temp of sondes", xlab="Time", ylab="degrees C", type='l', col="medium sea green", lwd=1.5, ylim=c(0,35))
points(obs1$TIMESTAMP, obs1$dotemp_5, col="black", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$EXO_wtr_1, col="magenta", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_pt_9, col="blue4", type='l', lwd=1.5)
legend("topleft", c("1.6m EXO", "5m DO", "9m DO", "9m PT"), text.col=c("magenta", "black", "medium sea green", "blue4"), x.intersp=0.001)

plot(obs1$TIMESTAMP,obs1$doobs_9, main="DO", xlab="Time", ylab="mg/L", type='l', col="medium sea green", lwd=1.5, ylim=c(-0.5,13))
points(obs1$TIMESTAMP, obs1$doobs_5, col="black", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$doobs_1, col="magenta", type='l', lwd=1.5)
legend("topleft", c("1m", "5m", "9m"), text.col=c("magenta", "black", "medium sea green"), x.intersp=0.001)

plot(obs1$TIMESTAMP,obs1$dosat_9, main="DO % saturation", xlab="Time", ylab="% saturation", type='l', col="medium sea green", lwd=1.5, ylim=c(-0.5,150))
points(obs1$TIMESTAMP, obs1$dosat_5, col="black", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$dosat_1, col="magenta", type='l', lwd=1.5)
legend("topleft", c("1m", "5m", "9m"), text.col=c("magenta", "black", "medium sea green"), x.intersp=0.001)

plot(obs1$TIMESTAMP,obs1$Cond_1, main="Cond, SpCond, TDS @ 1m", xlab="Time", ylab="uS/cm or mg/L", type='l', col="red", lwd=1.5, ylim=c(-0.5,60))
points(obs1$TIMESTAMP, obs1$SpCond_1, col="black", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$TDS_1, col="orange", type='l', lwd=1.5)
legend("topleft", c("TDS", "SpCond", "Cond"), text.col=c("orange", "black","red"), x.intersp=0.001)

plot(obs1$TIMESTAMP,obs1$Chla_1, main="Chla, Phyco, fDOM", xlab="Time", ylab="ug/L or QSU", type='l', col="green", lwd=1.5, ylim=c(-0.5,35))
points(obs1$TIMESTAMP, obs1$BGAPC_1, col="blue", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$fDOM_QSU_1, col="firebrick4", type='l', lwd=1.5)
legend("topleft", c("Chla", "Phyco", "fDOM"), text.col=c("green", "blue", "firebrick4"), x.intersp=0.001)

par(mfrow=c(1,1))
par(oma=c(1,1,1,4))
plot(obs1$TIMESTAMP,obs1$wtr_surface, main="Water Temp", xlab="Time", ylab="degrees C", type='l', col="firebrick4", lwd=1.5, ylim=c(0,35))
points(obs1$TIMESTAMP, obs1$wtr_1, col="firebrick1", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_2, col="DarkOrange1", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_3, col="gold", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_4, col="greenyellow", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_5, col="medium sea green", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_6, col="sea green", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_7, col="DeepSkyBlue4", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_8, col="blue2", type='l', lwd=1.5)
points(obs1$TIMESTAMP, obs1$wtr_9, col="blue4", type='l', lwd=1.5)
par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("right",c("0.1m","1m", "2m", "3m", "4m", "5m", "6m", "7m","8m", "9m"),
       text.col=c("firebrick4", "firebrick1", "DarkOrange1", "gold", "greenyellow", "medium sea green", "sea green",
             "DeepSkyBlue4", "blue2", "blue4"), cex=1, y.intersp=1, x.intersp=0.001, inset=c(0,0), xpd=T, bty='n')

dev.off() #file made!
}


weirheader<-read.csv("FCRweir.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
weirdata<-read.csv("FCRweir.csv", skip=4, header=F) #get data minus wonky Campbell rows
names(weirdata)<-names(weirheader) #combine the names to deal with Campbell logger formatting

end.time2 <- with_tz(as.POSIXct(strptime(Sys.time(), format = "%Y-%m-%d %H")), tzone = "Etc/GMT+5") #gives us current time with rounded minutes in EDT
start.time2 <- end.time2 - days(5) #to give us five days of data for looking at changes
full_time2 <- seq(start.time2, end.time2, by = "15 min") #create sequence of dates from past 5 days to fill in data

obs2 <- array(NA,dim=c(length(full_time2),7)) #create array that will be filled in with 8 columns
weirdata$TIMESTAMP<-as.POSIXct(strptime(weirdata$TIMESTAMP, "%Y-%m-%d %H:%M"), tz = "Etc/GMT+5") #get dates aligned

if (length(na.omit(weirdata$TIMESTAMP[weirdata$TIMESTAMP>start.time2]))==0) { #if there is no data after start time, then a pdf will be made explaining this
  pdf(paste0("WeirDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
  plot(NA, xlim=c(0,5), ylim=c(0,5), bty='n',xaxt='n', yaxt='n', xlab='', ylab='') #creates empty plot
  mtext(paste("No data found between", start.time2, "and", end.time2, sep = " ")) #fills in text in top margin of plot
  dev.off() #file made!
} else { #else, do normal data wrangling and plotting
#get columns
for(i in 1:length(full_time2)){ #this loop looks for matching dates and extracts data from metdata file to obs array
  index = which(weirdata$TIMESTAMP==full_time2[i])
  if(length(index)>0){
    obs2[i,] <- unlist(weirdata[index,c(1:7)])
  }
}
obs2<-as.data.frame(obs2) #make into DF
colnames(obs2)<-names(weirdata[index,c(1:7)]) #get column names
obs2$TIMESTAMP<-full_time2 #now have your array with a proper timedate stamp!

obs2$head=(0.149*obs2$Lvl_psi)/0.293 #equation as given by WW
obs2$flowcms= 2.391*(obs2$head^2.5) #original tidy code; obs2 <- obs2 %>%  mutate(head = (0.149*Lvl_psi)/0.293) %>% mutate(flow_cms = 2.391* (head^2.5)) 

pdf(paste0("WeirDataFigures_", Sys.Date(), ".pdf"), width=8.5, height=11) #call PDF file
par(mfrow=c(3,2))
plot(obs2$TIMESTAMP,obs2$RECORD, main="RECORD", xlab="Time", ylab="Number", type='l')
plot(obs2$TIMESTAMP,obs2$BattV, main="Battery", xlab="Time", ylab="Volts", type='l')
if(min(tail(na.omit(obs2$BattV)))<11.5){
  mtext("Battery Charge Low", side = 3, col="red")
}
plot(obs2$TIMESTAMP,obs2$AirTemp_C, main="Air Temp", xlab="Time", ylab="degrees C", type='l')
plot(obs2$TIMESTAMP,obs2$Lvl_psi, main="Water Level", xlab="Time", ylab="psi", type='l')
plot(obs2$TIMESTAMP,obs2$wtr_weir, main="Water Temp", xlab="Time", ylab="degrees C", type='l')
plot(obs2$TIMESTAMP,obs2$flowcms, main="Flow Rate", xlab="Time", ylab="cms", type='l')
dev.off() #file made!
}