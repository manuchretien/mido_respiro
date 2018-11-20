#### Calculation of RMR during daytime
# Isolate points after full recovery and turn off of lights... and also sunset/sunrise
# Calculate RMR
# Determine the number of points used for this calculation
# Determine CV for RMR.
#
# script by Emmanuelle Chretien
# created on November 15, 2018
# Last update: November 20, 2018


rm(list=ls())

# load data (trial_poisson_v14.csv):
setwd("~/Documents/Etudes/PhD/respiro/analyses")
trial.data<-read.csv(file.choose(),header=T,sep=";")

# load data (trial_time.csv):
#setwd("~/Documents/Etudes/PhD/respiro/analyses")
trial.time<-read.csv(file.choose(),header=T,sep=";")

RMR.day=NULL # RMR of points when light is on, after recovery
RMR.day2=NULL # RMR of points before sunset, after sunrise... after recovery

CV.rmr=NULL
CV.rmr2=NULL

no.RMR=NULL
no.RMR2=NULL

# require package for calculations
require(fishMO2)

setwd("~/Documents/Etudes/PhD/respiro/_trial_rec2")
files.rec2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_rec2"))

# THIS LOOP WAS NOT USED IN THE END FOR CALCULATION OF RMR
# USE NEXT ONE INSTEAD.

for (i in 1:length(files.rec2)){
  
  data <- files.rec2[i]
  data.MO2<-read.csv(data,header=TRUE,sep=",")
  
  # for the two fish for which experiment ended in the evening (power outage)
  if (max(data.MO2$Loop.deb)<50){
    
    # for RMR.day
    line.start<-trial.data$recov.final[i]
    line.end.day<-trial.time$loop.lumoff[i]
    
    # for RMR.day2
    line.end.day2<-trial.time$loop.coucher[i]
    
    data.rmr.day<-data.MO2[line.start:line.end.day,]
    data.rmr.day2<-data.MO2[line.start:line.end.day2,]
    
  # for the rest of the fish  
  }else{
    
    # isolate MO2 measurements after sunrise/turn on of lights... day after
    line.start.apr.day<-trial.time$loop.lumon[i]
    line.start.apr.day2<-trial.time$loop.lever[i]
    line.end.apr<-max(data.MO2$Loop.deb)
    
    data.apr.day<-data.MO2[line.start.apr.day:line.end.apr,]
    data.apr.day2<-data.MO2[line.start.apr.day2:line.end.apr,]
    
    # if recovery time occurs before lights are turned off
    if(trial.data$recov.temps.min[i]<trial.time$Temps.lumoff[i]){
      
      line.start.av.day<-trial.data$recov.final[i]
      line.end.av.day<-trial.time$loop.lumoff[i]
      
      data.av.day<-data.MO2[line.start.av.day:line.end.av.day,]
      
      data.rmr.day<-rbind(data.av.day,data.apr.day)
      
    # if recovery time does not occur before lights are turned off... only use day after.
    }else{
      
      data.rmr.day<-data.apr.day
    }
    
    # if recovery time occurs before sunset
    if(trial.data$recov.temps.min[i]<trial.time$Temps.coucher[i]){
      
      line.start.av.day2<-trial.data$recov.final[i]
      line.end.av.day2<-trial.time$loop.coucher[i]
      
      data.av.day2<-data.MO2[line.start.av.day2:line.end.av.day2,]
      
      data.rmr.day2<-rbind(data.av.day2,data.apr.day2)
    
    # if recovery time does not occur before sunset... only use after sunrise
    }else{
      
      data.rmr.day2<-data.apr.day2
    }
  }
  
  # now that we have data isolated, calculate RMR and other stuff
  calc.MO.day<-calcSMR(data.rmr.day$mo.cor.kg.hr,q=c(0.1,0.15,0.2,0.25,0.3),G=1:4)
  calc.MO.day2<-calcSMR(data.rmr.day2$mo.cor.kg.hr,q=c(0.1,0.15,0.2,0.25,0.3),G=1:4)
  
  rmr.day<-calc.MO.day$quant[3]
  rmr.day2<-calc.MO.day2$quant[3]
  
  RMR.day<-c(RMR.day,rmr.day)
  RMR.day2<-c(RMR.day2,rmr.day2)
  
  cv.rmr.day<-(sd(data.rmr.day$mo.cor.kg.hr)/mean(data.rmr.day$mo.cor.kg.hr))*100
  cv.rmr.day2<-(sd(data.rmr.day2$mo.cor.kg.hr)/mean(data.rmr.day2$mo.cor.kg.hr))*100
  
  CV.rmr<-c(CV.rmr,cv.rmr.day)
  CV.rmr2<-c(CV.rmr2,cv.rmr.day2)
  
  no.rmr<-length(data.rmr.day$Loop.deb)
  no.rmr2<-length(data.rmr.day2$Loop.deb)
  
  no.RMR<-c(no.RMR,no.rmr)
  no.RMR2<-c(no.RMR2,no.rmr2)
    
    
}


#### New RMR calculation - without before lights are turned off the day of ####
# This is the calculation that was integrated in the data set.
# RMR calculations are more standardized if they all include the same timeframe
# in this case, only MO2 after lights were turned on and after sunrise (suffixe 2)
# are included for calculation of RMR using 0.2 quantile method (Chabot et al. 2016).

# load data (trial_time.csv):
#setwd("~/Documents/Etudes/PhD/respiro/analyses")
trial.time<-read.csv(file.choose(),header=T,sep=";")

# load data (trial_poisson_v14.csv):
setwd("~/Documents/Etudes/PhD/respiro/analyses")
trial.data<-read.csv(file.choose(),header=T,sep=";")


RMR.day=NULL # RMR of points when light is on, after recovery
RMR.day2=NULL # RMR of points before sunset, after sunrise... after recovery

CV.rmr=NULL
CV.rmr2=NULL

no.RMR=NULL
no.RMR2=NULL

# require package for calculations
require(fishMO2)

setwd("~/Documents/Etudes/PhD/respiro/_trial_rec2")
files.rec2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_rec2"))


for (i in 1:length(files.rec2)){
  
  data <- files.rec2[i]
  data.MO2<-read.csv(data,header=TRUE,sep=",")
  
  # for the two fish for which experiment ended in the evening (power outage)
  if (max(data.MO2$Loop.deb)<50){
    
    rmr.day<-NA
    rmr.day2<-NA
    
    cv.rmr.day<-NA
    cv.rmr.day2<-NA
    
    no.rmr<-NA
    no.rmr2<-NA
    
    
    # for the rest of the fish  
  }else{
    
    # isolate MO2 measurements after sunrise/turn on of lights... day after
    line.start.apr.day<-trial.time$loop.lumon[i]
    line.start.apr.day2<-trial.time$loop.lever[i]
    line.end.apr<-max(data.MO2$Loop.deb)
    
    data.rmr.day<-data.MO2[line.start.apr.day:line.end.apr,]
    data.rmr.day2<-data.MO2[line.start.apr.day2:line.end.apr,]
    
    # now that we have data isolated, calculate RMR and other stuff
    calc.MO.day<-calcSMR(data.rmr.day$mo.cor.kg.hr,q=c(0.1,0.15,0.2,0.25,0.3),G=1:4)
    calc.MO.day2<-calcSMR(data.rmr.day2$mo.cor.kg.hr,q=c(0.1,0.15,0.2,0.25,0.3),G=1:4)
    
    rmr.day<-calc.MO.day$quant[3]
    rmr.day2<-calc.MO.day2$quant[3]
    
    cv.rmr.day<-(sd(data.rmr.day$mo.cor.kg.hr)/mean(data.rmr.day$mo.cor.kg.hr))*100
    cv.rmr.day2<-(sd(data.rmr.day2$mo.cor.kg.hr)/mean(data.rmr.day2$mo.cor.kg.hr))*100
    
    no.rmr<-length(data.rmr.day$Loop.deb)
    no.rmr2<-length(data.rmr.day2$Loop.deb)
    
  }
  
  RMR.day<-c(RMR.day,rmr.day)
  RMR.day2<-c(RMR.day2,rmr.day2)
  
  CV.rmr<-c(CV.rmr,cv.rmr.day)
  CV.rmr2<-c(CV.rmr2,cv.rmr.day2)
  
  no.RMR<-c(no.RMR,no.rmr)
  no.RMR2<-c(no.RMR2,no.rmr2)
  
  
}



trial.data$RMR.day<-RMR.day
trial.data$RMR.day2<-RMR.day2
trial.data$CV.rmr<-CV.rmr
trial.data$CV.rmr2<-CV.rmr2
trial.data$no.RMR<-no.RMR
trial.data$no.RMR2<-no.RMR2

trial.data$raw.RMR.day<-trial.data$RMR.day*trial.data$masse.kg
trial.data$raw.RMR.day2<-trial.data$RMR.day2*trial.data$masse.kg

par(mfrow=c(1,3))
plot(SMR.quant.noacc1~traitement,data=trial.data,ylim=c(60,160))
plot(RMR.day~traitement,data=trial.data,ylim=c(60,160))
plot(RMR.day2~traitement,data=trial.data,ylim=c(60,160))

par(mfrow=c(1,3))
plot(CV~traitement,data=trial.data,ylim=c(0,20))
plot(CV.rmr~traitement,data=trial.data,ylim=c(0,20))
plot(CV.rmr2~traitement,data=trial.data,ylim=c(0,20))

par(mfrow=c(1,1))
plot(RMR.raw~masse.kg,data=trial.data)
points(RMR2.raw~masse.kg,data=trial.data,col="blue")
points(raw.SMR~masse.kg,data=trial.data,col="green")


trial.data$log.RMR<-log10(trial.data$raw.RMR.day)
trial.data$log.RMR2<-log10(trial.data$raw.RMR.day2)

# calculate mass-adjusted RMR
M.rmr<-lm(log.RMR~log.masse,data=trial.data)
# b=0.7948
b.rmr<-M.rmr$coefficients[2]

M.rmr2<-lm(log.RMR2~log.masse,data=trial.data)
# b=0.7941
b.rmr2<-M.rmr2$coefficients[2]

trial.data$RMR.adj<-(mean(trial.data$masse.kg)^(b.rmr-1))*((trial.data$masse.kg)^(1-b.rmr))*trial.data$RMR.day
trial.data$RMR2.adj<-(mean(trial.data$masse.kg)^(b.rmr2-1))*((trial.data$masse.kg)^(1-b.rmr2))*trial.data$RMR.day2

setwd("~/Documents/Etudes/PhD/respiro/analyses")
write.table(trial.data, file="trial_poisson_v15.txt")
