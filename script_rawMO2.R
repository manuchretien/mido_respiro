# Script - data wrangling respirometry data
# - Creating a file for each fish/trial
# - Calculate raw metabolic rates (mo2/hr)
# - Correction for background respiration
# - Get MMR and add it to file
# 
# by Emmanuelle Chretien
# Created: October 4, 2018
# Last update: November 20, 2018

rm(list=ls())

# list file names in an object
files.trials <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_autoresp_csv"))

#### loop to get one file per trial/chamber ####
for (i in 1:length(files.trials)){
  
  setwd("~/Documents/Etudes/PhD/respiro/_autoresp_csv")
  trial.file <- files.trials[i]
  
  trial<-read.csv(trial.file,header=TRUE,sep=";")
  
  if (length(trial)==6){
    
    setwd("~/Documents/Etudes/PhD/respiro/_trial_chambre")
    file.name<-paste(trial.file,names(trial[5]),".csv",sep="")
    write.csv(trial,file=file.name,row.names=F)
                     
  }
  
  else if (length(trial)==8){
    
    trial1<-trial[,1:6]
    trial2<-trial[,c(1:4,7:8)]
    
    setwd("~/Documents/Etudes/PhD/respiro/_trial_chambre")
    file.name<-paste(trial.file,names(trial1[5]),".csv",sep="")
    write.csv(trial1,file=file.name,row.names=F)
    
    file.name<-paste(trial.file,names(trial2[5]),".csv",sep="")
    write.csv(trial2,file=file.name,row.names=F)
    
  }
  
  else if (length(trial)==10){
    
    trial1<-trial[,1:6]
    trial2<-trial[,c(1:4,7:8)]
    trial3<-trial[,c(1:4,9:10)]
    
    setwd("~/Documents/Etudes/PhD/respiro/_trial_chambre")
    file.name<-paste(trial.file,names(trial1[5]),".csv",sep="")
    write.csv(trial1,file=file.name,row.names=F)
    
    file.name<-paste(trial.file,names(trial2[5]),".csv",sep="")
    write.csv(trial2,file=file.name,row.names=F)
    
    file.name<-paste(trial.file,names(trial3[5]),".csv",sep="")
    write.csv(trial3,file=file.name,row.names=F)
    
  }
  
  else {
    
    trial1<-trial[,1:6]
    trial2<-trial[,c(1:4,7:8)]
    trial3<-trial[,c(1:4,9:10)]
    trial4<-trial[,c(1:4,11:12)]
    
    setwd("~/Documents/Etudes/PhD/respiro/_trial_chambre")
    file.name<-paste(trial.file,names(trial1[5]),".csv",sep="")
    write.csv(trial1,file=file.name,row.names=F)
    
    file.name<-paste(trial.file,names(trial2[5]),".csv",sep="")
    write.csv(trial2,file=file.name,row.names=F)
    
    file.name<-paste(trial.file,names(trial3[5]),".csv",sep="")
    write.csv(trial3,file=file.name,row.names=F)
    
    file.name<-paste(trial.file,names(trial4[5]),".csv",sep="")
    write.csv(trial4,file=file.name,row.names=F)
    
  }
}

#### loop to calculate back raw MO2 and correct for control ####
# (remove mass from MO2 data), calculate background respiration over time
# Finally, rename columns and file per trial/fish ID
files.fish <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_chambre"))

# import trial data (trial_poisson_v2.csv)
setwd("~/Documents/Etudes/PhD/respiro/analyses")
trial.data<-read.csv(file.choose(),header=T,sep=";")

for (i in 1:length(files.fish)){
  
  setwd("~/Documents/Etudes/PhD/respiro/_trial_chambre")
  fish.file <- files.fish[i]
  
  fish<-read.csv(fish.file,header=TRUE,sep=",")
  fish<-na.omit(fish) # remove any line with NA value
  
  fish$raw.mo<-fish[,5]*(trial.data[i,3]/1000)
  fish$control<-fish[,3]*trial.data$b.control[i]+trial.data$a.control[i]
  fish$mo.cor<-fish[,5]-fish$control
  
  names(fish)<-c("Date.Time","Time.sec","Time.hr","Loop","mo.kg.hr",
                 "mo.slope","mo.hr","control","mo.cor.kg.hr")
  
  
  file.name<-paste(trial.data[i,1],"_",trial.data[i,4],"_",trial.data[i,2],".csv",sep="")
  setwd("~/Documents/Etudes/PhD/respiro/_trial_fish")
  write.csv(fish,file=file.name,row.names=F)
  
}

#### loop to create graphs for each trial/fish and isolate MMR ####
files.MO2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_fish"))
files.rec2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_rec2"))

MMR=NULL
no.Loop.MMR=NULL

PDFPath = "~/Documents/Etudes/PhD/respiro/analyses/mido_mo2.pdf"
pdf(file=PDFPath)  
setwd("~/Documents/Etudes/PhD/respiro/_trial_fish")

par(mfrow=c(3,2)) 
for (j in 1:length(files.MO2)){
  
  data <- files.MO2[j]
  data.MO2<-read.csv(data,header=TRUE,sep=",")
  plot(mo.cor.kg.hr~Loop,data=data.MO2,type="p",main=data)
  
  mmr<-max(data.MO2$mo.cor.kg.hr)
  no.loop<-data.MO2[which.max(data.MO2$mo.cor.kg.hr),4]
  
  MMR=c(MMR,mmr)
  no.Loop.MMR<-c(no.Loop.MMR,no.loop)
  
}
dev.off() 

# Add MMR to trial.data. Then save trial data.
trial.data$MMR<-MMR
trial.data$no.Loop.MMR<-no.Loop.MMR

setwd("~/Documents/Etudes/PhD/respiro/analyses")
write.table(trial.data, file="trial_poisson_v4.txt")

par(mfrow=c(1,1))
plot(MMR~traitement,data=trial.data)
plot(MMR~ordre,data=trial.data,col=ID)
plot(no.Loop~traitement,data=trial.data)
plot(no.Loop~ordre,data=trial.data)


#### Get recovery time ####
# - this data will also be useful for calculations of SMR

setwd("~/Documents/Etudes/PhD/respiro/_trial_fish")
files.MO2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_fish"))

Recov.loop=NULL
for (i in 1:length(files.MO2)){
  
  # load data
  data <- files.MO2[i]
  mr.data<-read.csv(data,header=TRUE,sep=",")
  
  # fit loess function to MO2 decline with time
  m1<-loess(mo.cor.kg.hr~Time.hr,data=mr.data)
  
  # extract fitted values from model
  fit1<-predict(m1)
  
  # Use a moving slope to get to the point where MO2 stabilizes (slope = 0)
  Slope=NULL
  
  # Calculate a moving slope over 15 points, iteratively, on whole data
  for (j in 1:(length(fit1)-15)){
    
    Loop<-c(1:(length(fit1)-15))
    model.slope<-lm(fit1[j:(j+15)]~Loop[j:(j+15)])
    sl<-model.slope$coefficients[2]
    Slope<-c(Slope,sl)
  }
  
  # Get absolute and round values (with one decimal) and the loop number it corresponds to
  Slope_round<-abs(round(Slope,1))
  no.loop<-cbind(Loop,Slope_round)
  
  # Extract data for which slope equals zero
  no.loop.zero<-no.loop[no.loop[,2]==0.0,]
  
  # Get loop number where slope equals zero the first time 
  recov.loop<-no.loop.zero[1,1]
  
  Recov.loop<-c(Recov.loop,recov.loop)
  
}

# Add recovery loop to trial.data. Then save trial data.
trial.data$Recov.loop<-Recov.loop


setwd("~/Documents/Etudes/PhD/respiro/analyses")
write.table(trial.data, file="trial_poisson_v5.txt")


#### Calculate SMR using different techniques ####

# require package for calculations
require(fishMO2)

setwd("~/Documents/Etudes/PhD/respiro/_trial_fish")
files.MO2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_fish"))


SMR.mlnd=NULL
CV.mlnd=NULL
SMR.quant=NULL

for (i in 1:length(files.MO2)){
  
  data <- files.MO2[i]
  data.MO2<-read.csv(data,header=TRUE,sep=",")
  
  calc.MO<-calcSMR(data.MO2$mo.cor.kg.hr,q=c(0.1,0.15,0.2,0.25,0.3),G=1:4)
  
  smr.mlnd<-calc.MO$mlnd
  cv.mlnd<-calc.MO$CVmlnd
  smr.quant<-calc.MO$quant[3]
  
  SMR.mlnd=c(SMR.mlnd,smr.mlnd)
  CV.mlnd=c(CV.mlnd,cv.mlnd)
  SMR.quant=c(SMR.quant,smr.quant)
  
  
}

hist(SMR.mlnd)
hist(SMR.quant)
hist(CV.mlnd)

# import trial data (trial_poisson_v4.csv)
#setwd("~/Documents/Etudes/PhD/respiro/analyses")
#trial.data<-read.csv(file.choose(),header=T,sep=";")

trial.data$SMR.mlnd<-SMR.mlnd
trial.data$CV.mlnd<-CV.mlnd
trial.data$SMR.quant<-SMR.quant

plot(SMR.mlnd~traitement,data=trial.data)
plot(SMR.quant~traitement,data=trial.data)
plot(CV.mlnd~traitement,data=trial.data)

plot(SMR.mlnd~as.factor(ordre),data=trial.data)
plot(SMR.quant~as.factor(ordre),data=trial.data)
plot(CV.mlnd~as.factor(ordre),data=trial.data)

plot(SMR.mlnd~ID,data=trial.data)
plot(SMR.quant~ID,data=trial.data)
plot(CV.mlnd~ID,data=trial.data)


#### Calculate SMR without acclimation period ####

SMR.mlnd.noacc=NULL
CV.mlnd.noacc=NULL
SMR.quant.noacc=NULL

for (i in 1:length(files.MO2)){
  
  data <- files.MO2[i]
  data.MO2.all<-read.csv(data,header=TRUE,sep=",")
  
  line.no<-trial.data$Recov.loop[i]
  data.MO2<-data.MO2.all[line.no:length(data.MO2.all$Loop),]
  calc.MO<-calcSMR(data.MO2$mo.cor.kg.hr,q=c(0.1,0.15,0.2,0.25,0.3),G=1:4)
  
  smr.mlnd.noacc<-calc.MO$mlnd
  cv.mlnd.noacc<-calc.MO$CVmlnd
  smr.quant.noacc<-calc.MO$quant[3]
  
  SMR.mlnd.noacc=c(SMR.mlnd.noacc,smr.mlnd.noacc)
  CV.mlnd.noacc=c(CV.mlnd.noacc,cv.mlnd.noacc)
  SMR.quant.noacc=c(SMR.quant.noacc,smr.quant.noacc)
  
  
}

trial.data$SMR.mlnd.noacc<-SMR.mlnd.noacc
trial.data$CV.mlnd.noacc<-CV.mlnd.noacc
trial.data$SMR.quant.noacc<-SMR.quant.noacc

setwd("~/Documents/Etudes/PhD/respiro/analyses")
write.table(trial.data, file="trial_poisson_v6.txt")


#### New calculation of SMR ####
# Calculate SMR without acclimation period 
# Start loop adjusted when loess fit was not accurate with real observations
# End loop modified to be at sunrise (some fish showed activity after sunrise)

# load data (trial_poisson_v7.csv):
setwd("~/Documents/Etudes/PhD/respiro/analyses")
trial.data<-read.csv(file.choose(),header=T,sep=";")


SMR.quant.noacc=NULL

# require package for calculations
require(fishMO2)

setwd("~/Documents/Etudes/PhD/respiro/_trial_fish")
files.MO2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_fish"))


for (i in 1:length(files.MO2)){
  
  data <- files.MO2[i]
  data.MO2.all<-read.csv(data,header=TRUE,sep=",")
  
  line.start<-trial.data$loop.min[i]
  line.end<-trial.data$loop.max[i]
  data.MO2<-data.MO2.all[line.start:line.end,]
  calc.MO<-calcSMR(data.MO2$mo.cor.kg.hr,q=c(0.1,0.15,0.2,0.25,0.3),G=1:4)
  
  smr.quant.noacc<-calc.MO$quant[3]
  
  SMR.quant.noacc=c(SMR.quant.noacc,smr.quant.noacc)
  
  
}

trial.data$SMR.quant.noacc1<-SMR.quant.noacc
trial.data$AS<-trial.data$MMR-trial.data$SMR.quant.noacc1

#### Calculation of coefficient of variation on MO2 used for SMR ####
CV=NULL
for (i in 1:length(files.MO2)){
  
  data <- files.MO2[i]
  data.MO2.all<-read.csv(data,header=TRUE,sep=",")
  
  line.start<-trial.data$loop.min[i]
  line.end<-trial.data$loop.max[i]
  data.MO2<-data.MO2.all[line.start:line.end,]
  
  cv<-(sd(data.MO2$mo.cor.kg.hr)/mean(data.MO2$mo.cor.kg.hr))*100
  
  CV=c(CV,cv)
  
  
}

trial.data$CV<-CV

write.table(trial.data, file="trial_poisson_v8.txt")

#### Recovery time - 50% AS ####
# Get value at which MO2 is at 50% of AS.
trial.data$AS50<-trial.data$AS/2+trial.data$SMR.quant.noacc1

setwd("~/Documents/Etudes/PhD/respiro/_trial_fish")
files.MO2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_fish"))
files.rec2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_rec2"))
setwd("~/Documents/Etudes/PhD/respiro/_trial_rec2")

Recov50.loop=NULL
Recov50.time.min=NULL
for (i in 1:length(files.rec2)){
  
  # load data
  data <- files.rec2[i]
  mr.data<-read.csv(data,header=TRUE,sep=",")
  
  # Extract data for which slope equals 50% AS
  no.loop.50<-mr.data[mr.data$mo.cor.kg.hr<=trial.data$AS50[i],]
  
  # Get loop number where slope equals zero the first time 
  recov50.loop<-no.loop.50[1,10]
  recov50.time.min<-no.loop.50[1,11]
  
  
  Recov50.loop<-c(Recov50.loop,recov50.loop)
  Recov50.time.min<-c(Recov50.time.min,recov50.time.min)
  
}

trial.data$Recov50.loop<-Recov50.loop
trial.data$Recov50.time.min<-Recov50.time.min

write.table(trial.data, file="trial_poisson_v13.txt")


