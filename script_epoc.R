#### Calculation of EPOC
# using fit from 6th order polynomial regression
# time since beginning of experiment in minutes
# function auc() from package MESS{} calculates area under the curve
#
# script by Emmanuelle Chretien
# created on November 20, 2018
# modified on Nov. 23, 2018

require(MESS)

# load trial.data to get moment where full recovery to SMR occurs
# area under the curve is calculated until that point so that if MO2 increases the next day
# it is not added.
trial.data<-read.csv(file.choose(),header=T,sep=",")

setwd("~/Documents/Etudes/PhD/respiro/_trial_rec2")
files.rec2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_rec2"))

EPOC=NULL

for (i in 1:length(files.rec2)){
  
  data <- files.rec2[i]
  fish.all<-read.csv(data,header=TRUE,sep=",")
  
  recov.time<-trial.data$recov.temps.min[i]
  
  fish<-fish.all[fish.all$Time.min<recov.time,]
  
  # create variable y to equal MO2 values minus SMR.
  # create variable x to equal time
  y<-fish$fit6.sans.smr
  x<-fish$Time.min
  
  # calculate the area under the curve
  area<-auc(x,y,type=c("spline"))
  
  EPOC<-c(EPOC,area)
  
}

# load trial data to add EPOC to it
setwd("~/Documents/Etudes/PhD/respiro/analyses")
trial.data<-read.csv(file.choose(),header=T,sep=";")

trial.data$EPOC<-EPOC1

trial.data$FAS<-trial.data$raw.MMR/trial.data$raw.SMR
trial.data$FAS.RMR<-trial.data$raw.MMR/trial.data$raw.RMR.day
trial.data$FAS.RMR2<-trial.data$raw.MMR/trial.data$raw.RMR.day2

# write new CSV file
write.csv(trial.data,"trial_poisson_v16.csv",row.names=F)



