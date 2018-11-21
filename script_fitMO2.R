# Script - find best fit for MO2 depletion over time
# - Creates one figure per fish/trial with regression lines
# - Comparison of loess fit to polynomial (over 4 degree order)
#
# by Emmanuelle Chretien
# November 7, 2018 
# Last update: Nov. 13, 2018


#### Figures to find best fit per model
PDFPath = "~/Documents/Etudes/PhD/respiro/analyses/mido_fit_mo2-SMR_all.pdf"
pdf(file=PDFPath)  
setwd("~/Documents/Etudes/PhD/respiro/_trial_fish")

files.MO2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_fish"))


par(mfrow=c(1,1)) 


for (i in 1:length(files.MO2)){
  
  # load data
  data <- files.MO2[i]
  mr.data<-read.csv(data,header=TRUE,sep=",")
  
  mr.data$mo.minSMR<-mr.data$mo.cor.kg.hr-trial.data$SMR.quant.noacc1[i]
  
  # fit loess function to MO2 decline with time
  m1<-loess(mo.minSMR~Loop,data=mr.data)
  m4<-lm(mo.minSMR~poly(Loop,4),data=mr.data)
  m5<-lm(mo.minSMR~poly(Loop,5),data=mr.data)
  m6<-lm(mo.minSMR~poly(Loop,6),data=mr.data)
  
  # extract fitted values from model
  fit1<-predict(m1)
  fit4<-predict(m4)
  fit5<-predict(m5)
  fit6<-predict(m6)
  
  plot(mo.minSMR~Loop,data=mr.data,type="p")
  title(paste(data,"-","traitement","-",trial.data$traitement[i]))
  
  lines(mr.data$Loop, fit1, col="red") 
  lines(mr.data$Loop, fit4, col="purple")
  lines(mr.data$Loop, fit5, col="black")
  lines(mr.data$Loop, fit6, col="orange")
  abline(a=0,b=0,lty=2)
  
  legend("topright",legend=c("fit loess","fit4","fit5","fit6"),lty=c(1,1,1,1),col=c("red","purple","black","orange"),bty='n',cex=1.5)
  
}
dev.off()


#### loop to prepare data for EPOC ####
# create vector of loop no starting from 1
# create vector of time since begining of trial in minutes
# create vector of MO2 from which SMR has been substracted.

setwd("~/Documents/Etudes/PhD/respiro/_trial_fish")
files.MO2 <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_fish"))


for (i in 1:length(files.MO2)){
  
  setwd("~/Documents/Etudes/PhD/respiro/_trial_fish")
  fish.file <- files.MO2[i]
  
  fish<-read.csv(fish.file,header=TRUE,sep=",")
  
  fish$Loop.deb<-seq(1:length(fish$Loop))
  fish$Time.min<-(fish[,2]-fish[1,2])/60
  fish$Time.min<-round(fish$Time.min,digits=0)
  fish$mo.sans.smr<-fish$mo.cor.kg.hr-trial.data$SMR.quant.noacc1[i]
  
  names(fish)<-c("Date.Time","Time.sec","Time.hr","Loop","mo.kg.hr",
                 "mo.slope","mo.hr","control","mo.cor.kg.hr","Loop.deb","Time.min","mo.sans.smr")
  
  
  file.name<-paste(trial.data[i,1],"_",trial.data[i,5],"_",trial.data[i,3],"_recov.csv",sep="")
  setwd("~/Documents/Etudes/PhD/respiro/_trial_rec")
  write.csv(fish,file=file.name,row.names=F)
  
}


#### Calculate fit from 6 degree polynomial ####
# - Substract SMR from fit for EPOC

setwd("~/Documents/Etudes/PhD/respiro/_trial_rec")
files.rec <- as.character(list.files(path="~/Documents/Etudes/PhD/respiro/_trial_rec"))

for (i in 1:length(files.rec)){
  
  # load data
  setwd("~/Documents/Etudes/PhD/respiro/_trial_rec")
  data <- files.rec[i]
  mr.data<-read.csv(data,header=TRUE,sep=",")
  
  # fit loess function to MO2 decline with time
  m6<-lm(mo.cor.kg.hr~poly(Time.min,6),data=mr.data)
  
  # extract fitted values from model
  mr.data$fit6<-predict(m6)
  
  mr.data$fit6.sans.smr<-mr.data$fit6-trial.data$SMR.quant.noacc1[i]
  
  names(mr.data)<-c("Date.Time","Time.sec","Time.hr","Loop","mo.kg.hr",
                    "mo.slope","mo.hr","control","mo.cor.kg.hr","Loop.deb","Time.min","mo.sans.smr",
                    "fit6","fit6.sans.smr")
  
  
  file.name<-paste(trial.data[i,1],"_",trial.data[i,5],"_",trial.data[i,3],"_recov2.csv",sep="")
  setwd("~/Documents/Etudes/PhD/respiro/_trial_rec2")
  write.csv(mr.data,file=file.name,row.names=F)
  
  
}


