# Script - data wrangling respirometry data
# Creating control slopes (depletion of background respiration over time)
#
# 
# by Emmanuelle Chretien
# October 4, 2018

rm(list=ls())

setwd("~/Documents/Etudes/PhD/respiro/analyses")

# import data (trial_poisson.csv)
control<-read.csv(file.choose(),header=T,sep=";")

# create object for slope and intercept values to be calculated in loop
b.control=NULL
a.control=NULL

for (i in 1:length(control$trial)){

  if (is.na(control[i,12])){
    
    b=NA
    a=NA
    
  }else{
    
    x=c(0,control[i,16])
    y=c(control[i,10],control[i,13])
    
    reg.control=lm(y~x)
    
    b=summary(reg.control)$coef[2,1]
    a=summary(reg.control)$coef[1,1]
    
  }
  
  b.control=c(b.control,b)
  a.control=c(a.control,a)
  
}

control$a.control<-a.control
control$b.control<-b.control

write.table(control, file="trial_poisson_v2.txt")
