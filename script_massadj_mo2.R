# Script to generate mass adjusted metabolic rates
# - back calculate raw MO2 values
# - generate log transformed variables (mass, SMR, MMR)
# - Get a and b for SMR and MMR
# - calculate mass adjusted SMR and MMR. From that, get AS.
#
# script by Emmanuelle Chretien
# created on November 7, 2018

# Back calculate raw MO2 data
trial.data$raw.SMR<-trial.data$SMR.quant.noacc1*(trial.data$masse.kg)
trial.data$raw.MMR<-trial.data$MMR*(trial.data$masse.kg)
trial.data$raw.AS<-trial.data$raw.MMR-trial.data$raw.SMR


trial.data$log.SMR<-log10(trial.data$raw.SMR)
trial.data$log.MMR<-log10(trial.data$raw.MMR)
trial.data$log.AS<-log10(trial.data$raw.AS)

trial.data$log.masse<-log10(trial.data$masse.kg)

# Create linear regression to get a and b
# Find out if a and b differ across waterbodies

# SMR
m.smr<-lm(log.SMR~log.masse*provenance,data=trial.data)
summary(m.smr)
# interaction non significant
# b=0.8063

m.smr.2<-lm(log.SMR~log.masse+provenance,data=trial.data)
summary(m.smr.2)
# b=0.8098
# a different for each lake.
b.smr<-m.smr.2$coefficients[2]

m.smr.raw<-lm(raw.SMR~masse.kg*provenance,data=trial.data)
summary(m.smr.raw)

m.smr.raw2<-lm(raw.SMR~masse.kg+provenance,data=trial.data)
summary(m.smr.raw2)
par(mfrow=c(2,2))
plot(m.smr.raw2)
plot(m.smr.2)

# MMR
m.mmr<-lm(log.MMR~log.masse*provenance,data=trial.data)
summary(m.mmr)
# interaction non significant
# b=0.755

m.mmr.2<-lm(log.MMR~log.masse+provenance,data=trial.data)
summary(m.mmr.2)
# b=0.785
# a different for each lake.
b.mmr<-m.mmr.2$coefficients[2]

# Generate mass adjusted values of metabolic rates 
trial.data$SMR.adj<-(mean(trial.data$masse.kg)^(b.smr-1))*((trial.data$masse.kg)^(1-b.smr))*trial.data$SMR.quant.noacc1

trial.data$MMR.adj<-(mean(trial.data$masse.kg)^(b.mmr-1))*((trial.data$masse.kg)^(1-b.mmr))*trial.data$MMR

trial.data$AS.adj<-trial.data$MMR.adj-trial.data$SMR.adj

setwd("~/Documents/Etudes/PhD/respiro/analyses")
write.table(trial.data, file="trial_poisson_v10.txt")


