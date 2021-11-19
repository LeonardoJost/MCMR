### Corrected power estimation for number of blocks
#     Copyright (C) 2021  Leonardo Jost
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

###functions

#generate dataset
#n - number of participants
#N - number of measurement categories in total (this is currently always n*4)
#numberOfItem - number of repetitions of each measurement for binomial distribution (note the hard coded outliers)
#standard deviation consists of two parts
#sqrt(r)*sd - standard deviation between participants
#sqrt(1-r)*sd - standard deviation within participants
generateData=function(n,N,numberOfItem,sd=0.73,r=0.55) {
  testdata=data.frame(ids=as.factor(rep(1:n,each=N/n)),
                      sex=factor(rep(c("m","f"),each=N/2)),
                      nStimuli=factor(rep(c(2,8),each=2,N/4)),
                      type=factor(rep(c("type1","type2"),N/2)))
  testdata$baseProb=ifelse(testdata$nStimuli==2 & testdata$type=="type1" & testdata$sex=="m",0,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type1" & testdata$sex=="m",0,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type2" & testdata$sex=="m",0,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type2" & testdata$sex=="m",0,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type1" & testdata$sex=="f",0,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type1" & testdata$sex=="f",1/6,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type2" & testdata$sex=="f",1/2,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type2" & testdata$sex=="f",1,
                    0))))))))
  #convert to numeric to test for main effects as part of interactions
  testdata$sexNumeric=sapply(testdata$sex,function(i) contr.sum(2)[i,])
  testdata$typeNumeric=sapply(testdata$type,function(i) contr.sum(2)[i,])
  testdata$nStimuliNumeric=sapply(testdata$nStimuli,function(i) contr.sum(2)[i,])
  #get base probability for each participant, corr coeff of .66
  testdata$baseProbID=rep(rnorm(n),each=N/n)*sqrt(r)*sd
  #get probability from log odds
  testdata$performance=testdata$baseProb+testdata$baseProbID+rnorm(N)*sqrt(1-r)*sd
  return(testdata)
}

#get p values of effects of interest
getSignificant=function(testdata){
  glmerModel=glmer(correctResponses~sexNumeric*nStimuliNumeric*typeNumeric+(1|ids),
                    family=binomial(),data=testdata,weights=weights,
                    control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  glmerModel1=update(glmerModel,formula = ~. -sexNumeric:nStimuliNumeric:typeNumeric)
  glmerModel2=update(glmerModel,formula = ~. -sexNumeric:nStimuliNumeric)
  glmerModel3=update(glmerModel,formula = ~. -sexNumeric:typeNumeric)
  return(c(anova(glmerModel,glmerModel1)$"Pr(>Chisq)"[2],
           anova(glmerModel,glmerModel2)$"Pr(>Chisq)"[2],
           anova(glmerModel,glmerModel3)$"Pr(>Chisq)"[2]))
}
#linear models
getSignificantLmer=function(testdata){
  lmerModel=lmer(performance~sexNumeric*nStimuliNumeric*typeNumeric+(1|ids),
                 data=testdata,REML=FALSE,
                 control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  lmerModel1=update(lmerModel,formula = ~. -sexNumeric:nStimuliNumeric:typeNumeric)
  lmerModel2=update(lmerModel,formula = ~. -sexNumeric:nStimuliNumeric)
  lmerModel3=update(lmerModel,formula = ~. -sexNumeric:typeNumeric)
  return(c(anova(lmerModel,lmerModel1)$"Pr(>Chisq)"[2],
           anova(lmerModel,lmerModel2)$"Pr(>Chisq)"[2],
           anova(lmerModel,lmerModel3)$"Pr(>Chisq)"[2]))
}
#iterate over random data
#ns - vector of number of participants
#numberOfItems - vector of numbers of each trial (for binomial distribution in glmer)
#reps - number of simulations
#in case of lmer=TRUE, numberOfItems is not meaningful
randSim=function(ns,numberOfItems=c(1),reps=1000,sd=1/0.8,r=0.45){
  numTrials=length(numberOfItems)
  #loop over number of participants
  significantDataFrame=data.frame(numberOfItem=rep(numberOfItems,each=3*length(ns)),
                                  n=rep(ns,each=3*numTrials),
                                  effects=rep(c("sex*nStimuli*type","sex*nStimuli","sex*type"),length(ns)*numTrials),
                                  propSignificant=rep(0,length(ns)*3*numTrials))
  for(numberOfItem in numberOfItems){
    for(n in ns) {
      #to get some sense of progress
      print(n)
      #every combination (2 nStimuli*2 types=4)
      N=n*4
      significant = matrix(nrow=reps, ncol=3)
      for(i in 1:reps){
        testdata=generateData(n,N,numberOfItem,sd,r)
        significant[i,]=getSignificantLmer(testdata)<0.05
      }
      #save proportion to data frame
      significantDataFrame$propSignificant[which(significantDataFrame$numberOfItem==numberOfItem & significantDataFrame$n==n & significantDataFrame$effects=="sex*nStimuli*type")]=
        sum(significant[,1])/reps
      significantDataFrame$propSignificant[which(significantDataFrame$numberOfItem==numberOfItem & significantDataFrame$n==n & significantDataFrame$effects=="sex*nStimuli")]=
        sum(significant[,2])/reps
      significantDataFrame$propSignificant[which(significantDataFrame$numberOfItem==numberOfItem & significantDataFrame$n==n & significantDataFrame$effects=="sex*type")]=
        sum(significant[,3])/reps
    }
  }
  return(significantDataFrame)
}


###script

#load libraries
library(lme4)
library(optimx)
library(ggplot2)

#generate random seed (this should be random enough)
#sample(0:100000,1)
#71996
set.seed(71996)
#show different powers for different numbers of participants
ns=c(1:40)*25
significantDataFrame=randSim(ns,c(24),1000)
significantDataFrame$effects=ifelse(significantDataFrame$effects=="sex*nStimuli","sex*nStimuli (f = 0.2)",
                                     ifelse(significantDataFrame$effects=="sex*type","sex*type (f = 0.4)","sex*nStimuli*type (f = 0.1)"))
#plot by n
ggplot(significantDataFrame,aes(x=n,y=propSignificant,color=effects)) +
  geom_point() + geom_smooth(se=F) + labs(y="simulated power", x="N") +
  scale_x_continuous(breaks = c(1:10)*100) + 
  ylim(0.5,1.0) +
  theme_classic() + theme(legend.position = c(0.8,0.2))
#save graph
ggsave(paste("figs/Exp2SimulPowerN2.png",sep=""))
#save data
write.table(significantDataFrame,file="Exp2SimulPowerN2.csv",sep=";", row.names = FALSE)


##power analysis and effect sizes for anova


#effects on 2x2x2
#log odds
library(Superpower)
#old estimates
mu <- c(1.45,1.45,1.45,1.45,1.195,0.94,1.45,1.365)
mu <- c(1.83,1.58,1.61,1.61,1.52,0.83,1.66,1.47)
sd <- 1.02 #.75/.73 -> large effects for 8 mixed
r <- 0.45


n <- 400

#estimates
mu <- c(0,0,0,0,1/2,1,0,1/6)
#variance and r from glmm
varBetween = 0.860860 
varWithin = 0.822819
sd = sqrt(varBetween+varWithin)/0.85 #/0.85 to normalize to effect 1
r = varWithin/(varBetween+varWithin)
#variance and r from lmm
varBetween = 0.1122^2
varWithin = 0.1270^2
sd = sqrt(varBetween+varWithin)/0.145
r = varWithin/(varBetween+varWithin)

string = "2b*2w*2w"
alpha_level <- 0.05
labelnames = c("sex","m","f","type","mixed","pairwise","nStim","2","8")
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r,
                              labelnames = labelnames,
                              plot=T)
#design_result
exact_result <- ANOVA_exact(design_result,
                            alpha_level = alpha_level,
                            verbose=F)
exact_result$main_results

#rough estimates (not from real data)
n=100
#cohens f of ~0.1
mu <- c(0,0,0,0,1/2,1,0,1/6) #-> n = 400 (per group)
#cohens f of ~0.2
#mu <- c(0,0,0,0,1/3,1,0,0) #-> n = 100 (per group)
sd = 1/0.8
r = 0.55
string = "2b*2w*2w"
alpha_level <- 0.05
labelnames = c("sex","m","f","type","mixed","pairwise","nStim","2","8")
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r,
                              labelnames = labelnames,
                              plot=F)
#design_result
exact_result <- ANOVA_exact(design_result,
                            alpha_level = alpha_level,
                            verbose=F)
exact_result$main_results

#same as between design
sd = sqrt(varBetween)
string = "2b*2b*2b"
design_result <- ANOVA_design(design = string,
                                                 n = n, 
                                                 mu = mu, 
                                                 sd = sd, 
                                                 r = r,
                                                 labelnames = labelnames,
                                                 plot=F)
#design_result
exact_result <- ANOVA_exact(design_result,
                            alpha_level = alpha_level,
                            verbose=F)
exact_result$main_results


#accuracy
library(Superpower)
mu <- c(.862,.829,.833,.833,.821,.697,.841,.813)
n <- 400
sd <- 0.177
r <- 0.44
string = "2b*2w*2w"
alpha_level <- 0.05
labelnames = c("sex","m","f","type","mixed","pairwise","nStim","2","8")
design_result <- ANOVA_design(design = string,
                              n = n, 
                              mu = mu, 
                              sd = sd, 
                              r = r,
                              labelnames = labelnames,
                              plot=F)
#design_result
exact_result <- ANOVA_exact(design_result,
                            alpha_level = alpha_level,
                            verbose=F)
exact_result$main_results

