### Simulation to check effect of random slopes by order of blocks
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
#N - number of measurement categories in total (this is currently always n*12)
#numberOfEachTrial - number of repetitions of each measurement for binomial distribution (note the hard coded outliers)
#standard deviation consists of two parts
#sqrt(0.66)*sd - standard deviation between participants
#sqrt(sd2)*sd - standard deviation within participants
#for sd2=0.34 the total standard deviation is sd
generateData=function(n,N,numberOfEachTrial,sd=0.73,sd2=0.34) {
  testdata=data.frame(ids=as.factor(rep(1:n,each=N/n)),
                      sex=factor(rep(c("m","f"),each=N/2)),
                      nStimuli=factor(rep(c(2,4,8),N/3)),
                      type=factor(rep(c("type1","type2"),N/2)))
  testdata$baseProb=ifelse(testdata$nStimuli==2 & testdata$type=="type1" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==4 & testdata$type=="type1" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type1" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type2" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==4 & testdata$type=="type2" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type2" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type1" & testdata$sex=="f",1.195,
                    ifelse(testdata$nStimuli==4 & testdata$type=="type1" & testdata$sex=="f",0.94,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type1" & testdata$sex=="f",0.685,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type2" & testdata$sex=="f",1.45,
                    ifelse(testdata$nStimuli==4 & testdata$type=="type2" & testdata$sex=="f",1.365,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type2" & testdata$sex=="f",1.28,
                    0))))))))))))
  #convert to numeric to test for main effects as part of interactions
  testdata$sexNumeric=sapply(testdata$sex,function(i) contr.sum(2)[i,])
  testdata$typeNumeric=sapply(testdata$type,function(i) contr.sum(2)[i,])
  nStimuliNumeric=sapply(testdata$nStimuli,function(i) contr.sum(3)[i,])
  testdata$nStimuliNumeric1=nStimuliNumeric[1,]
  testdata$nStimuliNumeric2=nStimuliNumeric[2,]
  #get base probability for each participant, corr coeff of .66
  testdata$baseProbID=rep(rnorm(n),each=N/n)*sqrt(0.66)*sd
  #get probability from log odds
  testdata$logOdds=testdata$baseProb+testdata$baseProbID+rnorm(N)*sqrt(sd2)*sd
  #convert to probability
  testdata$prob=exp(testdata$logOdds)/(1+exp(testdata$logOdds))
  #generate number of correct responses
  #note that the binomial distribution adds additional variance of n*p*(1-p)
  #generate number of attempted tasks
  #not used, because unanswered tasks are already included in accuracy
  #testdata$weights=replicate(nrow(testdata),sum(runif(numberOfEachTrial)>0.3))
  testdata$weights=rep(numberOfEachTrial,N)
  #generate correct responses
  testdata$correctResponses=rbinom(N,size=testdata$weights,prob=testdata$prob)/testdata$weights
  #generate random order of blocks (with no learning overall)
  testdata$block=as.vector(replicate(N/6,sample(6)))
  testdata$blockFactor=as.factor(testdata$block)
  return(testdata)
}

#get p values of effects of interest
getSignificant=function(testdata){
  glmerModel=glmer(correctResponses~sexNumeric*nStimuliNumeric1*typeNumeric+sexNumeric*nStimuliNumeric2*typeNumeric+(1|ids),
                    family=binomial(),data=testdata,weights=weights,
                    control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  glmerModel1=update(glmerModel,formula = ~. -sexNumeric:nStimuliNumeric1:typeNumeric-sexNumeric:nStimuliNumeric2:typeNumeric)
  glmerModel2=update(glmerModel,formula = ~. -sexNumeric:nStimuliNumeric1-sexNumeric:nStimuliNumeric2)
  glmerModel3=update(glmerModel,formula = ~. -sexNumeric:typeNumeric)
  return(c(anova(glmerModel,glmerModel1)$"Pr(>Chisq)"[2],
           anova(glmerModel,glmerModel2)$"Pr(>Chisq)"[2],
           anova(glmerModel,glmerModel3)$"Pr(>Chisq)"[2]))
}
getSignificantWithOrder=function(testdata){
  glmerModel=glmer(correctResponses~sexNumeric*nStimuliNumeric1*typeNumeric+sexNumeric*nStimuliNumeric2*typeNumeric+(block|ids),
                   family=binomial(),data=testdata,weights=weights,
                   control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  glmerModel1=update(glmerModel,formula = ~. -sexNumeric:nStimuliNumeric1:typeNumeric-sexNumeric:nStimuliNumeric2:typeNumeric)
  glmerModel2=update(glmerModel,formula = ~. -sexNumeric:nStimuliNumeric1-sexNumeric:nStimuliNumeric2)
  glmerModel3=update(glmerModel,formula = ~. -sexNumeric:typeNumeric)
  return(c(anova(glmerModel,glmerModel1)$"Pr(>Chisq)"[2],
           anova(glmerModel,glmerModel2)$"Pr(>Chisq)"[2],
           anova(glmerModel,glmerModel3)$"Pr(>Chisq)"[2]))
}
getSignificantWithOrderFactor=function(testdata){
  glmerModel=glmer(correctResponses~sexNumeric*nStimuliNumeric1*typeNumeric+sexNumeric*nStimuliNumeric2*typeNumeric+(blockFactor|ids),
                   family=binomial(),data=testdata,weights=weights,
                   control = glmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  glmerModel1=update(glmerModel,formula = ~. -sexNumeric:nStimuliNumeric1:typeNumeric-sexNumeric:nStimuliNumeric2:typeNumeric)
  glmerModel2=update(glmerModel,formula = ~. -sexNumeric:nStimuliNumeric1-sexNumeric:nStimuliNumeric2)
  glmerModel3=update(glmerModel,formula = ~. -sexNumeric:typeNumeric)
  return(c(anova(glmerModel,glmerModel1)$"Pr(>Chisq)"[2],
           anova(glmerModel,glmerModel2)$"Pr(>Chisq)"[2],
           anova(glmerModel,glmerModel3)$"Pr(>Chisq)"[2]))
}
#iterate over random data
#ns - vector of number of participants
#linear - use of lmer or glmer
#numberOfEachTrials - vector of numbers of each trial (for binomial distribution in glmer)
#reps - number of simulations
#in case of lmer=TRUE, numberOfEachTrials is not meaningful
randSim=function(ns,numberOfEachTrials=c(1),reps=1000,sd=0.73,sd2=0.34){
  numTrials=length(numberOfEachTrials)
  #loop over number of participants
  significantDataFrame=data.frame(numberOfEachTrial=rep(numberOfEachTrials,each=3*length(ns)),
                                  n=rep(ns,each=3*numTrials),
                                  effects=rep(c("sex*nStimuli*type","sex*nStimuli","sex*type"),length(ns)*numTrials),
                                  propSignificant=rep(0,length(ns)*3*numTrials))
  for(numberOfEachTrial in numberOfEachTrials){
    for(n in ns) {
      #to get some sense of progress
      print(n)
      #every combination (3 nStimuli*2 types=6) twice
      N=n*6
      significant = matrix(nrow=reps, ncol=9)
      #repeatedly simulate data and get results
      for(i in 1:reps){
        testdata=generateData(n,N,numberOfEachTrial,sd,sd2)
        significant[i,1:3]=getSignificant(testdata)<0.05
        significant[i,4:6]=getSignificantWithOrder(testdata)<0.05
        significant[i,7:9]=getSignificantWithOrderFactor(testdata)<0.05
      }

      #save proportion to data frame
      significantDataFrame$propSignificant[which(significantDataFrame$numberOfEachTrial==numberOfEachTrial & significantDataFrame$n==n & significantDataFrame$effects=="sex*nStimuli*type")]=
        sum(significant[,1])/reps
      significantDataFrame$propSignificant[which(significantDataFrame$numberOfEachTrial==numberOfEachTrial & significantDataFrame$n==n & significantDataFrame$effects=="sex*nStimuli")]=
        sum(significant[,2])/reps
      significantDataFrame$propSignificant[which(significantDataFrame$numberOfEachTrial==numberOfEachTrial & significantDataFrame$n==n & significantDataFrame$effects=="sex*type")]=
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
sample(0:100000,1)
#42849
set.seed(42849)

#get power for simulated binomial distributions
numberOfEachTrials=c(24)
significantDataFrame=randSim(ns=c(100),F,numberOfEachTrials,1000,0.73,0)

#plot by number of trials
ggplot(significantDataFrame,aes(x=numberOfEachTrial,y=propSignificant,color=effects)) +
  geom_point() + geom_line() + labs(y="simulated power", x="trials per test") +
  scale_x_continuous(breaks = numberOfEachTrials) +
  theme_classic()
ggsave(paste("figs/SimulPowerTrials.png",sep=""))
#save data
write.table(significantDataFrame,file="simulPowerOrder.csv",sep=";", row.names = FALSE)

testdata=generateData(100,600,24,0.73,0)
