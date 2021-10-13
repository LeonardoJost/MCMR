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
#sqrt(0.66)*sd - standard deviation between participants
#sqrt(sd2)*sd - standard deviation within participants
#for sd2=0.34 the total standard deviation is sd
generateData=function(n,N,numberOfItem,sd=0.73,sd2=0.34) {
  testdata=data.frame(ids=as.factor(rep(1:n,each=N/n)),
                      sex=factor(rep(c("m","f"),each=N/2)),
                      nStimuli=factor(rep(c(2,8),each=2,N/4)),
                      type=factor(rep(c("type1","type2"),N/2)))
  testdata$baseProb=ifelse(testdata$nStimuli==2 & testdata$type=="type1" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type1" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type2" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type2" & testdata$sex=="m",1.45,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type1" & testdata$sex=="f",1.195,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type1" & testdata$sex=="f",0.94,
                    ifelse(testdata$nStimuli==2 & testdata$type=="type2" & testdata$sex=="f",1.45,
                    ifelse(testdata$nStimuli==8 & testdata$type=="type2" & testdata$sex=="f",1.365,
                    0))))))))
  #convert to numeric to test for main effects as part of interactions
  testdata$sexNumeric=sapply(testdata$sex,function(i) contr.sum(2)[i,])
  testdata$typeNumeric=sapply(testdata$type,function(i) contr.sum(2)[i,])
  testdata$nStimuliNumeric=sapply(testdata$nStimuli,function(i) contr.sum(2)[i,])
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
  #testdata$weights=replicate(nrow(testdata),sum(runif(numberOfItem)>0.3))
  testdata$weights=rep(numberOfItem,N)
  #generate correct responses
  testdata$correctResponses=rbinom(N,size=testdata$weights,prob=testdata$prob)/testdata$weights
  #generate fully correct trials (4 items, 2 consecutively correct items as p^2 because there are no other assumptions)
  testdata$correctResponsesScoringSystem=rbinom(N,size=testdata$weights/2,prob=testdata$prob^2)/testdata$weights/2
  #use p^3 as base probability due to scoring approximation
  testdata$correctResponsesScoringSystem3=rbinom(N,size=testdata$weights/2,prob=testdata$prob^3)/testdata$weights/2
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
#iterate over random data
#ns - vector of number of participants
#numberOfItems - vector of numbers of each trial (for binomial distribution in glmer)
#reps - number of simulations
#in case of lmer=TRUE, numberOfItems is not meaningful
randSim=function(ns,numberOfItems=c(1),reps=1000,sd=0.73,sd2=0.34){
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
        testdata=generateData(n,N,numberOfItem,sd,sd2)
        significant[i,]=getSignificant(testdata)<0.05
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
ns=c(15:30)*10
significantDataFrame=randSim(ns,c(24),1000,0.73,0)
#plot by n
ggplot(significantDataFrame,aes(x=n,y=propSignificant,color=effects)) +
  geom_point() + geom_line() + labs(y="simulated power", x="N") +
  scale_x_continuous(breaks = ns) +
  theme_classic()
ggsave(paste("figs/Exp2SimulPowerN2.png",sep=""))
#save data
write.table(significantDataFrame,file="Exp2SimulPowerN2.csv",sep=";", row.names = FALSE)


##power analysis and effect sizes for anova


#effects on 2x2x2
#log odds
library(Superpower)
mu <- c(1.45,1.45,1.45,1.45,1.195,0.94,1.45,1.365)
mu <- c(1.83,1.58,1.61,1.61,1.52,0.83,1.66,1.47)
n <- 300
sd <- 1.02 #.75/.73 -> large effects for 8 mixed
r <- 0.45
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

#accuracy
library(Superpower)
mu <- c(.862,.829,.833,.833,.821,.697,.841,.813)
n <- 300
sd <- 0.179
r <- 0.45
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

